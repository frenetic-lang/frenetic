open Core.Std
open Async.Std

module Net = Async_NetKAT.Net

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module SDN = SDN_Types

type switchId = SDN_Types.switchId

module SwitchMap = Map.Make(Int64)

module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let _ = Log.set_level `Info

let _ = Log.set_output
          [Log.make_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization");
              ("openflow", "controller");
              ("openflow", "openflow0x01");
              ("netkat", "topology.switch");
              ("netkat", "topology.host");
              ("netkat", "learning")]]

let tags = [("openflow", "controller")]

exception Assertion_failed of string

type t = {
  ctl : Controller.t;
  nib : Net.Topology.t ref;
  mutable locals : NetKAT_Types.policy SwitchMap.t
}

let bytes_to_headers port_id (bytes : Cstruct.t) =
  let open NetKAT_Types.HeadersValues in
  let open Packet in
  let pkt = Packet.parse bytes in
  { location = NetKAT_Types.Physical port_id
  ; ethSrc = pkt.dlSrc
  ; ethDst = pkt.dlDst
  ; vlan = (match pkt.dlVlan with Some (v) -> v | None -> 0)
  ; vlanPcp = pkt.dlVlanPcp
  ; ethType = dlTyp pkt
  ; ipProto = (try nwProto pkt with Invalid_argument(_) -> 0)
  ; ipSrc = (try nwSrc pkt with Invalid_argument(_) -> 0l)
  ; ipDst = (try nwDst pkt with Invalid_argument(_) -> 0l)
  ; tcpSrcPort = (try tpSrc pkt with Invalid_argument(_) -> 0)
  ; tcpDstPort = (try tpDst pkt with Invalid_argument(_) -> 0)
  }


let headers_to_actions
  (h_new:NetKAT_Types.HeadersValues.t)
  (h_old:NetKAT_Types.HeadersValues.t)
  : SDN_Types.action list =
  let open SDN_Types in
  let g p acc f =
    if (Field.get f h_new) = (Field.get f h_old)
      then acc
      else (p (Field.get f h_new))::acc in
  let init = match h_new.NetKAT_Types.HeadersValues.location with
     | NetKAT_Types.Pipe p ->
       raise (Assertion_failed (Printf.sprintf
         "Controller.headers_to_action: impossible pipe location \"%s\"" p))
     | NetKAT_Types.Physical p -> [Output(Physical(p))] in
  NetKAT_Types.HeadersValues.Fields.fold
    ~init
    ~location:(fun acc f -> acc)
    ~ethSrc:(g (fun v -> Modify(SetEthSrc v)))
    ~ethDst:(g (fun v -> Modify(SetEthDst v)))
    ~vlan:(g (fun v -> Modify(SetVlan (Some(v)))))
    ~vlanPcp:(g (fun v -> Modify(SetVlanPcp v)))
    ~ethType:(g (fun v -> Modify(SetEthTyp v)))
    ~ipProto:(g (fun v -> Modify(SetIPProto v)))
    ~ipSrc:(g (fun v -> Modify(SetIP4Src v)))
    ~ipDst:(g (fun v -> Modify(SetIP4Dst v)))
    ~tcpSrcPort:(g (fun v -> Modify(SetTCPSrcPort v)))
    ~tcpDstPort:(g (fun v -> Modify(SetTCPDstPort v)))

exception Unsupported_mod of string

let payload_bytes p = match p with
  | SDN_Types.NotBuffered(bytes)
  | SDN_Types.Buffered(_, bytes) -> bytes

let packet_sync_headers (pkt:NetKAT_Types.packet) : NetKAT_Types.packet * bool =
  let open NetKAT_Types in
  let change = ref false in
  let g p q acc f =
    let v = Field.get f pkt.headers in
    if p v acc then
      acc
    else begin
      change := true;
      q acc v
    end in
  let fail field = (fun _ -> raise (Unsupported_mod field)) in
  let packet = Packet.parse (SDN_Types.payload_bytes pkt.payload) in
  let packet' = HeadersValues.Fields.fold
    ~init:packet
    ~location:(fun acc _ -> acc)
    ~ethSrc:(g (fun v p -> v = p.Packet.dlSrc) Packet.setDlSrc)
    ~ethDst:(g (fun v p -> v = p.Packet.dlDst) Packet.setDlDst)
    (* XXX(seliopou): Fix impls of: vlan, vlanPcp *)
    ~vlan:(g (fun _ _ -> true) (fail "vlan"))
    ~vlanPcp:(g (fun _ _ -> true) (fail "vlanPcp"))
    ~ipSrc:(g
      (fun v p -> try v = Packet.nwSrc p with Invalid_argument(_) -> true)
      (fun acc nw -> Packet.setNwSrc acc nw))
    ~ipDst:(g
      (fun v p -> try v = Packet.nwDst p with Invalid_argument(_) -> true)
      (fun acc nw -> Packet.setNwDst acc nw))
    ~tcpSrcPort:(g
      (fun v p -> try v= Packet.tpSrc p with Invalid_argument(_) -> true)
      Packet.setTpSrc)
    ~tcpDstPort:(g
      (fun v p -> try v = Packet.tpDst p with Invalid_argument(_) -> true)
      Packet.setTpDst)
    (* XXX(seliopou): currently does not support: *)
    ~ethType:(g (fun _ _ -> true) (fail "ethType"))
    ~ipProto:(g (fun _ _ -> true) (fail "ipProto")) in
  ({ pkt with payload = match pkt.payload with
    | SDN_Types.NotBuffered(_) -> SDN_Types.NotBuffered(Packet.marshal packet')
    | SDN_Types.Buffered(n, _) -> SDN_Types.Buffered(n, Packet.marshal packet')
  }, !change)

let send t c_id msg =
  Controller.send t c_id msg
  >>| function
    | `Sent _ -> ()
    | `Drop exn -> raise exn

let port_desc_useable pd =
  let open OpenFlow0x01.PortDescription in
  if pd.config.PortConfig.down
    then false
    else not (pd.state.PortState.down)

let to_event w_out (t : t) evt =
  let open NetKAT_Types in
  match evt with
    | `Connect (c_id, feats) ->
      let ports = feats.OpenFlow0x01.SwitchFeatures.ports in
      let sw_id = feats.OpenFlow0x01.SwitchFeatures.switch_id in
      (* Generate a SwitchUp event, and PortUp event for ever port that is
       * useable. *)
      return ((SwitchUp sw_id) :: (List.fold ports ~init:[] ~f:(fun acc pd ->
        let open OpenFlow0x01.PortDescription in
        if port_desc_useable pd && pd.port_no < 0xff00 then
          let pt_id = Int32.of_int_exn pd.port_no in
          PortUp(sw_id, pt_id)::acc
        else
          acc)))
    | `Disconnect (c_id, switch_id, exn) ->
      let open Net.Topology in
      let v  = vertex_of_label !(t.nib) (Async_NetKAT.Switch switch_id) in
      let ps = vertex_to_ports !(t.nib) v in
      return (PortSet.fold (fun p acc -> (PortDown(switch_id, p))::acc)
        ps [SwitchDown switch_id])
    | `Message (c_id, (xid, msg)) ->
      let open OpenFlow0x01.Message in
      let switch_id = Controller.switch_id_of_client t.ctl c_id in
      begin match msg with
        | PacketInMsg pi ->
          let open OpenFlow0x01_Core in
          let port_id = Int32.of_int_exn pi.port in
          let buf_id, bytes = match pi.input_payload with
            | Buffered(n, bs) -> Some(n), bs
            | NotBuffered(bs) -> None, bs in
          begin match SwitchMap.find t.locals switch_id with
            | None ->
              (* The switch may be connected but has yet had rules installed on
               * it. In that case, just drop the packet.
               * *)
              return []
            | Some local ->
              (* Eval the packet to get the list of packets that should go to
               * pipes, and the list of packets that can be forwarded to physical
               * locations.
               * *)
              let packet = {
                switch = switch_id;
                headers = bytes_to_headers (Int32.of_int_exn pi.port) bytes;
                payload = SDN_OpenFlow0x01.to_payload pi.input_payload
              } in
              begin
                (* XXX(seliopou): What if the packet's modified? Should buf_id be
                 * exposed to the application?
                 * *)
                let pis, phys = NetKAT_Semantics.eval_pipes packet local in
                let outs = Deferred.List.iter phys ~f:(fun packet1 ->
                  let acts = headers_to_actions
                    packet1.headers packet.headers in
                  let payload = match buf_id with
                    | None -> SDN_Types.NotBuffered(bytes)
                    | Some(buf_id) -> SDN_Types.Buffered(buf_id, bytes) in
                  let out = (switch_id, (payload, Some(port_id), acts)) in
                  Pipe.write w_out out) in
                outs >>= fun _ ->
                return (List.map pis ~f:(fun (p, pkt) ->
                  let pkt', changed = packet_sync_headers pkt in
                  let payload = match buf_id, changed with
                      | None, _
                      | _   , true ->
                        SDN_Types.NotBuffered(payload_bytes pkt'.payload)
                      | Some(buf_id), false ->
                        SDN_Types.Buffered(buf_id, bytes)
                  in
                  PacketIn(p, switch_id, port_id, payload, pi.total_len)))
              end
          end
        | PortStatusMsg ps ->
          let open OpenFlow0x01.PortStatus in
          begin match ps.reason, port_desc_useable ps.desc with
            | ChangeReason.Add, true
            | ChangeReason.Modify, true ->
              let pt_id = Int32.of_int_exn (ps.desc.OpenFlow0x01.PortDescription.port_no) in
              return [PortUp(switch_id, pt_id)]
            | ChangeReason.Delete, _
            | ChangeReason.Modify, false ->
              let pt_id = Int32.of_int_exn (ps.desc.OpenFlow0x01.PortDescription.port_no) in
              return [PortDown(switch_id, pt_id)]
            | _ ->
              return []
          end
        | _ ->
          Log.debug ~tags "Dropped message from %Lu: %s" switch_id (to_string msg);
          return []
      end

let update_table_for (t : t) (sw_id : switchId) pol : unit Deferred.t =
  let delete_flows =
    OpenFlow0x01.Message.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  t.locals <- SwitchMap.add t.locals sw_id
    (Optimize.specialize_policy sw_id pol);
  let local = NetKAT_LocalCompiler.compile sw_id pol in
  Monitor.try_with ~name:"update_table_for" (fun () ->
    send t.ctl c_id (5l, delete_flows) >>= fun _ ->
    let priority = ref 65536 in
    let table = NetKAT_LocalCompiler.to_table local in
    if List.length table <= 0
      then raise (Assertion_failed (Printf.sprintf
          "Controller.update_table_for: empty table for switch %Lu" sw_id));
    Deferred.List.iter table ~f:(fun flow ->
      decr priority;
      send t.ctl c_id (0l, to_flow_mod !priority flow)))
  >>= function
    | Ok () -> return ()
    | Error exn_ ->
      Log.error ~tags
        "switch %Lu: Failed to update table" sw_id;
      Log.flushed ()

let get_switchids nib =
  Net.Topology.fold_vertexes (fun v acc -> match Net.Topology.vertex_to_label nib v with
    | Async_NetKAT.Switch id -> id::acc
    | _ -> acc)
  nib []

let handler (t : t) w app =
  let app' = Async_NetKAT.run app t.nib w () in
  fun e ->
    app' e >>= fun m_pol ->
    match m_pol with
      | Some (pol) ->
        Deferred.List.iter (get_switchids !(t.nib)) ~f:(fun sw_id ->
          update_table_for t sw_id pol)
      | None ->
        begin match e with
          | NetKAT_Types.SwitchUp sw_id ->
            update_table_for t sw_id (Async_NetKAT.default app)
          | _ -> return ()
        end

let start app ?(port=6633) () =
  let open Async_OpenFlow.Stage in
  Controller.create ~max_pending_connections ~port ()
  >>> fun ctl ->
    let t = {
      ctl = ctl;
      nib = ref (Net.Topology.empty ());
      locals = SwitchMap.empty
    } in

    (* The pipe for packet_outs. The Pipe.iter below will run in its own logical
     * thread, sending packet outs to the switch whenever it's scheduled.
     * *)
    let r_out, w_out = Pipe.create () in
    Deferred.don't_wait_for (Pipe.iter r_out ~f:(fun out ->
      let (sw_id, pkt_out) = out in
      Monitor.try_with ~name:"packet_out" (fun () ->
        let c_id = Controller.client_id_of_switch ctl sw_id in
        send ctl c_id (0l, OpenFlow0x01.Message.PacketOutMsg
          (SDN_OpenFlow0x01.from_packetOut pkt_out)))
      >>= function
        | Ok () -> return ()
        | Error exn_ ->
          Log.error ~tags "switch %Lu: Failed to send packet_out" sw_id;
          Log.flushed ()));

    let stages = let open Controller in
      (local (fun t -> t.ctl)
        features)
       >=> (to_event w_out) in

    (* Build up the application by adding topology discovery into the mix. *)
    let d_ctl, topo = Discovery.create () in
    let app = Async_NetKAT.union ~how:`Sequential topo (Discovery.guard app) in
    let sdn_events = run stages t (Controller.listen ctl) in
    (* The discovery application itself will generate events, so the actual
     * event stream must be a combination of switch events and synthetic
     * topology discovery events. Pipe.interleave will wait until one of the
     * pipes is readable, take a batch, and send it along.
     *
     * Whatever happens, happens. Can't stop won't stop.
     * *)
    let events = Pipe.interleave [Discovery.events d_ctl; sdn_events] in

    Deferred.don't_wait_for (Pipe.iter events ~f:(handler t w_out app))
