open Core.Std
open Async.Std

module Net = Async_NetKAT.Net

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module SDN = SDN_Types
module NetKAT = NetKAT_Types

type switchId = SDN_Types.switchId

module SwitchMap = Map.Make(Int64)

module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let _ = Log.set_level `Debug

let _ = Log.set_output
          [Log.make_colored_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization");
              ("openflow", "controller");
              ("netkat", "topology.switch")]]

let tags = [("openflow", "controller")]

exception Assertion_failed of string

type t = {
  ctl : Controller.t;
  nib : Net.Topology.t ref;
  mutable locals : LocalCompiler.t SwitchMap.t
}

let bytes_to_headers port_id (bytes : Cstruct.t) =
  let open NetKAT.Headers in
  let open Packet in
  let pkt = Packet.parse bytes in
  { location = Some(NetKAT.Physical port_id)
  ; ethSrc = Some(pkt.dlSrc)
  ; ethDst = Some(pkt.dlDst)
  ; vlan = pkt.dlVlan
  ; vlanPcp = Some(pkt.dlVlanPcp)
  ; ethType = Some(dlTyp pkt)
  ; ipProto = (try Some(nwProto pkt) with Invalid_argument(_) -> None)
  ; ipSrc = (try Some((nwSrc pkt, 0)) with Invalid_argument(_) -> None)
  ; ipDst = (try Some((nwDst pkt, 0)) with Invalid_argument(_) -> None)
  ; tcpSrcPort = (try Some(tpSrc pkt) with Invalid_argument(_) -> None)
  ; tcpDstPort = (try Some(tpDst pkt) with Invalid_argument(_) -> None)
  }

let headers_to_actions port (h:NetKAT_Types.Headers.t) : SDN_Types.action list =
  let open SDN_Types in
  let g p acc f =
    match Field.get f h with
      | Some v -> (p v)::acc
      | None -> acc in
  let init = match h.NetKAT_Types.Headers.location with
     | Some(NetKAT_Types.Pipe     p) ->
       raise (Assertion_failed (Printf.sprintf
         "Controller.headers_to_action: impossible pipe location \"%s\"" p))
     | Some(NetKAT_Types.Physical p) -> [OutputPort(VInt.Int32 p)]
     | None -> [OutputPort (VInt.Int32 (Int32.of_int_exn port))] in
  NetKAT_Types.Headers.Fields.fold
    ~init
    ~location:(fun acc f -> acc)
    ~ethSrc:(g (fun v -> SetField(EthSrc, VInt.Int48 v)))
    ~ethDst:(g (fun v -> SetField(EthDst, VInt.Int48 v)))
    ~vlan:(g (fun v -> SetField(Vlan, VInt.Int16 v)))
    ~vlanPcp:(g (fun v -> SetField(VlanPcp, VInt.Int8 v)))
    ~ethType:(g (fun v -> SetField(EthType, VInt.Int16 v)))
    ~ipProto:(g (fun v -> SetField(IPProto, VInt.Int8 v)))
    ~ipSrc:(g (fun (v, _) -> SetField(IP4Src, VInt.Int32 v)))
    ~ipDst:(g (fun (v, _) -> SetField(IP4Dst, VInt.Int32 v)))
    ~tcpSrcPort:(g (fun v -> SetField(TCPSrcPort, VInt.Int16 v)))
    ~tcpDstPort:(g (fun v -> SetField(TCPDstPort, VInt.Int16 v)))

exception Unsupported_mod of string

let payload_bytes p = match p with
  | SDN_Types.NotBuffered(bytes)
  | SDN_Types.Buffered(_, bytes) -> bytes

let packet_sync_headers (pkt:NetKAT_Types.packet) : NetKAT_Types.packet * bool =
  let open NetKAT_Types in
  let change = ref false in
  let g p q acc f =
    match Field.get f pkt.headers with
      | Some v -> if p v acc then
          acc
        else begin
          change := true;
          q acc v
        end
      | None -> acc in
  let fail field = (fun _ -> raise (Unsupported_mod field)) in
  let packet = Packet.parse (payload_bytes pkt.payload) in
  let packet' = Headers.Fields.fold
    ~init:packet
    ~location:(fun acc _ -> acc)
    ~ethSrc:(g (fun v p -> v = p.Packet.dlSrc) Packet.setDlSrc)
    ~ethDst:(g (fun v  p -> v = p.Packet.dlDst) Packet.setDlDst)
    (* XXX(seliopou): Fix impls of: vlan, vlanPcp *)
    ~vlan:(g (fail "vlan") (fail "vlan"))
    ~vlanPcp:(g (fun _ _ -> true) (fail "vlanPcp"))
    ~ipSrc:(g (fun (v, _) p -> v = Packet.nwSrc p) (fun acc (nw, _) -> Packet.setNwSrc acc nw))
    ~ipDst:(g (fun (v, _) p -> v = Packet.nwDst p) (fun acc (nw, _) -> Packet.setNwDst acc nw))
    ~tcpSrcPort:(g (fun v p -> v = Packet.tpSrc p) Packet.setTpSrc)
    ~tcpDstPort:(g (fun v p -> v = Packet.tpDst p) Packet.setTpDst)
    (* XXX(seliopou): currently does not support: *)
    ~ethType:(g (fun _ _ -> true) (fail "ethType"))
    ~ipProto:(g (fun _ _ -> true) (fail "ipProto")) in
  ({ pkt with payload = match pkt.payload with
    | SDN_Types.NotBuffered(_) -> SDN_Types.NotBuffered(Packet.marshal packet')
    | SDN_Types.Buffered(n, _) -> SDN_Types.Buffered(n, Packet.marshal packet')
  }, !change)

let packet_out_to_message (_, bytes, buffer_id, port_id, actions) =
  let open OpenFlow0x01_Core in
  let output_payload = match buffer_id with
    | Some(id) -> Buffered(id, bytes)
    | None -> NotBuffered bytes in
  let port_id = match port_id with
    | Some(vi) -> Some(VInt.get_int vi)
    | None -> None in
  let apply_actions = SDN_OpenFlow0x01.from_group port_id [[actions]] in
  OpenFlow0x01.Message.PacketOutMsg{ output_payload; port_id; apply_actions }

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

let to_event (t : t) evt =
  let open NetKAT_Types in
  match evt with
    | `Connect (c_id, feats) ->
      let ports = feats.OpenFlow0x01.SwitchFeatures.ports in
      let sw_id = feats.OpenFlow0x01.SwitchFeatures.switch_id in
      (* Generate a SwitchUp event, and PortUp event for ever port that is
       * useable. *)
      return ((SwitchUp sw_id) :: (List.fold ports ~init:[] ~f:(fun acc pd ->
        if port_desc_useable pd then
          let open OpenFlow0x01.PortDescription in
          let pt_id = VInt.Int32 (Int32.of_int_exn pd.port_no) in
          PortUp(sw_id, pt_id)::acc
        else
          acc)))
    | `Disconnect (c_id, switch_id, exn) ->
      return [SwitchDown switch_id]
    | `Message (c_id, (xid, msg)) ->
      let open OpenFlow0x01.Message in
      let switch_id = Controller.switch_id_of_client t.ctl c_id in
      begin match msg with
        | PacketInMsg pi ->
          let open OpenFlow0x01_Core in
          let port_id = VInt.Int16 pi.port in
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
                let pis, phys = LocalCompiler.eval local packet in
                let outs = Deferred.List.map phys ~f:(fun packet1 ->
                  let acts = headers_to_actions pi.port
                    (Headers.diff packet1.headers packet.headers) in
                  let po = (switch_id, bytes, buf_id, Some(port_id), acts) in
                  send t.ctl c_id (0l, packet_out_to_message po)) in
                Deferred.ignore outs >>= fun _ ->
                return (List.map pis ~f:(fun (p, pkt) ->
                  let pkt', changed = packet_sync_headers pkt in
                  if changed then
                    let bytes = payload_bytes pkt'.payload in
                    PacketIn(p, switch_id, port_id, bytes, pi.total_len, None)
                  else
                    PacketIn(p, switch_id, port_id, bytes, pi.total_len, buf_id)))
              end
          end
        | PortStatusMsg ps ->
          let open OpenFlow0x01.PortStatus in
          begin match ps.reason, port_desc_useable ps.desc with
            | ChangeReason.Add, true
            | ChangeReason.Modify, true ->
              let pt_id = Int32.of_int_exn (ps.desc.OpenFlow0x01.PortDescription.port_no) in
              return [PortUp(switch_id, VInt.Int32 pt_id)]
            | ChangeReason.Delete, _
            | ChangeReason.Modify, false ->
              let pt_id = Int32.of_int_exn (ps.desc.OpenFlow0x01.PortDescription.port_no) in
              return [PortDown(switch_id, VInt.Int32 pt_id)]
            | _ ->
              return []
          end
        | _ ->
          Log.debug ~tags "Dropped message from %Lu: %s" switch_id (to_string msg);
          return []
      end

let update_table_for (t : t) (sw_id : switchId) pol =
  let delete_flows =
    OpenFlow0x01.Message.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  let local = LocalCompiler.of_policy sw_id pol in
  t.locals <- SwitchMap.add t.locals sw_id local;
  send t.ctl c_id (5l, delete_flows) >>= fun _ ->
  let priority = ref 65536 in
  let table = LocalCompiler.to_table local in
  if List.length table <= 0
    then raise (Assertion_failed (Printf.sprintf
        "Controller.update_table_for: empty table for switch %Lu" sw_id));
  Deferred.List.iter table ~f:(fun flow ->
    decr priority;
    send t.ctl c_id (0l, to_flow_mod !priority flow))

let get_switchids nib =
  Net.Topology.fold_vertexes (fun v acc -> match Net.Topology.vertex_to_label nib v with
    | Async_NetKAT.Switch id -> id::acc
    | _ -> acc)
  nib []

let handler (t : t) w app =
  let app' = Async_NetKAT.run app in
  fun e ->
    let open Deferred in
    app' t.nib w e >>= fun m_pol ->
    match m_pol with
      | Some (pol) ->
        List.iter (get_switchids !(t.nib)) ~f:(fun sw_id ->
          update_table_for t sw_id pol)
      | None ->
        let open NetKAT_Types in
        begin match e with
          | SwitchUp sw_id ->
            update_table_for t sw_id (Async_NetKAT.default app)
          | _ -> return ()
        end

let start app ?(port=6633) () =
  let open Async_OpenFlow.Platform.Trans in
  let stages = let open Controller in
    (local (fun t -> t.ctl)
      features)
     >=> to_event in

  Controller.create ~max_pending_connections ~port ()
  >>> fun t ->
    let t' = {
      ctl = t;
      nib = ref (Net.Topology.empty ());
      locals = SwitchMap.empty
    } in
    let events = run stages t' (Controller.listen t) in

    (* The pipe for packet_outs. The Pipe.iter below will run in its own logical
     * thread, sending packet outs to the switch whenever it's scheduled.
     * *)
    let r_out, w_out = Pipe.create () in
    Deferred.don't_wait_for (Pipe.iter r_out ~f:(fun out ->
      let (sw_id, _, _, _, _) = out in
      let c_id = Controller.client_id_of_switch t sw_id in
      send t c_id (0l, packet_out_to_message out)));

    Deferred.don't_wait_for (Pipe.iter events ~f:(handler t' w_out app))
