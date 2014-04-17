open Core.Std
open Async.Std

module Net = Async_NetKAT.Net

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module Stage = Async_OpenFlow.Stage
module SDN = SDN_Types

type switchId = SDN_Types.switchId

module SwitchMap = Map.Make(Int64)
module XidMap = Map.Make(Int32)

module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let _ = Log.set_level `Debug

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
  mutable locals : NetKAT_Types.policy SwitchMap.t;
  mutable barriers : (unit Ivar.t) XidMap.t
}

let bytes_to_headers
  (port_id : SDN_Types.portId)
  (bytes : Cstruct.t)
  : NetKAT_Types.HeadersValues.t =
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

let port_desc_useable (pd : OpenFlow0x01.PortDescription.t) : bool =
  let open OpenFlow0x01.PortDescription in
  if pd.config.PortConfig.down
    then false
    else not (pd.state.PortState.down)

let to_event (w_out : (switchId * SDN_Types.pktOut) Pipe.Writer.t)
  : (t, Controller.f, NetKAT_Types.event) Stage.t =
  let open NetKAT_Types in
  fun t evt -> match evt with
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
      Log.debug ~tags "switch %Ld disconnected" switch_id;
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
          let payload = SDN_OpenFlow0x01.to_payload pi.input_payload in
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
              let pkt0 = {
                switch = switch_id;
                headers = bytes_to_headers port_id (SDN_Types.payload_bytes payload);
                payload = payload;
              } in
              begin
                (* XXX(seliopou): What if the packet's modified? Should buf_id be
                 * exposed to the application?
                 * *)
                let pis, phys = NetKAT_Semantics.eval_pipes pkt0 local in
                let outs = Deferred.List.iter phys ~f:(fun pkt1 ->
                  let acts = headers_to_actions pkt1.headers pkt0.headers in
                  let out  = (switch_id, (payload, Some(port_id), acts)) in
                  Pipe.write w_out out) in
                outs >>= fun _ ->
                return (List.map pis ~f:(fun (pipe, pkt2) ->
                  let pkt3, changed = packet_sync_headers pkt2 in
                  let payload = match payload, changed with
                      | SDN_Types.NotBuffered(_), _
                      | _                       , true ->
                        SDN_Types.NotBuffered(SDN_Types.payload_bytes pkt3.payload)
                      | SDN_Types.Buffered(buf_id, bytes), false ->
                        SDN_Types.Buffered(buf_id, bytes)
                  in
                  PacketIn(pipe, switch_id, port_id, payload, pi.total_len)))
              end
          end
        | PortStatusMsg ps ->
          let open OpenFlow0x01.PortStatus in
          let open OpenFlow0x01.PortDescription in
          begin match ps.reason, port_desc_useable ps.desc with
            | ChangeReason.Add, true
            | ChangeReason.Modify, true ->
              let pt_id = Int32.of_int_exn (ps.desc.port_no) in
              return [PortUp(switch_id, pt_id)]
            | ChangeReason.Delete, _
            | ChangeReason.Modify, false ->
              let pt_id = Int32.of_int_exn (ps.desc.port_no) in
              return [PortDown(switch_id, pt_id)]
            | _ ->
              return []
          end
        | BarrierReply ->
          Log.debug ~tags "Received barrier_reply %Lu" switch_id;
          Log.flushed ();          
          begin match XidMap.find t.barriers xid with
            | None -> Log.error ~tags "to_event: received unexpected BarrierReply from %Lu" switch_id;
              Log.flushed ();
              ()
            | Some ivar -> Ivar.fill ivar ()
          end;
          return []
        | _ ->
          Log.debug ~tags "Dropped message from %Lu: %s" switch_id (to_string msg);
          return []
      end

let get_switchids nib =
  Net.Topology.fold_vertexes (fun v acc -> match Net.Topology.vertex_to_label nib v with
    | Async_NetKAT.Switch id -> id::acc
    | _ -> acc)
  nib []

let _xid = ref 0l

let next_xid () =
  _xid := Int32.succ (!_xid);
  !_xid
  
let send_barrier_to_sw (t : t) (sw_id : switchId) : unit Deferred.t =
  let ivar = Ivar.create () in
  let xid = next_xid () in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  t.barriers <- XidMap.add t.barriers xid ivar;
  send t.ctl c_id (xid, OpenFlow0x01.Message.BarrierRequest);
  Ivar.read ivar

let send_barrier_to_sw_with_timeout (t : t) (sw_id : switchId) : unit Deferred.t =
  with_timeout (Time.Span.of_sec 15.0) (send_barrier_to_sw t sw_id)
  >>= function
  | `Result () -> return ()
  | `Timeout ->
    Log.error ~tags
      "Async_NetKAT_Controller.send_barrier_to_sw_with_timeout: switch %Lu timed out" sw_id;
    Log.flushed ()

let internal_update_table_for (t : t) ver pol (sw_id : switchId) : unit Deferred.t =
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  t.locals <- SwitchMap.add t.locals sw_id
    (Optimize.specialize_policy sw_id pol);
  let local = NetKAT_LocalCompiler.compile sw_id pol in
  Monitor.try_with ~name:"internal_update_table_for" (fun _ ->
    let priority = ref 65536 in
    (* Add match on ver *)    
    let table = List.map (NetKAT_LocalCompiler.to_table local) ~f:(fun flow ->
      {flow with pattern = { flow.pattern with dlVlan = Some ver }}) in
    Log.debug ~tags
      "switch %Lu: Installing internal table %s" sw_id (SDN_Types.string_of_flowTable table);
    let open SDN_Types in
    if List.length table <= 0
      then raise (Assertion_failed (Printf.sprintf
          "Controller.internal_update_table_for: empty table for switch %Lu" sw_id));
      Deferred.List.iter table ~f:(fun flow ->
          decr priority;
          send t.ctl c_id (0l, to_flow_mod !priority flow))
    >>= fun () -> send_barrier_to_sw_with_timeout t sw_id)
  >>= function
  | Ok () ->
    Log.debug ~tags
      "switch %Lu: installed internal table for ver %d" sw_id ver;
    Log.flushed ()
  | Error exn_ ->
    Log.error ~tags
      "switch %Lu: Failed to update table in internal_update_table_for" sw_id;
    Log.flushed ()

(* Topology detection doesn't really detect hosts. So, I treat any port not connected to a known switch as an edge port *)
let get_edge_ports (t : t) (sw_id : switchId) =
  let open Async_NetKAT in
  let open Net.Topology in
  let topo = !(t.nib) in
  let sw = vertex_of_label topo (Switch sw_id) in
  PortSet.fold (fun pt acc ->
      match next_hop topo sw pt with
      | Some e -> let (node, _) = edge_dst e in
        begin match vertex_to_label topo node with
          | Host _ -> PortSet.add pt acc
          | _ -> acc
        end
      | _ -> PortSet.add pt acc) (vertex_to_ports topo sw) PortSet.empty

let get_internal_ports (t : t) (sw_id : switchId) =
  let open Async_NetKAT in
  let open Net.Topology in
  let topo = !(t.nib) in
  Log.debug ~tags "topo: %s" (Net.Pretty.to_string topo);
  let sw = vertex_of_label topo (Switch sw_id) in
  PortSet.fold (fun pt acc ->
      match next_hop topo sw pt with
      | Some e -> let (node, _) = edge_dst e in
        begin match vertex_to_label topo node with
          | Switch _ -> PortSet.add pt acc
          | _ -> acc
        end
      | _ -> acc) (vertex_to_ports topo sw) PortSet.empty


let compute_edge_table (t : t) ver table sw_id =
  let internal_ports = get_internal_ports t sw_id in
  let vlan_none = 65535 in
  (* Fold twice: once to fix match, second to fix fwd *)
  let open SDN_Types in
  let open Async_NetKAT.Net.Topology in
  let rec fix_actions = function
    | Output (Physical pt) :: acts ->
      if not (PortSet.mem pt internal_ports)
      then
        (Modify (SetVlan None)) :: (Output (Physical pt)) :: (fix_actions acts)
      else
        (Modify (SetVlan (Some ver))) :: (Modify (SetVlanPcp 1)) :: (Output (Physical pt)) :: (fix_actions acts)
    | Output (Controller n) :: acts ->
      (Modify (SetVlan None)) :: (Output (Controller n)) :: (fix_actions acts)
    | Output _ :: acts ->
      raise (Assertion_failed "Controller.compute_edge_table: Port not supported by consistent updates")
    | act :: acts ->
      act :: (fix_actions acts)
    | [] -> []
  in
  let match_table = List.fold table ~init:[] ~f:(fun acc r ->
      begin
        match r.pattern.inPort with
        | Some pt ->
          if PortSet.mem pt internal_ports
          then acc
          else {r with pattern = {r.pattern with dlVlan = Some vlan_none}} :: acc
        | None ->
          {r with pattern = {r.pattern with dlVlan = Some vlan_none}} :: acc
      end)
  in
  List.fold match_table ~init:[] ~f:(fun acc r ->
      {r with action = List.map r.action ~f:(fun x -> List.map x ~f:(fix_actions))} :: acc)



let edge_update_table_for (t : t) ver pol (sw_id : switchId) : unit Deferred.t =
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  t.locals <- SwitchMap.add t.locals sw_id
    (Optimize.specialize_policy sw_id pol);
  let local = NetKAT_LocalCompiler.compile sw_id pol in
  Monitor.try_with ~name:"edge_update_table_for" (fun _ ->
    let priority = ref 65536 in
    let table = NetKAT_LocalCompiler.to_table local in
    let edge_table = compute_edge_table t ver table sw_id in
    Log.debug ~tags
      "switch %Lu: Installing edge table %s" sw_id (SDN_Types.string_of_flowTable edge_table);
    Deferred.List.iter edge_table ~f:(fun flow ->
        decr priority;
        send t.ctl c_id (0l, to_flow_mod !priority flow))
    >>= fun () ->  send_barrier_to_sw_with_timeout t sw_id)
  >>= function
  | Ok () ->
    Log.debug ~tags
      "switch %Lu: installed edge table for ver %d" sw_id ver;
    Log.flushed ()
  | Error exn_ ->
    Log.error ~tags
      "switch %Lu: Failed to update table from edge_update_table_for" sw_id;
    Log.error ~tags
      "%s" (Exn.to_string exn_);
    
    Log.flushed ()

let clear_old_table_for (t : t) ver sw_id : unit Deferred.t =
  let open SDN_Types in
  let delete_flows =
    OpenFlow0x01.Message.FlowModMsg {(SDN_OpenFlow0x01.from_flow 0 {pattern = FieldMap.singleton Vlan ver;
                                                                   action = [];
                                                                   cookie = 0L;
                                                                   idle_timeout = Permanent;
                                    hard_timeout = Permanent})
     with command = DeleteFlow} in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  Monitor.try_with ~name:"clear_old_table_for" (fun () ->
    send t.ctl c_id (5l, delete_flows))
  >>= function
  | Ok () -> return ()
  | Error exn_ ->
    Log.error ~tags
      "switch %Lu: Failed to update table in delete_flows" sw_id;
    Log.flushed ()

let ver = ref 1

let consistently_update_table (t : t) pol : unit Deferred.t =
  let switches = get_switchids !(t.nib) in
  let ver_num = !ver + 1 in
  (* Install internal update *)
  Log.debug ~tags "Installing internal tables for ver %d" ver_num;
  Log.flushed ()
  >>=
  fun () -> Deferred.List.iter switches (internal_update_table_for t ver_num pol)
  >>=
  fun () -> Log.debug ~tags "Installing edge tables for ver %d" ver_num;
  Log.flushed ()
  >>=
  (* Install edge update *)
  fun () -> Deferred.List.iter switches (edge_update_table_for t ver_num pol)
  >>=
  (* Delete old rules *)
  fun () -> Deferred.List.iter switches (clear_old_table_for t (VInt.Int16 (ver_num - 1)))
  >>=
  (* Incr ver number *)
  fun () -> return (incr ver)

let update_table_for (t : t) (sw_id : switchId) pol : unit Deferred.t =
  Log.info ~tags "switch %Lu: %s" sw_id (NetKAT_Pretty.string_of_policy pol);
  let delete_flows =
    OpenFlow0x01.Message.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
  let to_flow_mod prio flow =
    OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
  let c_id = Controller.client_id_of_switch t.ctl sw_id in
  t.locals <- SwitchMap.add t.locals sw_id
    (Optimize.specialize_policy sw_id pol);
  let local = NetKAT_LocalCompiler.compile sw_id pol in
  Log.info ~tags "switch %Lu local: %s" sw_id (NetKAT_LocalCompiler.to_string local);
  Monitor.try_with ~name:"update_table_for" (fun () ->
    send t.ctl c_id (5l, delete_flows) >>= fun _ ->
    let priority = ref 65536 in
    let table = NetKAT_LocalCompiler.to_table local in
    Log.info ~tags
      "switch %Lu: Installing table %s" sw_id (SDN_Types.string_of_flowTable table);
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
        "switch %Lu: Failed to update table in update_table_for" sw_id;
      Log.flushed ()

let get_switchids nib =
  Net.Topology.fold_vertexes (fun v acc -> match Net.Topology.vertex_to_label nib v with
    | Async_NetKAT.Switch id -> id::acc
    | _ -> acc)
  nib []

let handler
  (t : t)
  (w : (switchId * SDN_Types.pktOut) Pipe.Writer.t)
  (app : Async_NetKAT.app)
  : (NetKAT_Types.event -> unit Deferred.t) =
  let app' = Async_NetKAT.run app t.nib w () in
  fun e ->
    app' e >>= fun m_pol ->
    match m_pol with
    | Some (pol) ->
      consistently_update_table t pol
      (* Deferred.List.iter (get_switchids !(t.nib)) (fun sw -> update_table_for t sw pol) *)
    | None ->
      begin match e with
        | NetKAT_Types.SwitchUp sw_id ->
        update_table_for t sw_id (Async_NetKAT.default app)
        | _ -> return ()
      end

let start app ?(port=6633) () =
  let open Async_OpenFlow.Stage in
  Controller.create ~log_disconnects:true ~max_pending_connections ~port ()
  >>> fun ctl ->
    let t = {
      ctl = ctl;
      nib = ref (Net.Topology.empty ());
      locals = SwitchMap.empty;
      barriers = XidMap.empty;
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

    Deferred.don't_wait_for (
      Monitor.try_with ~name:"start" (fun () ->
          (Pipe.iter events ~f:(handler t w_out app)))
      >>= function
      | Ok a -> return a
      | Error exn_ ->
        Log.error ~tags "start: Exception occured %s" (Exn.to_string exn_);
        Log.flushed ())
        
