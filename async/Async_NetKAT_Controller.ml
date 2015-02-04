open Core.Std
open Async.Std

module Net = Async_NetKAT.Net

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module Stage = Async_OpenFlow.Stage
module SDN = SDN_Types
module M = OpenFlow0x01.Message
module LC = NetKAT_LocalCompiler

type switchId = SDN_Types.switchId

module SwitchMap = Map.Make(Int64)
module XidMap = Map.Make(Int32)

module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let tags = [("openflow", "controller")]

exception Assertion_failed of string

type t = {
  ctl : Controller.t;
  dis : Discovery.t;
  nib : Net.Topology.t ref;
  mutable prev_order : LC.order;
  mutable order : LC.order;
  mutable repr : LC.t;
  mutable edge : (SDN_Types.flow*int) list SwitchMap.t;
}

let bytes_to_headers
  (port_id : SDN_Types.portId)
  (bytes : Cstruct.t)
  : NetKAT_Semantics.HeadersValues.t =
  let open NetKAT_Semantics.HeadersValues in
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
  (h_new:NetKAT_Semantics.HeadersValues.t)
  (h_old:NetKAT_Semantics.HeadersValues.t)
  : SDN_Types.action list =
  let open SDN_Types in
  let g p acc f =
    if (Field.get f h_new) = (Field.get f h_old)
      then acc
      else (p (Field.get f h_new))::acc in
  let init = match h_new.NetKAT_Semantics.HeadersValues.location with
    | NetKAT_Types.Physical p -> [Output(Physical(p))]
    | _ -> assert false
  in
  NetKAT_Semantics.HeadersValues.Fields.fold
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

let packet_sync_headers (pkt:NetKAT_Semantics.packet) : NetKAT_Semantics.packet * bool =
  let open NetKAT_Semantics in
  let open NetKAT_Types in
  let change = ref false in
  let g p q acc f =
    let v = Field.get f pkt.NetKAT_Semantics.headers in
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

let pattern_matches_pred pattern pred =
  let matches v = function
    | None    -> true
    | Some(u) -> u = v
  in
  let open NetKAT_Types in
  let rec loop p k =
    match pred with
    | True  -> k true
    | False -> k false
    | And(p1, p2) -> loop p1 (fun x -> if x then loop p2 k else k false)
    | Or (p1, p2) -> loop p1 (fun x -> if x then k true else loop p2 k)
    | Neg p1      -> loop p1 (fun x -> k (not x))
    | Test hv ->
      let open OpenFlow0x01_Core in
      begin match hv, pattern with
      | Switch              _ , _
      | Location(Query      _), _
      | Location(Pipe       _), _ -> assert false
      | Location(Physical   v), { inPort = mv } ->
        begin match mv with
        | None    -> true
        | Some(u) -> v = Int32.of_int_exn u
        end
      | EthSrc              v , { dlSrc = mv } -> matches v mv
      | EthDst              v , { dlDst = mv } -> matches v mv
      | EthType             v , { dlTyp = mv } -> matches v mv
      | Vlan                v , { dlVlan = mv } -> matches (Some v) mv
      | VlanPcp             v , { dlVlanPcp = mv } -> matches v mv
      | IPProto             v , { nwProto = mv } -> matches v mv
      | IP4Src         (v, m) , { nwSrc = mv }
      | IP4Dst         (v, m) , { nwDst = mv } ->
        begin match mv with
        | None -> true
        | Some(u) ->
          let n = match u.m_mask with None -> 32l | Some n -> Int32.(32l - m) in
          let open SDN_Types.Pattern in
          Ip.less_eq (v, m) (u.m_value, n)
        end
      | TCPSrcPort          v , { tpSrc = mv } -> matches v mv
      | TCPDstPort          v , { tpDst = mv } -> matches v mv
      end
  in
  loop pred (fun x -> x)

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
  : (t, Controller.e, NetKAT_Types.event) Stage.t =
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
    | `Disconnect (c_id, exn) ->
      let open Net.Topology in
      let v  = vertex_of_label !(t.nib) (Async_NetKAT.Switch c_id) in
      let ps = vertex_to_ports !(t.nib) v in
      return (PortSet.fold ps ~init:[SwitchDown c_id]
		~f:(fun acc p -> (PortDown(c_id, p))::acc))
    | `Message (c_id, (xid, msg)) ->
      let open OpenFlow0x01.Message in
      begin match msg with
        (* only process packet_ins from physical ports *)
        | PacketInMsg pi when pi.OpenFlow0x01_Core.port <= 0xff00 ->
          let open OpenFlow0x01_Core in
          let port_id = Int32.of_int_exn pi.port in
          let payload = SDN_OpenFlow0x01.to_payload pi.input_payload in
          (* Eval the packet to get the list of packets that should go to
           * pipes, and the list of packets that can be forwarded to physical
           * locations.
           * *)
          let open NetKAT_Semantics in
          let pkt0 = {
            switch = c_id;
            headers = bytes_to_headers port_id (SDN_Types.payload_bytes payload);
            payload = payload;
          } in
          begin
            (* XXX(seliopou): What if the packet's modified? Should buf_id be
             * exposed to the application?
             * *)
            let pis, qus, phys = NetKAT_LocalCompiler.eval_pipes pkt0 t.repr in
            let outs = Deferred.List.iter phys ~f:(fun pkt1 ->
              let acts = headers_to_actions pkt1.headers pkt0.headers in
              let out  = (c_id, (payload, Some(port_id), acts)) in
              Pipe.write w_out out) in
            (* XXX(seliopou): queries? *)
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
              PacketIn(pipe, c_id, port_id, payload, pi.total_len)))
          end
        | PortStatusMsg ps ->
          let open OpenFlow0x01.PortStatus in
          let open OpenFlow0x01.PortDescription in
          begin match ps.reason, port_desc_useable ps.desc with
            | ChangeReason.Add, true
            | ChangeReason.Modify, true ->
              let pt_id = Int32.of_int_exn (ps.desc.port_no) in
              return [PortUp(c_id, pt_id)]
            | ChangeReason.Delete, _
            | ChangeReason.Modify, false ->
              let pt_id = Int32.of_int_exn (ps.desc.port_no) in
              return [PortDown(c_id, pt_id)]
            | _ ->
              return []
          end
        | _ ->
          Log.debug ~tags "switch %Lu: dropped unhandled message: %s" c_id (to_string msg);
          return []
      end

module BestEffort = struct
  let restrict sw_id repr =
    LC.restrict NetKAT_Types.(Switch sw_id) repr

  let install_flows_for (t : Controller.t) sw_id table =
    let to_flow_mod p f = M.FlowModMsg (SDN_OpenFlow0x01.from_flow p f) in
    let priority = ref 65536 in
    Deferred.List.iter table ~f:(fun flow ->
      decr priority;
      Controller.send t sw_id (0l, to_flow_mod !priority flow)
      >>| function
        | `Drop exn -> raise exn
        | `Sent _   -> ())

  let delete_flows_for (t :Controller.t) sw_id =
    let delete_flows = M.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
    Controller.send t sw_id (5l, delete_flows)
    >>| function
      | `Drop exn -> raise exn
      | `Sent _   -> ()

  let bring_up_switch (t : t) (sw_id : SDN.switchId) ?old new_r =
    match old with
    | Some(old_r) when 
	   (LC.equal (restrict sw_id old_r) (restrict sw_id new_r) &&
	    t.prev_order = t.order) ->
      Log.debug ~tags
        "[policy] Skipping identical policy update for swithc %Lu" sw_id ;
      return ()
    | _ ->
      let table = LC.(to_table sw_id new_r) in
      Monitor.try_with ~name:"BestEffort.bring_up_switch" (fun () ->
        delete_flows_for t.ctl sw_id >>= fun () ->
        install_flows_for t.ctl sw_id table)
      >>= function
        | Ok x       -> return x
        | Error _exn ->
          Log.debug ~tags
            "switch %Lu: disconnected while attempting to bring up... skipping" sw_id;
          Log.flushed () >>| fun () -> Printf.eprintf "%s\n%!" (Exn.to_string _exn)

  let implement_policy (t : t) (nib : Net.Topology.t) ?old repr =
    Deferred.List.iter (TUtil.switch_ids nib) (fun sw_id ->
      bring_up_switch t sw_id ?old repr)
end

module PerPacketConsistent = struct
  open SDN_Types

  let specialize_action ver internal_ports actions =
    List.fold_right actions ~init:[] ~f:(fun action acc ->
      begin match action with
      | Output (Physical   pt) ->
        if not (Net.Topology.PortSet.mem internal_ports pt) then
          [Modify (SetVlan None)]
        else
          [Modify (SetVlan (Some ver))]
      | Output (Controller n ) ->
        [Modify (SetVlan None)]
      | Output _               -> assert false
      | _                      -> acc
      end @ (action :: acc))

  let specialize_edge_to (ver : int) internal_ports (table : SDN.flowTable) =
    let vlan_none = 65535 in
    List.filter_map table ~f:(fun flow ->
      begin match flow.pattern.Pattern.inPort with
      | Some pt when Net.Topology.PortSet.mem internal_ports pt ->
        None
      | _ ->
        Some { flow with
          pattern = { flow.pattern with Pattern.dlVlan = Some vlan_none }
        ; action  = List.map flow.action ~f:(fun x ->
            List.map x ~f:(specialize_action ver internal_ports))
        }
      end)

  let specialize_internal_to (ver : int) internal_ports (table : SDN.flowTable) =
    List.map table ~f:(fun flow ->
      { flow with
        pattern = { flow.pattern with Pattern.dlVlan = Some ver }
      ; action  = List.map flow.action ~f:(fun x ->
          List.map x ~f:(specialize_action ver internal_ports))
      })

  let clear_policy_for (t : Controller.t) (ver : int) sw_id =
    let open OpenFlow0x01_Core in
    let clear_version_message = M.FlowModMsg { SDN_OpenFlow0x01.from_flow 0
      { pattern = { Pattern.match_all with Pattern.dlVlan = Some ver }
      ; action = []
      ; cookie = 0L
      ; idle_timeout = Permanent
      ; hard_timeout = Permanent
      } with command = DeleteFlow } in
    Monitor.try_with ~name:"PerPacketConsistent.clear_policy_for" (fun () ->
      Controller.send t sw_id (5l, clear_version_message))
    >>| function
      | Ok (`Sent _)    -> ()
      | Ok (`Drop _exn)
      | Error _exn      ->
        Log.error ~tags "switch %Lu: Failed to delete flows for ver %d" sw_id ver

  let internal_install_policy_for (t : t) (ver : int) repr (sw_id : switchId) =
    begin let open Deferred.Result in
    Monitor.try_with ~name:"PerPacketConsistent.internal_install_policy_for" (fun () ->
      let table0 = LC.(to_table sw_id repr) in
      let table1 = specialize_internal_to
        ver (TUtil.internal_ports !(t.nib) sw_id) table0 in
      assert (List.length table1 > 0);
      BestEffort.install_flows_for t.ctl sw_id table1)
    >>= fun () -> Controller.barrier t.ctl sw_id
    end
    >>| function
      | Ok () ->
        Log.debug ~tags
          "switch %Lu: installed internal table for ver %d" sw_id ver;
      | Error _ ->
        Log.debug ~tags
          "switch %Lu: disconnected while installing internal table for ver %d... skipping" sw_id ver

  (* Comparison should be made based on patterns only, not actions *)
  (* Assumes both FT are sorted in descending order by priority *)
  let rec flowtable_diff (ft1 : (SDN_Types.flow*int) list) (ft2 : (SDN_Types.flow*int) list) =
    let open SDN_Types in
    match ft1,ft2 with
    | (flow1,pri1)::ft1, (flow2,pri2)::ft2 ->
      if pri1 > pri2 then
        (flow1, pri1) :: flowtable_diff ft1 ((flow2,pri2)::ft2)
      else if pri1 = pri2 && flow1.pattern = flow2.pattern then
        flowtable_diff ft1 ((flow2,pri2)::ft2)
      else
        flowtable_diff ((flow1,pri1) :: ft1) ft2
    | _, [] -> ft1
    | [], _ -> []

  (* Assumptions:
     - switch respects priorities when deleting flows
  *)
  let swap_update_for (t : t) sw_id c_id new_table : unit Deferred.t =
    let open OpenFlow0x01_Core in
    let max_priority = 65535 in
    let old_table = match SwitchMap.find t.edge sw_id with
      | Some ft -> ft
      | None -> [] in
    let (new_table, _) = List.fold new_table ~init:([], max_priority)
        ~f:(fun (acc,pri) x -> ((x,pri) :: acc, pri - 1)) in
    let new_table = List.rev new_table in
    let del_table = List.rev (flowtable_diff old_table new_table) in
    let to_flow_mod prio flow =
      M.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
    let to_flow_del prio flow =
      M.FlowModMsg ({SDN_OpenFlow0x01.from_flow prio flow with command = DeleteStrictFlow}) in
    (* Install the new table *)
    Deferred.List.iter new_table ~f:(fun (flow, prio) ->
      send t.ctl c_id (0l, to_flow_mod prio flow))
    (* Delete the old table from the bottom up *)
    >>= fun () -> Deferred.List.iter del_table ~f:(fun (flow, prio) ->
      send t.ctl c_id (0l, to_flow_del prio flow))
    >>| fun () -> t.edge <- SwitchMap.add t.edge sw_id new_table

  let edge_install_policy_for (t : t) ver repr (sw_id : switchId) : unit Deferred.t =
    begin let open Deferred.Result in
    Monitor.try_with ~name:"PerPacketConsistent.edge_install_policy_for" (fun () ->
      let table = LC.(to_table sw_id repr) in
      let edge_table = specialize_edge_to
        ver (TUtil.internal_ports !(t.nib) sw_id) table in
      Log.debug ~tags
        "switch %Lu: Installing edge table for ver %d" sw_id ver;
      swap_update_for t sw_id sw_id edge_table)
    >>= fun () -> Controller.barrier t.ctl sw_id
    end
    >>| function
      | Ok () ->
        Log.debug ~tags "switch %Lu: installed edge table for ver %d" sw_id ver
      | Error _ ->
        Log.debug ~tags "switch %Lu: disconnected while installing edge table for ver %d... skipping" sw_id ver

  let ver = ref 1

  let implement_policy (t : t) repr : unit Deferred.t =
    (* XXX(seliopou): It might be better to iterate over client ids rather than
     * switch ids. A client id is guaranteed to be unique within a run of a
     * program, whereas a switch id may be reused across client ids, i.e., a
     * switch connects, disconnects, and connects again. Due to this behavior,
     * it may be possible to get into an inconsistent state below. Maybe. *)
    let switches = TUtil.switch_ids !(t.nib) in
    let ver_num = !ver + 1 in
    (* Install internal update *)
    Log.debug ~tags "Installing internal tables for ver %d" ver_num;
    Log.flushed ()
    >>= fun () ->
    Deferred.List.iter switches (internal_install_policy_for t ver_num repr)
    >>= fun () ->
    (Log.debug ~tags "Installing edge tables for ver %d" ver_num;
     Log.flushed ())
    >>= fun () ->
    (* Install edge update *)
    Deferred.List.iter switches (edge_install_policy_for t ver_num repr)
    >>= fun () ->
    (* Delete old rules *)
    Deferred.List.iter switches (clear_policy_for t.ctl (ver_num - 1))
    >>| fun () ->
      incr ver

  let bring_up_switch (t : t) (sw_id : switchId) repr =
    Monitor.try_with ~name:"PerPacketConsistent.bring_up_switch" (fun () ->
      BestEffort.delete_flows_for t.ctl sw_id >>= fun () ->
      internal_install_policy_for t !ver repr sw_id >>= fun () ->
      edge_install_policy_for t !ver repr sw_id)
    >>= function
      | Ok x -> return ()
      | Error _exn ->
        Log.debug ~tags
          "switch %Lu: disconnected while attempting to bring up... skipping" sw_id;
        Log.flushed () >>| fun () -> Printf.eprintf "%s\n%!" (Exn.to_string _exn)

end

(* Send a packet_out message to the specified switch, and log any errors that
 * may occur.
 * *)
let send_pkt_out (ctl : Controller.t) (sw_id, pkt_out) =
  Monitor.try_with ~name:"send_pkt_out" (fun () ->
    Controller.send ctl sw_id (0l, M.PacketOutMsg
      (SDN_OpenFlow0x01.from_packetOut pkt_out)))
  >>= function
    | Ok (`Sent x)    -> return ()
    | Ok (`Drop _exn)
    | Error _exn      ->
      Log.error ~tags "switch %Lu: Failed to send packet_out" sw_id;
      Log.flushed () >>| fun () ->
        if not (_exn = Not_found) then
          Printf.eprintf "%s\n%!" (Exn.to_string _exn)

(* Start the controller, running the given application.
 * *)
let start app ?(port=6633) ?(update=`BestEffort) ?(policy_queue_size=0) () =
  let open Stage in
  Controller.create ~max_pending_connections ~port ()
  >>= fun ctl ->
    let d_ctl, d_stage, d_app = Discovery.create () in
    let app =
      let open Async_NetKAT in
      let guarded_app = guard' (neg (Discovery.managed_traffic d_ctl)) app in
      union d_app guarded_app
    in

    (* Create the controller struct to contain all the state of the controller.
     * *)
    let t =
      { ctl = ctl
      ; dis = d_ctl
      ; nib = ref (Net.Topology.empty ())
      ; prev_order = `Heuristic
      ; order = `Heuristic
      ; repr = LC.compile (Async_NetKAT.default app)
      ; edge = SwitchMap.empty
      }
    in

    (* Setup the controller stages. Use the provides features stage to collect
     * switch features, and sequence that with a stage that will transform
     * OpenFlow 1.0 events to the high-level event type that applications know
     * how to consume.
     *
     * The process of transforming OpenFlow 1.0 events into network events may
     * produce packet_out messages, as each packet_in message is evaluated
     * against the current policy. Only packets that remain at controller
     * locations, i.e., "pipes", will produce a PacketIn network event. Those
     * that end up at a physical port will produce packet_out messages. Create
     * a pipe that to_event can write packet_out messages to, and handle them
     * below.
     * *)
    let r_pkt_out, s_pkt_out = Pipe.create () in
    let stages =
      let events   = to_event s_pkt_out in
      let discover = local (fun t -> t.nib) d_stage in
      events >=> discover
    in

    (* Initialize the application to produce an event callback and
     * Pipe.Reader.t's for packet out messages and policy updates.
     * *)
    let recv, callback = Async_NetKAT.run app t.nib () in
    let events = run stages t (Controller.listen ctl) in

    (* Pick a method for updating the network. Each method needs to be able to
     * implement a policy across the entire network as well as handle new
     * switches entering the network.
     * *)
    let implement_policy, bring_up_switch = match update with
      | `BestEffort ->
        BestEffort.(
          (fun t ?old repr -> implement_policy t !(t.nib) ?old repr),
          (fun t sw_id ?old repr -> bring_up_switch t sw_id ?old repr))
      | `PerPacketConsistent ->
        (* XXX(seliopou): budget has to be big, otherwise consistent updates will
         * lead to deadlocks where event processing is blocked on a table update,
         * but the table update can't complete until an event, specifically a
         * barrier reply, is processed.
         *
         * This and other parameters need to be tweaked. This'll happen in the app
         * branch. For now, the parameter is set so that the controller can manage a
         * topo,2,3 and achieve connectivity with --learn enabled.
         *)
        Pipe.set_size_budget events 50;
        PerPacketConsistent.(
          (fun t ?old repr -> implement_policy t repr),
          (fun t sw_id ?old repr -> bring_up_switch t sw_id repr))
    in

    let implement_policy' t q =
      let len = Queue.length q in
      assert (len > 0);
      if policy_queue_size > 0 then
        Log.info ~tags "[policy] Processing queue of size %d" len;

      let old = t.repr in
      let cache = `Preserve old in
      t.repr   <- LC.compile ~cache (Queue.get q (len - 1));

      if LC.equal old t.repr then begin
        Log.debug ~tags "[policy] Skipping identical policy update";
        return ()
      end else
        implement_policy t ~old t.repr
    in

    (* This is the main event handler for the controller. First it sends
     * events to the application callback. Then it checks to see if the event
     * is a SwitchUp event, in which case it's necessary to populate the new
     * switch's flowtable with the application's current policy.
     *)
    let handler e =
      callback e >>= fun () ->
      match e with
      | NetKAT_Types.SwitchUp sw_id ->
        bring_up_switch t sw_id t.repr
      | _ ->
        return ()
    in

    (* Combine the pkt_out messages receied from the application and those that
     * are generated from evaluating the policy at the controller.
     * *)
    let open Async_NetKAT in
    let pkt_outs = Pipe.interleave [r_pkt_out; recv.pkt_out] in

    Pipe.set_size_budget recv.update policy_queue_size;

    (* Kick off the three top-level logical threads of the controller. The
     * first handles incoming events from switches. The second sends pkt_out
     * messages to switches that are generated from either the application, or
     * policy evaluation of packets at the controller (described above). The
     * third listens for policy updates from the application and implements the
     * policy on the switches.
     * *)
    let open Deferred in
    don't_wait_for (Pipe.iter  events      handler);                (* input  *)
    don't_wait_for (Pipe.iter  pkt_outs    (send_pkt_out ctl));     (* output *)
    don't_wait_for (Pipe.iter' recv.update (implement_policy' t));  (* output *)

    return t
  ;;

let query ?(ignore_drops=true) pred t =
  let pkt, byte = (ref 0L, ref 0L) in
  Deferred.List.iter ~how:`Parallel (TUtil.switch_ids !(t.nib)) ~f:(fun sw_id ->
    match Optimize.specialize_pred sw_id pred with
    | NetKAT_Types.False -> return ()
    | pred  -> Controller.individual_stats t.ctl sw_id
      >>| function
        | Result.Ok flows ->
          List.iter flows (fun f ->
            let open OpenFlow0x01_Stats in
            (* When ignore_drops is true, then packet and byte counts of the
             * flow do not contribute to the total. *)
            if ((not ignore_drops) || f.actions <> []) &&
                pattern_matches_pred f.of_match pred then begin
              pkt  := Int64.(!pkt + f.packet_count);
              byte := Int64.(!byte + f.byte_count)
            end)
        | Result.Error exn_ ->
          Log.error ~tags "Unable to complete query: %s" (Sexp.to_string
          (Exn.sexp_of_t exn_)))
  >>| fun () -> (!pkt, !byte)

let nib t =
  !(t.nib)

let enable_discovery t =
  Discovery.start t.dis

let disable_discovery t =
  Discovery.stop t.dis

let enable_host_discovery t =
  Discovery.(Host.start t.dis.host_ctl)

let disable_host_discovery t =
  Discovery.(Host.stop t.dis.host_ctl)

let set_order (t : t) (order : LC.order) : unit =
  t.prev_order <- t.order;
  t.order <- order
