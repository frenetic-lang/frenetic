open Core.Std
open Async.Std
open Async_NetKAT_Controller_Common
open Async_NetKAT_Updates

exception Assertion_failed of string

type t = Async_NetKAT_Controller_Common.t

let max_pending_connections = 64

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

module type STATE = sig
  val ctl : Controller.t
  val get_switches : unit -> SDN_Types.switchId list
  val get_order : unit -> LC.order
  val get_prev_order : unit -> LC.order
  val get_nib : unit -> Net.Topology.t
end

module Make (MakeUpdate : functor (Args : CONSISTENT_UPDATE_ARGS) -> UPDATE) = struct

  let start app ?(port=6633) ?(policy_queue_size=0) () =
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
      let module State = (struct
        let ctl = ctl
        let get_switches () = TUtil.switch_ids !(t.nib)
        let get_order () = t.order
        let get_prev_order () = t.prev_order
        let get_nib () = !(t.nib)
        let get_edge () = t.edge
        let set_edge edge = t.edge <- edge
      end) in
      let module Update = MakeUpdate (State) in
      let open Update in
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

      (* XXX(seliopou): budget has to be big, otherwise consistent updates will
       * lead to deadlocks where event processing is blocked on a table update,
       * but the table update can't complete until an event, specifically a
       * barrier reply, is processed.
       *
       * This and other parameters need to be tweaked. This'll happen in the app
       * branch. For now, the parameter is set so that the controller can manage a
       * topo,2,3 and achieve connectivity with --learn enabled.
       *
       * NOTE(arjun): This used to only be set for PerPacketConsistent.
       *)
      Pipe.set_size_budget events 50;

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
          implement_policy ~old t.repr
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
          bring_up_switch sw_id t.repr
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
end

(* Start the controller, running the given application.
 * *)
let start app ?(port=6633) ?(update=`BestEffort) ?(policy_queue_size=0) () =
  match update with
    | `BestEffort ->
      let module M = Make (BestEffort) in
      M.start app ~port ~policy_queue_size ()
    | `PerPacketConsistent ->
      let module M = Make (PerPacketConsistent) in
      M.start app ~port ~policy_queue_size ()

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
