open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

module OF10 = Frenetic_OpenFlow0x01
module Controller = Frenetic_OpenFlow0x01_Controller
module Log = Frenetic_Log
module Upd = Frenetic_NetKAT_Updates

let printf = Log.printf

let bytes_to_headers
  (port_id : Frenetic_OpenFlow.portId)
  (bytes : Cstruct.t)
  : Frenetic_NetKAT_Semantics.HeadersValues.t =
  let open Frenetic_NetKAT_Semantics.HeadersValues in
  let open Frenetic_Packet in
  let pkt = Frenetic_Packet.parse bytes in
  { location = Frenetic_NetKAT.Physical port_id
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

let packet_sync_headers (pkt:Frenetic_NetKAT_Semantics.packet) : Frenetic_NetKAT_Semantics.packet * bool =
  let open Frenetic_NetKAT_Semantics in
  let open Frenetic_NetKAT in
  let change = ref false in
  let g p q acc f =
    let v = Field.get f pkt.Frenetic_NetKAT_Semantics.headers in
    if p v acc then
      acc
    else begin
      change := true;
      q acc v
    end in
  let fail field = (fun _ -> failwith "unsupported modification") in
  let packet = Frenetic_Packet.parse (Frenetic_OpenFlow.payload_bytes pkt.payload) in
  let packet' = HeadersValues.Fields.fold
    ~init:packet
    ~location:(fun acc _ -> acc)
    ~ethSrc:(g (fun v p -> v = p.Frenetic_Packet.dlSrc) Frenetic_Packet.setDlSrc)
    ~ethDst:(g (fun v p -> v = p.Frenetic_Packet.dlDst) Frenetic_Packet.setDlDst)
    (* XXX(seliopou): Fix impls of: vlan, vlanPcp *)
    ~vlan:(g (fun _ _ -> true) (fail "vlan"))
    ~vlanPcp:(g (fun _ _ -> true) (fail "vlanPcp"))
    ~ipSrc:(g
      (fun v p -> try v = Frenetic_Packet.nwSrc p with Invalid_argument(_) -> true)
      (fun acc nw -> Frenetic_Packet.setNwSrc acc nw))
    ~ipDst:(g
      (fun v p -> try v = Frenetic_Packet.nwDst p with Invalid_argument(_) -> true)
      (fun acc nw -> Frenetic_Packet.setNwDst acc nw))
    ~tcpSrcPort:(g
      (fun v p -> try v= Frenetic_Packet.tpSrc p with Invalid_argument(_) -> true)
      Frenetic_Packet.setTpSrc)
    ~tcpDstPort:(g
      (fun v p -> try v = Frenetic_Packet.tpDst p with Invalid_argument(_) -> true)
      Frenetic_Packet.setTpDst)
    (* XXX(seliopou): currently does not support: *)
    ~ethType:(g (fun _ _ -> true) (fail "ethType"))
    ~ipProto:(g (fun _ _ -> true) (fail "ipProto")) in
  ({ pkt with payload = match pkt.payload with
    | Frenetic_OpenFlow.NotBuffered(_) -> Frenetic_OpenFlow.NotBuffered(Frenetic_Packet.marshal packet')
    | Frenetic_OpenFlow.Buffered(n, _) -> Frenetic_OpenFlow.Buffered(n, Frenetic_Packet.marshal packet')
  }, !change)

let of_to_netkat_event fdd (evt : Controller.event) : Frenetic_NetKAT.event list =
  match evt with
  (* TODO(arjun): include switch features in SwitchUp *)
  | `Connect (sw_id, feats) ->
     (* TODO(joe): Did we just want the port number? Or do we want the entire description? *)
     let ps =
       List.filter
	       (List.map feats.ports ~f:(fun desc -> Int32.of_int_exn desc.port_no))
	       ~f:(fun p -> not (p = 0xFFFEl))
     in [SwitchUp(sw_id, ps)]
  | `Disconnect (sw_id) -> [SwitchDown sw_id]
  | `Message (sw_id, hdr, PortStatusMsg ps) ->
      begin match ps.reason, ps.desc.config.down with
        | Add, _
        | Modify, true ->
          let pt_id = Int32.of_int_exn (ps.desc.port_no) in
          [PortUp (sw_id, pt_id)]
        | Delete, _
        | Modify, false ->
          let pt_id = Int32.of_int_exn (ps.desc.port_no) in
          [PortDown (sw_id, pt_id)]
      end
  | `Message (sw_id,hdr,PacketInMsg pi) when pi.port <= 0xff00 ->
      let port_id = Int32.of_int_exn pi.port in
      let payload : Frenetic_OpenFlow.payload =
        match pi.input_payload with
        | Buffered (id,bs) -> Buffered (id,bs)
        | NotBuffered bs -> NotBuffered bs in
      (* Eval the packet to get the list of packets that should go to
       * pipes, and the list of packets that can be forwarded to physical
       * locations.
       * *)
      let open Frenetic_NetKAT_Semantics in
      let pkt0 = {
        switch = sw_id;
        headers = bytes_to_headers port_id (Frenetic_OpenFlow.payload_bytes payload);
        payload = payload;
      } in
      let pis, _, _ =
        match pi.reason with
        | NoMatch -> ( [("table_miss", pkt0)], [], [] )
        | ExplicitSend -> Frenetic_NetKAT_Compiler.eval_pipes pkt0 fdd
      in
      List.map pis ~f:(fun (pipe, pkt2) ->
        let pkt3, changed = packet_sync_headers pkt2 in
        let payload = match payload, changed with
          | Frenetic_OpenFlow.NotBuffered(_), _
          | _ , true ->
            Frenetic_OpenFlow.NotBuffered(Frenetic_OpenFlow.payload_bytes pkt3.payload)
          | Frenetic_OpenFlow.Buffered(buf_id, bytes), false ->
            Frenetic_OpenFlow.Buffered(buf_id, bytes)
        in
        PacketIn(pipe, sw_id, port_id, payload, pi.total_len)
      )
  | _ -> []

module type CONTROLLER = sig
  val update_policy : policy -> unit Deferred.t
  val send_packet_out : switchId -> Frenetic_OpenFlow.pktOut -> unit Deferred.t
  val event : unit -> event Deferred.t
  val query : string -> (Int64.t * Int64.t) Deferred.t
  val port_stats : switchId -> portId -> OF10.portStats list Deferred.t
  val is_query : string -> bool
  val start : int -> unit
  val current_switches : unit -> (switchId * portId list) list Deferred.t
  val set_current_compiler_options : Frenetic_NetKAT_Compiler.compiler_options -> unit
end

module Make : CONTROLLER = struct
  let fdd = ref (Frenetic_NetKAT_Compiler.compile_local drop)
  let current_compiler_options = ref (Frenetic_NetKAT_Compiler.default_compiler_options)
  let stats : (string, Int64.t * Int64.t) Hashtbl.Poly.t = Hashtbl.Poly.create ()
  let (pol_reader, pol_writer) = Pipe.create ()
  let (pktout_reader, pktout_writer) = Pipe.create ()
  let (event_reader, event_writer) =  Pipe.create ()

  (* TODO(arjun,jnfoster): Result should be determined with network is
     updated. *)
  let update_policy (pol : policy) : unit Deferred.t =
    Pipe.write pol_writer pol

  let send_packet_out (sw_id : switchId)
    (pkt_out : Frenetic_OpenFlow.pktOut) : unit Deferred.t =
    Log.printf ~level:`Debug "SENDING PKT_OUT";
    Pipe.write pktout_writer (sw_id, pkt_out)

  let event () : event Deferred.t =
    Pipe.read event_reader
    >>= function
    | `Eof -> assert false
    | `Ok evt -> Deferred.return evt

  let current_switches () =
    Controller.get_switches () >>= fun switches ->
    Deferred.List.filter_map ~f:Controller.get_switch_features
      switches >>| fun features ->
    let get_switch_and_ports (feats : OF10.SwitchFeatures.t) =
      (feats.switch_id,
       List.filter_map ~f:(fun port_desc ->
         if port_desc.port_no = 0xFFFE then
           None
         else
           Some (Int32.of_int_exn port_desc.port_no))
         feats.ports) in
    List.map ~f:get_switch_and_ports features

  let get_table (sw_id : switchId) : (Frenetic_OpenFlow.flow * string list) list =
    Frenetic_NetKAT_Compiler.to_table' ~options:!current_compiler_options sw_id !fdd

  let raw_query (name : string) : (Int64.t * Int64.t) Deferred.t =
    Controller.get_switches () >>= fun switches ->
    Deferred.List.map ~how:`Parallel
      switches ~f:(fun sw_id ->
        let pats = List.filter_map (get_table sw_id) ~f:(fun (flow, names) ->
          if List.mem names name then
            Some flow.pattern
          else
            None) in
        Deferred.List.map ~how:`Parallel pats
          ~f:(fun pat ->
            let pat0x01 = To0x01.from_pattern pat in
            let req =
              OF10.IndividualRequest
                { sr_of_match = pat0x01; sr_table_id = 0xff; sr_out_port = None } in
            Controller.send_txn sw_id (OF10.Message.StatsRequestMsg req) >>= function
              | `Eof -> return (0L,0L)
              | `Ok l -> begin
                l >>| function
                  | [OF10.Message.StatsReplyMsg (IndividualFlowRep stats)] ->
                    (List.sum (module Int64) stats ~f:(fun stat -> stat.packet_count),
                     List.sum (module Int64) stats ~f:(fun stat -> stat.byte_count))
                  | _ -> (0L, 0L)
              end))
    >>| fun stats ->
      List.fold (List.concat stats) ~init:(0L, 0L)
        ~f:(fun (pkts, bytes) (pkts', bytes') ->
            Int64.(pkts + pkts', bytes + bytes'))

  let query (name : string) : (Int64.t * Int64.t) Deferred.t =
    raw_query name
    >>= fun (pkts, bytes) ->
    let (pkts', bytes') = Hashtbl.Poly.find_exn stats name in
    Deferred.return (Int64.(pkts + pkts', bytes + bytes'))

  let port_stats (sw_id : switchId) (pid : portId) : OF10.portStats list Deferred.t =
    let pt = Int32.(to_int_exn pid) in
    let req = OF10.PortRequest (Some (PhysicalPort pt)) in
    Controller.send_txn sw_id (OF10.Message.StatsRequestMsg req) >>= function
      | `Eof -> assert false
      | `Ok l -> begin
        l >>| function
          | [OF10.Message.StatsReplyMsg (PortRep ps)] -> ps
          | _ -> assert false
      end

  let is_query (name : string) : bool = Hashtbl.Poly.mem stats name

  let update_all_switches (pol : policy) : unit Deferred.t =
    Log.printf ~level:`Debug "Installing policy\n%s" (Frenetic_NetKAT_Pretty.string_of_policy pol);

    let new_queries = Frenetic_NetKAT_Semantics.queries_of_policy pol in
    (* Discard old queries *)
    Hashtbl.Poly.filteri_inplace stats
      ~f:(fun key _ -> List.mem new_queries key);
    (* Queries that have to be saved. *)
    let preserved_queries = Hashtbl.Poly.keys stats in
    (* Initialize new queries to 0 *)
    List.iter new_queries ~f:(fun query ->
      if not (Hashtbl.Poly.mem stats query) then
        Hashtbl.Poly.set stats ~key:query ~data:(0L, 0L));
    (* Update queries that have been preserved. The query function itself
       adds the current value of the counters to the cumulative sum. We
       simply store this in stats. *)
    Deferred.List.iter preserved_queries ~f:(fun qname ->
      query qname
      >>| fun stat ->
      Hashtbl.Poly.set stats qname stat)
    >>= fun () ->
    (* Actually update things *)
    fdd := Frenetic_NetKAT_Compiler.compile_local ~options:!current_compiler_options pol;
    Upd.BestEffortUpdate.set_current_compiler_options !current_compiler_options;
    Upd.BestEffortUpdate.implement_policy !fdd

  let handle_event (evt : Controller.event) : unit Deferred.t =
    List.iter (of_to_netkat_event !fdd evt) ~f:(fun netkat_evt ->
      Pipe.write_without_pushback event_writer netkat_evt);
    match evt with
     | `Connect (sw_id, feats) ->
       printf ~level:`Info "switch %Ld connected" sw_id;
       Upd.BestEffortUpdate.set_current_compiler_options !current_compiler_options;
       Upd.BestEffortUpdate.bring_up_switch sw_id !fdd
     | _ -> Deferred.return ()

  let set_current_compiler_options opt =
    current_compiler_options := opt

  let send_pktout ((sw_id, pktout) : switchId * Frenetic_OpenFlow.pktOut) : unit Deferred.t =
    let pktout0x01 = To0x01.from_packetOut pktout in
    Controller.send sw_id 0l (OF10.Message.PacketOutMsg pktout0x01) >>= function
      | `Eof -> return ()
      | `Ok -> return ()

  let start (openflow_port:int) : unit =
    Controller.init openflow_port;
    don't_wait_for (Pipe.iter pol_reader ~f:update_all_switches);
    don't_wait_for (Pipe.iter (Controller.events) ~f:handle_event);
    don't_wait_for (Pipe.iter pktout_reader ~f:send_pktout)
end

