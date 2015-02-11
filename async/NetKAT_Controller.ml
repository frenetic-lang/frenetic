open Core.Std
open Async.Std
open NetKAT_Types
open Async_NetKAT_Updates
module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module Log = Async_OpenFlow.Log

let printf = Log.printf

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
  let fail field = (fun _ -> failwith "unsupported modification") in
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

let of_to_netkat_event fdd (evt : Controller.e) : NetKAT_Types.event list =
  match evt with
  (* TODO(arjun): include switch features in SwitchUp *)
  | `Connect (sw_id, feats) -> [SwitchUp sw_id]
  | `Disconnect (sw_id, exn) -> [SwitchDown sw_id]
  | `Message (sw_id, (xid, PortStatusMsg ps)) ->
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
  | `Message (sw_id, (xid, PacketInMsg pi)) when pi.port <= 0xff00 ->
      let open OpenFlow0x01_Core in
      let port_id = Int32.of_int_exn pi.port in
      let payload = SDN_OpenFlow0x01.to_payload pi.input_payload in
      (* Eval the packet to get the list of packets that should go to
       * pipes, and the list of packets that can be forwarded to physical
       * locations.
       * *)
      let open NetKAT_Semantics in
      let pkt0 = {
        switch = sw_id;
        headers = bytes_to_headers port_id (SDN_Types.payload_bytes payload);
        payload = payload;
      } in
      let pis, qus, phys = NetKAT_LocalCompiler.eval_pipes pkt0 fdd in
      List.map pis ~f:(fun (pipe, pkt2) ->
        let pkt3, changed = packet_sync_headers pkt2 in
        let payload = match payload, changed with
            | SDN_Types.NotBuffered(_), _
            | _                       , true ->
              SDN_Types.NotBuffered(SDN_Types.payload_bytes pkt3.payload)
            | SDN_Types.Buffered(buf_id, bytes), false ->
              SDN_Types.Buffered(buf_id, bytes)
        in
        PacketIn(pipe, sw_id, port_id, payload, pi.total_len))
  | _ -> []

module type ARGS = sig

  val controller : Controller.t
end

module type CONTROLLER = sig

  val update_policy : policy -> unit Deferred.t
  val send_packet_out : switchId -> SDN_Types.pktOut -> unit Deferred.t
  val event : unit -> event Deferred.t
  val query : string -> (Int64.t * Int64.t) Deferred.t
  val start : unit -> unit

end

module Make (Args : ARGS) : CONTROLLER = struct
  open Args

  let fdd = ref (NetKAT_LocalCompiler.compile drop)
  let stats : (string, Int64.t * Int64.t) Hashtbl.Poly.t = Hashtbl.Poly.create ()
  let (pol_reader, pol_writer) = Pipe.create ()
  let (pktout_reader, pktout_writer) = Pipe.create ()
  let (event_reader, event_writer) =  Pipe.create ()

  (* TODO(arjun,jnfoster): Result should be determined with network is
     updated. *)
  let update_policy (pol : policy) : unit Deferred.t =
    Pipe.write pol_writer pol

  let send_packet_out (sw_id : switchId)
    (pkt_out : SDN_Types.pktOut) : unit Deferred.t =
    Pipe.write pktout_writer (sw_id, pkt_out)

  let event () : event Deferred.t =
    Pipe.read event_reader
    >>= function
    | `Eof -> assert false
    | `Ok evt -> Deferred.return evt

  let get_table (sw_id : switchId) : (SDN_Types.flow * string list) list =
    NetKAT_LocalCompiler.to_table' sw_id !fdd

  let raw_query (name : string) : (Int64.t * Int64.t) Deferred.t =
    Deferred.List.map ~how:`Parallel
      (Controller.get_switches controller) ~f:(fun sw_id ->
        let pats = List.filter_map (get_table sw_id) ~f:(fun (flow, names) ->
          if List.mem names name then
            Some flow.pattern
          else
            None) in
        Deferred.List.map ~how:`Parallel pats
          ~f:(fun pat ->
            printf "Sending 2...";
            Controller.individual_stats controller sw_id
            >>| function
            | Ok [stat] -> (stat.packet_count, stat.byte_count)
            | Ok _ -> assert false
            | Error _ -> (0L, 0L)))
    >>| fun stats ->
      List.fold (List.concat stats) ~init:(0L, 0L)
        ~f:(fun (pkts, bytes) (pkts', bytes') ->
            Int64.(pkts + pkts', bytes + bytes'))

  let query (name : string) : (Int64.t * Int64.t) Deferred.t =
    raw_query name
    >>= fun (pkts, bytes) ->
    let (pkts', bytes') = Hashtbl.Poly.find_exn stats name in
    Deferred.return (Int64.(pkts + pkts', bytes + bytes'))

  let update_all_switches (pol : policy) : unit Deferred.t =
    let new_queries = NetKAT_Misc.queries_of_policy pol in
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
    fdd := NetKAT_LocalCompiler.compile pol;
    BestEffortUpdate.implement_policy controller !fdd

  let handle_event (evt : Controller.e) : unit Deferred.t =
    List.iter (of_to_netkat_event !fdd evt) ~f:(fun netkat_evt ->
      Pipe.write_without_pushback event_writer netkat_evt);
    match evt with
     | `Connect (sw_id, feats) ->
       printf ~level:`Info "switch %Ld connected" sw_id;
       BestEffortUpdate.bring_up_switch controller sw_id !fdd
     | _ -> Deferred.return ()

  let send_pktout ((sw_id, pktout) : switchId * SDN_Types.pktOut) : unit Deferred.t =
    let pktout0x01 = SDN_OpenFlow0x01.from_packetOut pktout in
    Controller.send_pkt_out controller sw_id pktout0x01
    >>= function
    | Ok () -> Deferred.return ()
    | Error exn -> Deferred.return ()

  let start () : unit =
    let net_reader = Controller.listen controller in
    don't_wait_for (Pipe.iter pol_reader ~f:update_all_switches);
    don't_wait_for (Pipe.iter net_reader ~f:handle_event);
    don't_wait_for (Pipe.iter pktout_reader ~f:send_pktout)



end

