open Printf
open Packet
open OpenFlow0x01
open NetCore_Types

module Log = Frenetic_Log
module SwitchSet = Set.Make (Int64)

let (<&>) = Lwt.(<&>)

let init_pol : pol = PoFilter PrNone

(* Internal module for managing individual queries. *)
module type QUERY = functor (Platform : OpenFlow0x01.PLATFORM) -> sig
 
  type t

  val create : xid
            -> action_atom
            -> int (* Time to wait between queries. *)
            -> get_count_handler
            -> Lwt_switch.t 
            -> SwitchSet.t 
            -> pol 
            -> t

  val kill : t -> unit Lwt.t
  val start : t -> unit Lwt.t
  val handle_reply : switchId 
                  -> t
                  -> IndividualFlowStats.t 
                  -> unit

  val refresh_switches : SwitchSet.t -> t -> unit
  val remove_switch : switchId -> t -> unit

end

module Query (Platform : OpenFlow0x01.PLATFORM) = struct

  (* switch id, match, priority *)
  type counter_ids = (switchId * Match.t * int, unit) Hashtbl.t

  type t = 
    { (* Static. *)
      xid : xid
    ; atom : NetCore_Types.action_atom
    ; time : float
    ; cb : get_count_handler
    ; policy : pol
      (* Mutable. *)
    ; counter_ids : counter_ids
    ; lock : Lwt_mutex.t
    ; kill_switch : Lwt_switch.t
    ; switches : SwitchSet.t ref
    ; switches_to_respond : SwitchSet.t ref
    ; last_packet_count : Int64.t ref
    ; this_packet_count : Int64.t ref
    ; last_byte_count : Int64.t ref
    ; this_byte_count : Int64.t ref
    }

  let to_string q = Printf.sprintf "{ xid = %d, atom = %s, time = %f, ...}"
    (Int32.to_int q.xid)
    (match q.atom with
        | SwitchAction _ -> "SwitchAction"
        | ControllerAction _ -> "ControllerAction"
        | ControllerQuery (t, _) -> Printf.sprintf "ControllerQuery (%d,_)" t)
    q.time

  let set_fields tbl pol atom sw =
    let qfields = NetCore_Compiler.query_fields_of_policy pol atom sw in
    let pri = ref 65535 in
    List.iter 
      (fun match_ -> Hashtbl.replace tbl (sw, match_, !pri) (); decr pri)
      qfields

  let kill q = 
    Lwt_switch.turn_off q.kill_switch >>
    let _ = Lwt_mutex.unlock q.lock in
    Lwt.return ()

  let is_dead q = not (Lwt_switch.is_on q.kill_switch)

  let create xid atom time cb kill_switch switches policy =
    let counter_ids = Hashtbl.create 200 in
    let _ = SwitchSet.iter (set_fields counter_ids policy atom) switches in
    let q = 
      { xid = xid
      ; atom = atom
      ; time = time
      ; cb = cb
      ; policy = policy 
      ; counter_ids = counter_ids
      ; lock = Lwt_mutex.create ()
      ; kill_switch = kill_switch
      ; switches = ref switches 
      ; switches_to_respond = ref switches
      ; last_packet_count = ref Int64.zero 
      ; this_packet_count = ref Int64.zero 
      ; last_byte_count = ref Int64.zero 
      ; this_byte_count = ref Int64.zero 
      } in
    let _ = try
      Lwt_switch.add_hook (Some kill_switch) (fun () -> kill q)
    with Lwt_switch.Off -> () in
    q

  let reset q =
    q.switches_to_respond := !(q.switches);
    q.last_packet_count := !(q.this_packet_count);
    q.this_packet_count := Int64.zero;
    q.last_byte_count := !(q.this_byte_count);
    q.this_byte_count := Int64.zero

  let do_callback q =
    let packet_count = 
      Int64.sub !(q.this_packet_count) !(q.last_packet_count) in
    let byte_count = 
      Int64.sub !(q.this_byte_count) !(q.last_byte_count) in
    q.cb packet_count byte_count

  let send q =
    if SwitchSet.is_empty !(q.switches) then
      let () = do_callback q in
      Lwt.return ()
    else
      let query_msg =
        let open IndividualFlowRequest in
        StatsRequestMsg 
          (IndividualFlowReq 
          {of_match = Match.all; table_id = 0; port = None}) in
      Lwt_mutex.lock q.lock >>
      if not (is_dead q) then
        let _ = reset q in
        Lwt_list.iter_p (fun sw -> Platform.send_to_switch sw q.xid query_msg)
          (SwitchSet.elements !(q.switches))
      else
        Lwt.return ()

  let start q =
    let rec loop () =
      if not (is_dead q) then
        send q >>
        Lwt_unix.sleep q.time >>
        loop ()
      else Lwt.return () in
    loop ()

  let handle_reply sw q rep =
    let open IndividualFlowStats in
    if Hashtbl.mem q.counter_ids (sw, rep.of_match, rep.priority) then
      let _ = q.this_packet_count := Int64.add rep.packet_count !(q.this_packet_count) in
      let _ = q.this_byte_count := Int64.add rep.byte_count !(q.this_byte_count) in
      let _ = q.switches_to_respond := SwitchSet.remove sw !(q.switches_to_respond) in
      if SwitchSet.is_empty (SwitchSet.inter !(q.switches_to_respond) !(q.switches)) then
        let _ = do_callback q in
        Lwt_mutex.unlock q.lock
      else ()
    else ()

  let refresh_switches switches q =
    let new_switches = SwitchSet.diff switches !(q.switches) in
    q.switches := switches;
    SwitchSet.iter (set_fields q.counter_ids q.policy q.atom) new_switches

  let remove_switch sw q =
    q.switches := SwitchSet.diff !(q.switches) (SwitchSet.singleton sw)

end

module type MAKE  = functor (Platform : OpenFlow0x01.PLATFORM) -> 
  sig
    val start_controller : NetCore_Types.pol NetCore_Stream.t -> unit Lwt.t
  end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

  module Q = Query(Platform)

  let switches = ref SwitchSet.empty

  (* Per-policy state. *)

  let queries = ref []
  let query_ids : (NetCore_Types.action_atom, int) Hashtbl.t =
    Hashtbl.create 200
  let get_count_handlers : (int, (int * get_count_handler * bool)) Hashtbl.t = 
    Hashtbl.create 200

  let bucket_cell = ref 0 
  let vlan_cell = ref 0 

  let genbucket () = 
    incr bucket_cell;
    !bucket_cell

  let genvlan () = 
    incr vlan_cell;
    Some !vlan_cell

  let reset_policy_state () =
    bucket_cell := 0;
    vlan_cell := 0;
    Hashtbl.reset get_count_handlers;
    Hashtbl.reset query_ids;
    queries := []

  (* used to initialize newly connected switches and handle packet-in 
     messages *)
  let pol_now : pol ref = ref init_pol

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    lwt flow_table = Lwt.wrap2 NetCore_Compiler.flow_table_of_policy sw pol in
    Platform.send_to_switch sw 0l delete_all_flows >>
    let prio = ref 65535 in
    Lwt_list.iter_s
      (fun (match_, actions) ->
          Platform.send_to_switch sw 0l (add_flow !prio match_ actions) >>
          (decr prio; Lwt.return ()))
      flow_table

  let install_new_policies sw pol_stream =
    Lwt_stream.iter_p (configure_switch sw)
      (NetCore_Stream.to_stream pol_stream)
      
  let handle_packet_in sw pkt_in = 
    let in_port = pkt_in.packetInPort in
    match Packet.parse pkt_in.packetInPacket with
      | None -> 
        let _ = Log.printf "NetCore_Controller" "unparsable packet\n%!" in
        Lwt.return ()
      | Some packet ->
        let inp = Pkt (sw, Physical in_port, packet,
                       match pkt_in.packetInBufferId with
                         | Some id -> Buffer id
                         | None -> Packet pkt_in.packetInPacket) in
        let full_action = NetCore_Semantics.eval !pol_now inp in
        let controller_action =
          NetCore_Action.Output.apply_controller full_action
            (sw, Physical in_port, packet) in
        let action = match pkt_in.packetInReason with
          | ExplicitSend -> controller_action
          | NoMatch -> NetCore_Action.Output.par_action controller_action
            (NetCore_Action.Output.switch_part full_action) in
        let outp = { 
          pktOutBufOrBytes = begin match pkt_in.packetInBufferId with
            | Some id -> Buffer id
            | None -> Packet pkt_in.packetInPacket
          end;
          pktOutPortId = None;
          pktOutActions = 
            NetCore_Action.Output.as_actionSequence (Some in_port) action 
        } in
        Platform.send_to_switch sw 0l (PacketOutMsg outp)  

  let rec handle_switch_messages sw = 
    lwt v = Platform.recv_from_switch sw in
    lwt _ = match v with
      | (_, PacketInMsg pktIn) -> handle_packet_in sw pktIn
      | (xid, StatsReplyMsg (IndividualFlowRep reps)) -> 
        let open Q in
        let q = List.find (fun q -> q.xid = xid) !queries in
        let () = List.iter (Q.handle_reply sw q) reps in
        Lwt.return ()
      | (xid, StatsReplyMsg r) ->
        let _ = Log.printf "NetCore_Controller" "received unexpected stats reply type (%s)"
          (OpenFlow0x01.string_of_statsReply r) in
          Lwt.return ()
      | _ -> Lwt.return ()
      in
    handle_switch_messages sw

  let switch_thread features pol_stream =
    let sw = features.switch_id in
    switches := SwitchSet.add sw !switches;
    List.iter (Q.refresh_switches !switches) !queries;
    (try_lwt
      lwt _ = Lwt.wrap2 NetCore_Semantics.handle_switch_events
        (SwitchUp (sw, features))
        (NetCore_Stream.now pol_stream) in
      Lwt.pick [ install_new_policies sw pol_stream;
                 handle_switch_messages sw ]
    with exn ->
      begin
        match exn with
          | OpenFlow0x01_Platform.SwitchDisconnected _ ->
            (* TODO(arjun): I can assume sw itself disconnected? *)
            Lwt.return ()
          | _ ->
            (Log.printf "[NetCore_Controller]" "unhandled exception %s.\n%!"
              (Printexc.to_string exn);
             Lwt.return ())
      end) >>
    begin
      switches := SwitchSet.remove sw !switches;
      Lwt.async
        (fun () -> (* TODO(arjun): 
                      confirm this gracefully discards the exception *)
          Lwt.wrap2 NetCore_Semantics.handle_switch_events 
            (SwitchDown sw)
            (NetCore_Stream.now pol_stream));
      List.iter (Q.remove_switch sw) !queries;
      Lwt.return ()
    end

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    Lwt.async (fun () -> switch_thread feats pol_stream);
    accept_switches pol_stream

  let rec extract_query_ids pol tbl = 
    match pol with
    | HandleSwitchEvent _ -> ()
    | PoAction atoms ->
      List.iter (fun atom -> match atom with
        | ControllerQuery (time, cb) -> Hashtbl.replace tbl atom (genbucket ())
        | _ -> ())
        atoms
    | PoFilter _ -> ()
    | PoUnion (p1, p2)
    | PoSeq (p1, p2)
    | PoITE (_, p1, p2) ->
      extract_query_ids p1 tbl; extract_query_ids p2 tbl

  let make_query pol kill_switch atom qid : unit = 
    let open NetCore_Types in
    match atom with
    | ControllerQuery (time, cb) ->
      let xid = Int32.of_int qid in
      let float_time = float_of_int time in
      let query = Q.create xid atom float_time cb kill_switch !switches pol in
      queries := query :: !queries
    | _ -> ()

  let spawn_queries pol kill_switch : unit Lwt.t =
    let _ = extract_query_ids pol query_ids in
    Hashtbl.iter (make_query pol kill_switch) query_ids;
    let rec loop qlist : unit Lwt.t = match qlist with
      | [] -> Lwt.return ()
      | q :: qtail -> Q.start q <&> loop qtail
      in
    loop !queries

  let kill_outstanding_queries switch : unit Lwt.t = 
    Lwt_switch.turn_off switch

  let rec accept_policies push_pol sugared_pol_stream kill_switch =
    lwt pol = Lwt_stream.next sugared_pol_stream in
    kill_outstanding_queries kill_switch >>
    let _ = reset_policy_state () in
    let _ = pol_now := pol in
    let _ = push_pol (Some pol) in
    let new_kill_switch = Lwt_switch.create () in
    accept_policies push_pol sugared_pol_stream new_kill_switch
      <&> spawn_queries pol new_kill_switch

  let start_controller pol = 
    let (pol_stream, push_pol) = Lwt_stream.create () in
    let (stream_lwt, pol_netcore_stream) =
      NetCore_Stream.from_stream init_pol pol_stream in
    Lwt.pick
      [ accept_switches pol_netcore_stream;
        accept_policies
          push_pol (NetCore_Stream.to_stream pol) (Lwt_switch.create ());
        stream_lwt ]

end
