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
                  -> IndividualFlowStats.t list
                  -> unit

  val refresh_switches : SwitchSet.t -> t -> unit
  val remove_switch : switchId -> t -> unit

  val find : xid -> t list -> t

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

  let to_string q = Printf.sprintf "{ xid = %d, atom = %s, switches = %s, ...}"
    (Int32.to_int q.xid)
    (match q.atom with
        | SwitchAction _ -> "SwitchAction"
        | ControllerAction _ -> "ControllerAction"
        | ControllerQuery (t, _) -> Printf.sprintf "ControllerQuery (%d,_)" t)
    (Frenetic_Misc.string_of_list (fun sw -> Printf.sprintf "%Ld" sw)
      (SwitchSet.elements !(q.switches)))

  let set_fields tbl pol atom sw =
    let qfields = NetCore_Compiler.query_fields_of_policy pol atom sw in
    let pri = ref 65535 in
    List.iter 
      (fun match_ -> Hashtbl.replace tbl (sw, match_, !pri) (); decr pri)
      qfields

  let kill q = 
    (* let () = Log.printf "NetCore_Controller.Query" "killing (%s)\n%!" (to_string q) in *)
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
    (* let () = Log.printf "NetCore_Controller.Query" "creating (%s)\n%!" (to_string q) in *)
    q

  let reset q =
    (* let () = Log.printf "NetCore_Controller.Query" "resetting (%s)\n%!" (to_string q) in *)
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
    (* let () = Log.printf "NetCore_Controller.Query" "sending (%s)\n%!" (to_string q) in *)
    if SwitchSet.is_empty !(q.switches) then
      (* let () = Log.printf "NetCore_Controller.Query" "... but no switches\n%!" in *)
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
    (* let () = Log.printf "NetCore_Controller.Query" "starting (%s)\n%!" (to_string q) in *)
    let rec loop () =
      if not (is_dead q) then
        send q >>
        Lwt_unix.sleep q.time >>
        loop ()
      else Lwt.return () in
    loop ()

  let handle_single_reply sw q rep =
    (* let () = Log.printf 
      "NetCore_Controller.Query" "handle reply (%s) (%Ld)\n    (%s)\n%!"
      (to_string q) sw (IndividualFlowStats.to_string rep) in *)
    let open IndividualFlowStats in
    if Hashtbl.mem q.counter_ids (sw, rep.of_match, rep.priority) then begin
      q.this_packet_count := Int64.add rep.packet_count !(q.this_packet_count);
      q.this_byte_count := Int64.add rep.byte_count !(q.this_byte_count);
      q.switches_to_respond := SwitchSet.remove sw !(q.switches_to_respond)
      end
    else ()

  let try_done q =
    if SwitchSet.is_empty 
      (SwitchSet.inter !(q.switches_to_respond) !(q.switches))
    then
      (*let () = Log.printf "NetCore_Controller.Query" "got all replies! (%s)\n%!" (to_string q) in *)
      let _ = do_callback q in
      Lwt_mutex.unlock q.lock
    else ()

  let handle_reply sw q reps =
    let () = List.iter (handle_single_reply sw q) reps in
    let () = 
      q.switches_to_respond := 
        SwitchSet.remove sw !(q.switches_to_respond) in
    try_done q

  let refresh_switches switches q =
    let new_switches = SwitchSet.diff switches !(q.switches) in
    q.switches := switches;
    (* SwitchSet.iter 
      (fun s -> Log.printf "NetCore_Controller.Query" "refresh switch (%s) (%Ld) \n%!" (to_string q) s)
      !(q.switches);
    SwitchSet.iter 
      (fun s -> Log.printf "NetCore_Controller.Query" "new switch (%s) (%Ld) \n%!" (to_string q) s)
      new_switches; *)
    SwitchSet.iter (set_fields q.counter_ids q.policy q.atom) new_switches

  let remove_switch sw q =
    q.switches := SwitchSet.remove sw !(q.switches);
    try_done q

  let find xid queries =
    List.find (fun q -> q.xid = xid) queries

end

module type QUERYSET = functor (Platform : OpenFlow0x01.PLATFORM) ->
  sig
    val start : pol -> unit Lwt.t
    val stop : unit -> unit
    val add_switch : switchId -> unit
    val remove_switch : switchId -> unit
    val handle_reply : switchId -> xid -> IndividualFlowStats.t list -> unit
  end

module QuerySet (Platform : OpenFlow0x01.PLATFORM) = struct

  module Q = Query (Platform)
  type qid = xid

  (* The query ID generator is persistent across start/stop
   * cycles, in order to properly ignore old query responses.
   *)
  let query_id_cell = ref Int32.zero
  let gen_query_id () = 
    query_id_cell := Int32.succ !query_id_cell; 
    !query_id_cell

  let queries = ref []
  let switches = ref SwitchSet.empty

  (* Only start and stop access this reference. *)
  let query_switch = ref None

  (* Map query atoms to IDs, which will be used as xid's in stats requests. *)
  let q_actions_to_query_ids : (NetCore_Types.action_atom, qid) Hashtbl.t =
    Hashtbl.create 200

  let reset_state () =
    queries := [];
    query_switch := None;
    Hashtbl.reset q_actions_to_query_ids

  (* Populate q_actions_to_query_ids. *)
  let rec generate_query_ids pol gen_query_id : unit =
    match pol with
    | HandleSwitchEvent _ -> ()
    | PoAction atoms ->
      List.iter (fun atom -> match atom with
        | ControllerQuery (time, cb) -> 
          Hashtbl.replace q_actions_to_query_ids atom (gen_query_id ())
        | _ -> ())
        atoms
    | PoFilter _ -> ()
    | PoUnion (p1, p2)
    | PoSeq (p1, p2)
    | PoITE (_, p1, p2) ->
      generate_query_ids p1 gen_query_id; generate_query_ids p2 gen_query_id

  let make_query pol kill_switch atom qid : unit = 
    if Lwt_switch.is_on kill_switch then
      let open NetCore_Types in
      match atom with
      | ControllerQuery (time, cb) ->
        let float_time = float_of_int time in
        let query = 
          Q.create qid atom float_time cb kill_switch !switches pol in
        queries := query :: !queries
      | _ -> ()
    else ()

  (* Populate and start queries. *)
  let spawn_queries pol kill_switch : unit Lwt.t =
    let _ = generate_query_ids pol in
    Hashtbl.iter (make_query pol kill_switch) q_actions_to_query_ids;
    let rec loop qlist : unit Lwt.t = match qlist with
      | [] -> Lwt.return ()
      | q :: qtail -> Q.start q <&> loop qtail
      in
    loop !queries

  let start pol : unit Lwt.t =
    (* If old queries are still going, stop them. *)
    lwt () = match !query_switch with 
      | Some s -> Lwt_switch.turn_off s 
      | None -> Lwt.return () in
    let switch = Lwt_switch.create () in
    let () = query_switch := Some switch in

    (* Build and start the queries. *)
    let () = generate_query_ids pol gen_query_id in
    spawn_queries pol switch

  let handle_reply sw xid reps = 
    try
      let q = Q.find xid !queries in
      Q.handle_reply sw q reps
    with Not_found -> ()

  let stop () : unit Lwt.t = 
    (* let () = Log.printf "NetCore_Controller.QuerySet" "stopping\n%!" in *)
    lwt () = match !query_switch with
      | Some s -> Lwt_switch.turn_off s
      | None -> Lwt.return () in
    let () = reset_state () in
    Lwt.return ()

  let add_switch sw =
    (* let () = Log.printf "NetCore_Controller.QuerySet" "adding switch (%Ld)\n%!" sw in *)
    switches := SwitchSet.add sw !switches;
    List.iter (Q.refresh_switches !switches) !queries

  let remove_switch sw = 
    (* let () = Log.printf "NetCore_Controller.QuerySet" "removing switch (%Ld)\n%!" sw in *)
    switches := SwitchSet.add sw !switches;
    switches := SwitchSet.remove sw !switches;
    List.iter (Q.remove_switch sw) !queries

end

module type MAKE  = functor (Platform : OpenFlow0x01.PLATFORM) -> 
  sig
    val start_controller : NetCore_Types.pol NetCore_Stream.t -> unit Lwt.t
  end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

  module Queries = QuerySet(Platform)

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
        Queries.handle_reply sw xid reps;
        Lwt.return ()
      | (xid, StatsReplyMsg r) ->
        let _ = Log.printf "NetCore_Controller" 
          "received unexpected stats reply type (%s)"
          (OpenFlow0x01.string_of_statsReply r) in
          Lwt.return ()
      | (xid, PortStatusMsg msg) ->
	let _ = Log.printf "NetCore_Controller" "received %s" (OpenFlow0x01_Parser.PortStatus.to_string msg) in
	Lwt.return ()
      | _ -> Lwt.return ()
      in
    handle_switch_messages sw

  let switch_thread features pol_stream =
    let sw = features.switch_id in
    Queries.add_switch sw;
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
      Lwt.async
        (fun () -> (* TODO(arjun): 
                      confirm this gracefully discards the exception *)
          Lwt.wrap2 NetCore_Semantics.handle_switch_events 
            (SwitchDown sw)
            (NetCore_Stream.now pol_stream));
      Queries.remove_switch sw;
      Lwt.return ()
    end

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    Lwt.async (fun () -> switch_thread feats pol_stream);
    accept_switches pol_stream

  let rec accept_policies push_pol sugared_pol_stream =
    lwt pol = Lwt_stream.next sugared_pol_stream in
    Queries.stop () >>
    let _ = pol_now := pol in
    let _ = push_pol (Some pol) in
    accept_policies push_pol sugared_pol_stream <&> Queries.start pol

  (** emits packets synthesized at the controller. Produced packets are
      subject to the current NetCore policy. *)
  let emit_packets pkt_stream pol_stream = 
    let emit_pkt (sw, pt, bytes) = match Packet.parse bytes with
      | None -> 
        Log.printf "NetCore_Controller" "cannot pars synth packet\n%!";
        Lwt.return ()
      | Some pkt -> 
        let actions = NetCore_Semantics.eval
          (NetCore_Stream.now pol_stream) (* TODO(arjun): glitch? *)
          (Pkt (sw, Physical pt, pkt, Packet bytes)) in
        let msg = {
          pktOutBufOrBytes = Packet bytes;
          pktOutPortId = None;
          pktOutActions = NetCore_Action.Output.as_actionSequence None actions
        } in
        Platform.send_to_switch sw 0l (PacketOutMsg msg) in
    Lwt_stream.iter_s emit_pkt pkt_stream

  let start_controller pkt_stream pol = 
    let (pol_stream, push_pol) = Lwt_stream.create () in
    let (stream_lwt, pol_netcore_stream) =
      NetCore_Stream.from_stream init_pol pol_stream in
    Lwt.pick
      [ accept_switches pol_netcore_stream;
        emit_packets pkt_stream pol_netcore_stream;
        accept_policies
          push_pol (NetCore_Stream.to_stream pol);
        stream_lwt ]

end
