open Printf
open Packet
open OpenFlow0x01
open OpenFlow0x01_Core
open NetCore_Types

module Log = Frenetic_Log
module SwitchSet = Set.Make (Int64)
module Stats = OpenFlow0x01_Stats

let (<&>) = Lwt.(<&>)

let init_pol : pol = Filter Nothing

module type PLATFORM = OpenFlow0x01_PlatformSig.PLATFORM

(* Internal module for managing individual queries. *)
module type QUERY = functor (Platform : PLATFORM) -> sig
 
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
                  -> Stats.individualStats list
                  -> unit

  val refresh_switches : SwitchSet.t -> t -> unit
  val remove_switch : switchId -> t -> unit

  val find : xid -> t list -> t

end

module Query (Platform : PLATFORM) = struct

  module Flow = struct
    type t = (switchId * Match.t)
    let compare = compare
  end

  module FlowSet = Set.Make (Flow)

  (* switch id, match, priority *)
  type counter_ids = FlowSet.t ref

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
        | ControllerQuery (t, _) -> Printf.sprintf "ControllerQuery (%f,_)" t)
    (Frenetic_Misc.string_of_list (fun sw -> Printf.sprintf "%Ld" sw)
      (SwitchSet.elements !(q.switches)))

  let set_fields tbl pol atom sw =
    let qfields = NetCore_Compiler.query_fields_of_policy pol atom sw in
    List.iter 
      (fun match_ -> tbl := FlowSet.add (sw, match_) !tbl)
      qfields

  let kill q = 
    (* let () = Log.printf "NetCore_Controller.Query" "killing (%s)\n%!" (to_string q) in *)
    Lwt_switch.turn_off q.kill_switch >>
    let _ = Lwt_mutex.unlock q.lock in
    Lwt.return ()

  let is_dead q = not (Lwt_switch.is_on q.kill_switch)

  let create xid atom time cb kill_switch switches policy =
    let counter_ids = ref FlowSet.empty in
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
        Message.StatsRequestMsg 
          (Stats.IndividualRequest (match_all, 0, None)) in
      Lwt_mutex.lock q.lock >>
      if not (is_dead q) then
        let _ = reset q in
        Lwt_list.iter_s (fun sw -> Platform.send_to_switch sw q.xid query_msg)
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
      (to_string q) sw (Stats.IndividualFlowStats.to_string rep) in *)
    let open Stats in
    if FlowSet.mem (sw, rep.of_match) !(q.counter_ids) then begin
      q.this_packet_count := Int64.add rep.packet_count !(q.this_packet_count);
      q.this_byte_count := Int64.add rep.byte_count !(q.this_byte_count);
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

module type QUERYSET = functor (Platform : PLATFORM) ->
  sig
    val start : pol -> unit Lwt.t
    val stop : unit -> unit
    val add_switch : switchId -> unit
    val remove_switch : switchId -> unit
    val handle_reply : switchId 
                    -> xid 
                    -> Stats.individualStats list 
                    -> unit
  end

module QuerySet (Platform : PLATFORM) = struct

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
    | Action atoms ->
      List.iter (fun atom -> match atom with
        | ControllerQuery (_, cb) -> 
          Hashtbl.replace q_actions_to_query_ids atom (gen_query_id ())
        | _ -> ())
        atoms
    | Filter _ -> ()
    | Union (p1, p2)
    | Seq (p1, p2)
    | ITE (_, p1, p2) ->
      generate_query_ids p1 gen_query_id; generate_query_ids p2 gen_query_id

  let make_query pol kill_switch atom qid : unit = 
    if Lwt_switch.is_on kill_switch then
      let open NetCore_Types in
      match atom with
      | ControllerQuery (time, cb) ->
        let query = 
          Q.create qid atom time cb kill_switch !switches pol in
        queries := query :: !queries
      | _ -> ()
    else ()

  (* Start queries. If any query fails, we stop all queries. *)
  let spawn_queries pol kill_switch : unit Lwt.t =
    let _ = generate_query_ids pol in
    Hashtbl.iter (make_query pol kill_switch) q_actions_to_query_ids;
    Lwt.pick (List.map Q.start !queries)

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

module type MAKE  = functor (Platform : PLATFORM) -> 
  sig
    val start_controller : NetCore_Types.pol NetCore_Stream.t -> unit Lwt.t
  end

module Make (Platform : PLATFORM) = struct

  module Queries = QuerySet (Platform)

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    lwt flow_table = Lwt.wrap2 NetCore_Compiler.flow_table_of_policy sw pol in
    Platform.send_to_switch sw 0l (Message.FlowModMsg delete_all_flows) >>
    let prio = ref 65535 in
    Lwt_list.iter_s
      (fun (match_, actions) ->
        Platform.send_to_switch sw 0l 
          (Message.FlowModMsg (add_flow !prio match_ actions)) >>
        (decr prio; Lwt.return ()))
      flow_table

  let install_new_policies sw pol_stream =
    Lwt_stream.iter_s (configure_switch sw)
      (NetCore_Stream.to_stream pol_stream)
      
  let handle_packet_in pol sw pkt_in = 
    let in_port = pkt_in.port in
    try_lwt
      let pol = NetCore_Stream.now pol in
      let in_packet = parse_payload pkt_in.input_payload in
      let in_val = 
        Pkt (sw, Physical in_port, in_packet, pkt_in.input_payload) in

      (* Evaluate the packet against the full policy. *)
      let policy_out_vals = NetCore_Semantics.eval pol in_val in

      (* Evaluate the packet against the flow table on the switch. *)
      let classifier = NetCore_Compiler.compile_pol pol sw in
      let switch_action =
        NetCore_Action.Output.switch_part
          (NetCore_Compiler.OutputClassifier.scan 
            classifier (Physical in_port) in_packet) in
      let classifier_out_vals = 
        NetCore_Semantics.eval_action switch_action in_val in

    (* These are packets not already processed in the data plane. *)
      let new_out_vals =
        List.filter 
          (fun v -> not (List.mem v classifier_out_vals)) 
          policy_out_vals in

      (* Calculate the action required to transform the incoming packet
       * into each of the new outgoing packets. *)
      let action = 
        List.fold_left 
          NetCore_Action.Output.par_action
          NetCore_Action.Output.drop
          (List.map (NetCore_Action.Output.make_transformer in_val)
            new_out_vals) in

      let out_payload = 
        { output_payload = pkt_in.input_payload
        ; port_id = None
        ; apply_actions = 
            NetCore_Action.Output.as_actionSequence (Some in_port) action
        } in
      Platform.send_to_switch sw 0l (Message.PacketOutMsg out_payload)  
    with Unparsable _ -> 
      begin
        let _ = Log.printf "NetCore_Controller" "unparsable packet\n%!" in
        Lwt.return ()
      end

  let rec handle_switch_messages pol sw = 
    let open Message in
    lwt (xid, msg) = Platform.recv_from_switch sw in
    lwt _ = match msg with
      | PacketInMsg pktIn -> handle_packet_in pol sw pktIn
      | StatsReplyMsg (Stats.IndividualFlowRep reps) -> 
        Queries.handle_reply sw xid reps;
        Lwt.return ()
      | StatsReplyMsg r ->
        let _ = Log.printf "NetCore_Controller" 
          "received unexpected stats reply type (%s)"
          (Stats.reply_to_string r) in
          Lwt.return ()
      | PortStatusMsg msg ->
        (* TODO(arjun): this should be used by NetCore_Topo *)
        Lwt.return ()
      | _ -> Lwt.return () in
    handle_switch_messages pol sw

  let switch_thread features pol_stream =
    let open SwitchFeatures in
    let sw = features.switch_id in
    Log.printf "NetCore Controller" "switch %Ld connected.\n%!"  sw;
    Queries.add_switch sw;
    (try_lwt
      lwt _ = Lwt.wrap2 NetCore_Semantics.handle_switch_events
        (SwitchUp (sw, features))
        (NetCore_Stream.now pol_stream) in
      Lwt.pick [ install_new_policies sw pol_stream;
                 handle_switch_messages pol_stream sw ]
    with exn ->
      begin
        match exn with
          | OpenFlow0x01_Platform.SwitchDisconnected _ ->
            (* TODO(arjun): I can assume sw itself disconnected? *)
            Lwt.return ()
          | _ ->
            (Log.printf "NetCore Controller" "unhandled exception %s.\n%!"
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
      Log.printf "NetCore Controller" "switch %Ld disconnected.\n%!"  sw;
      Lwt.return ()
    end

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    (* An exception raised by a switch thread does not kill the controller. *)
    Lwt.async (fun () -> switch_thread feats pol_stream);
    accept_switches pol_stream

  let rec setup_queries pol_stream = 
    lwt pol = Lwt_stream.next pol_stream in
    Queries.stop () >>
    Queries.start pol <&> setup_queries pol_stream

  (** Emits packets synthesized at the controller. Produced packets
      are _not_ subjected to the current NetCore policy, so they do not
      compose nicely with NetCore operatores. This requires some deep thought,
      which is banned until after PLDI 2013. *)
  let emit_packets pkt_stream = 
    let open PacketOut in
    let emit_pkt (sw, pt, bytes) =
      let msg = {
        output_payload = NotBuffered bytes;
        port_id = None;
        apply_actions = [Output (PhysicalPort pt)]
      } in
      Platform.send_to_switch sw 0l (Message.PacketOutMsg msg) in
    Lwt_stream.iter_s emit_pkt pkt_stream

  let start_controller pkt_stream pol = 
    (* Do not delete the following printf. It is meant to be the minimal output
       from the controller. *)
    Log.printf "NetCore" "controller started.";
    Lwt.pick [
      accept_switches pol;
      emit_packets pkt_stream;
      setup_queries (NetCore_Stream.to_stream pol)
    ]

end

module MakeConsistent (Platform : PLATFORM) = struct

  open NetCore_ConsistentUpdates
  module Queries = QuerySet(Platform)
  module Topo = NetCore_Topo.Topo 

  (* used to initialize newly connected switches and handle packet-in 
     messages *)
  let pol_now : pol ref = ref init_pol

  (** Emits packets synthesized at the controller. Produced packets
      are _not_ subjected to the current NetCore policy, so they do not
      compose nicely with NetCore operatores. This requires some deep thought,
      which is banned until after PLDI 2013. *)
  let emit_pkt (sw, pt, bytes) =
    let open PacketOut in
	let msg = {
          output_payload = NotBuffered bytes;
          port_id = None;
          apply_actions = [Output (PhysicalPort pt)]
	} in
	Platform.send_to_switch sw 0l (Message.PacketOutMsg msg)

  let emit_packets pkt_stream = 
    Lwt_stream.iter_s emit_pkt pkt_stream

  let (topo_pol_stream, discovery_lwt) = Topo.create (fun x -> emit_pkt x)
    (* (fun lp ->  *)
    (*   Log.printf "NetCore_Controller" *)
    (*     "TODO(arjun): give me a function to emit packets") *)
  
  let topo_pol = NetCore_Stream.now topo_pol_stream

  let clear_switch (sw : switchId) : unit Lwt.t =
    Platform.send_to_switch sw 0l (Message.FlowModMsg delete_all_flows)

  let rec pop_last lst = match lst with
    | [] -> failwith "pop_last must be called w/ non-empty list"
    | [a] -> [],a
    | a :: lst -> let lst',last = pop_last lst in
                  a::lst',last

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    Log.printf "ConsistentController" "In configure_squence\n%!";
    (* Flow table always emits default drop rule. Installing policies
       in sequence w/o clearing results in multiple such rules, possibly
       messing up semantics w/ they overlap/shadow real rules. Instead,
       default drop rule should be installed at bottom priority *)
    lwt flow_table, drop_rule = 
      Lwt.wrap1 pop_last (NetCore_Compiler.flow_table_of_policy sw pol) in
    let prio = ref 65535 in
    Lwt_list.iter_s
      (fun (match_, actions) ->
        printf " %s => %s\n%!"
          (OpenFlow0x01.Match.to_string match_)
          (OpenFlow0x01.Action.sequence_to_string actions);
        Platform.send_to_switch sw 0l 
          (Message.FlowModMsg (add_flow !prio match_ actions)) >>
          (decr prio; Lwt.return ()))
      flow_table >>
      Platform.send_to_switch sw 0l 
        (Message.FlowModMsg (add_flow 1 (fst drop_rule) (snd drop_rule)))


  (* First draft: ignore barriers *)
  let install_new_policies sw pol_stream =
    Lwt_stream.iter_s (fun (int, ext, topo_pol) -> 
      clear_switch sw >>
      configure_switch sw topo_pol >>
      let _ = 
        Log.printf "NetCore_Controller" "internal pol:\n%s\n%!" 
          (NetCore_Pretty.string_of_pol int) in
      configure_switch sw int >>
      let _ = Log.printf "NetCore_Controller" "external pol:\n%s\n%!" 
        (NetCore_Pretty.string_of_pol ext) in
      configure_switch sw (Union(int,ext)))
      (NetCore_Stream.to_stream pol_stream)

  let handle_packet_in sw pkt_in = 
    failwith "NYI: Consistent updates handle_packet_in."
(*
    let in_port = pkt_in.port in
    let in_packet = parse_payload pkt_in.input_payload in
    let in_val = Pkt (sw, Physical in_port, in_packet, pkt_in.input_payload) in
    let action = NetCore_Semantics.eval !pol_now in_val in
    let out_packet = 
      { output_payload = pkt_in.input_payload
      ; port_id = None
      ; apply_actions = 
         NetCore_Action.Output.as_actionSequence (Some in_port) action 
      } in
    Platform.send_to_switch sw 0l (Message.PacketOutMsg out_packet)
*)

  let rec handle_switch_messages sw = 
    let open Message in
    lwt v = Platform.recv_from_switch sw in
    lwt _ = match v with
      | (_, PacketInMsg pkt_in) -> handle_packet_in sw pkt_in
      | (xid, StatsReplyMsg (Stats.IndividualFlowRep reps)) -> 
        Queries.handle_reply sw xid reps;
        Lwt.return ()
      | (xid, StatsReplyMsg r) ->
        let _ = Log.printf "NetCore_Controller" 
          "received unexpected stats reply type (%s)"
          (Stats.reply_to_string r) in
          Lwt.return ()
      | (xid, PortStatusMsg msg) ->
        let _ = 
      Log.printf "NetCore_Controller" 
        "received %s" (PortStatus.to_string msg) in
        Lwt.return ()
      | _ -> Lwt.return ()
      in
    handle_switch_messages sw

  let switch_thread features pol_stream =
    let open SwitchFeatures in
    let sw = features.switch_id in
    Queries.add_switch sw;
    (try_lwt
       let (int_pol,ext_pol,topo_pol) = (NetCore_Stream.now pol_stream) in
      lwt _ = Lwt.wrap2 NetCore_Semantics.handle_switch_events
        (SwitchUp (sw, features)) (Union (Union(int_pol,ext_pol), topo_pol))
         in
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
          let (int_pol,ext_pol,topo_pol) = (NetCore_Stream.now pol_stream) in
          Lwt.wrap2 NetCore_Semantics.handle_switch_events 
            (SwitchDown sw)
            (Union (Union(int_pol,ext_pol), topo_pol)));
      Queries.remove_switch sw;
      Lwt.return ()
    end

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    Lwt.async (fun () -> switch_thread feats pol_stream);
    accept_switches pol_stream

  let make_extPorts sw =
    List.fold_left (fun acc (pId,loc) -> 
      match loc with
              | NetCore_Topo.Switch _ -> acc
              | _ -> pId :: acc)
      (Topo.ports_of_switch  sw) []

  module GenSym = (* TODO(arjun): consider NetCore_Gensym *)
  struct
    let create () = ref 0
    let next_val g =  incr g; !g
  end

  let rec accept_policies push_pol sugared_pol_stream genSym =
    lwt pol = Lwt_stream.next sugared_pol_stream in
    let ver = GenSym.next_val genSym in
    let (int_pol,ext_pol) = gen_update_pols pol ver (Topo.get_switches ()) make_extPorts in
    Queries.stop () >>
    let _ = pol_now := Union(Union(int_pol, ext_pol), topo_pol) in
    let _ = push_pol (Some (int_pol, ext_pol, topo_pol)) in
    accept_policies push_pol sugared_pol_stream genSym <&> Queries.start (Union(pol, topo_pol))

  let start_controller pkt_stream pol = 
    let (pol_stream, push_pol) = Lwt_stream.create () in
    let genSym = GenSym.create () in
    let (stream_lwt, pol_netcore_stream) =
      NetCore_Stream.from_stream (init_pol, init_pol, topo_pol) pol_stream in
    Lwt.pick
      [ accept_switches pol_netcore_stream;
        emit_packets pkt_stream;
        accept_policies
          push_pol (NetCore_Stream.to_stream pol) genSym;
        stream_lwt;
        discovery_lwt ]

end
