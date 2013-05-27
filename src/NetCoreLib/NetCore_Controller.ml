open Printf
open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Types.External

module Log = Frenetic_Log

(* Internal policy type *)
type pol = Internal.pol

let (<&>) = Lwt.(<&>)

let init_pol : pol = Internal.PoFilter Internal.PrNone

module type MAKE  = functor (Platform : OpenFlow0x01.PLATFORM) -> 
  sig
    val start_controller : policy NetCore_Stream.t -> unit Lwt.t
  end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

  module SwitchSet = Set.Make (Int64)
  module IntMap = Map.Make (struct type t = int let compare = compare end)

  let switches = ref SwitchSet.empty

  (* Per-policy state. *)

  let get_count_handlers : (int, (int * get_count_handler)) Hashtbl.t = 
    Hashtbl.create 200

  let counters : (int, Int64.t) Hashtbl.t =
    Hashtbl.create 200

  let buckets_to_counters : int list IntMap.t ref = ref IntMap.empty
  let counters_to_buckets : int list IntMap.t ref = ref IntMap.empty

  let query_kill_switch = ref (Lwt_switch.create ())

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
    Hashtbl.reset counters;
    query_kill_switch := Lwt_switch.create()

  (* used to initialize newly connected switches and handle packet-in 
     messages *)
  let pol_now : pol ref = ref init_pol

  module Queries = struct

    (* Query runtime state. *)
    let query_xid = ref 2l
    let doing_query = Lwt_mutex.create ()
    let queried_switches = ref SwitchSet.empty
    let callbacks = ref []
    let counts : (switchId * Match.t * int, Int64.t * Int64.t) Hashtbl.t =
      Hashtbl.create 200

    let set_query_state switches =
      queried_switches := switches;
      query_xid := Int32.succ !query_xid;
      Hashtbl.reset counts
  
    let query_switches switches : unit Lwt.t =
      if SwitchSet.is_empty switches then
        Lwt.return ()
      else
        let query_all = 
          let open IndividualFlowRequest in
          StatsRequestMsg 
            (IndividualFlowReq 
              {of_match = Match.all; table_id = 0; port = None}) in
        Lwt_mutex.lock doing_query >>
        let _ = set_query_state switches in
        Lwt_list.iter_p (fun sw -> Platform.send_to_switch sw !query_xid query_all)
          (SwitchSet.elements switches) >>
        Lwt.return ()
  
    let handle_query_reply sw xid rep : unit Lwt.t =
      let open IndividualFlowStats in
      if xid <> !query_xid then
        Log.printf "NetCore_Controller.Queries" "bad query xid (%ld)\n%!" xid
      else
        Hashtbl.add counts 
          (sw, rep.of_match, rep.priority) (rep.packet_count, rep.byte_count);
        queried_switches := SwitchSet.remove sw !queried_switches;
        if SwitchSet.is_empty (SwitchSet.inter !switches !queried_switches) then
          (* TODO(cole): warn if queried switches have disconnected. *)
          let _ = List.iter (fun cb -> cb counts) !callbacks in
          Lwt_mutex.unlock doing_query;
          Lwt.return ()
        else
          Lwt.return ()

    let register_callback 
      (cb : ((switchId * Match.t * int, Int64.t * Int64.t) Hashtbl.t -> unit)) = 
      callbacks := cb :: !callbacks

    let unregister_callback cb = 
      callbacks := List.filter (fun cb' -> cb == cb') !callbacks

  end

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    Log.printf "NetCore_Controller" " compiling new policy for switch %Ld\n%!" sw;
    lwt flow_table = Lwt.wrap2 NetCore_Compiler.flow_table_of_policy sw pol in
    Log.printf "NetCore_Controller" " flow table is:\n%!";
    List.iter
      (fun (m,a) -> Log.printf "NetCore_Controller" " %s => %s\n%!"
        (OpenFlow0x01.Match.to_string m)
        (OpenFlow0x01.Action.sequence_to_string a))
      flow_table;
    Platform.send_to_switch sw 0l delete_all_flows >>
    let prio = ref 65535 in
    Lwt_list.iter_s
      (fun (match_, actions) ->
        try_lwt
          Platform.send_to_switch sw 0l (add_flow !prio match_ actions) >>
          (decr prio; Lwt.return ())
       with exn -> 
         Log.printf "NetCore_Controller" "FAIL %s\n%!" (Printexc.to_string exn);
           raise_lwt exn)
      flow_table >>
    (Log.printf "NetCore_Controller" " initialized switch %Ld\n%!" sw;
     Lwt.return ())

  let install_new_policies sw pol_stream =
    Lwt_stream.iter_p (configure_switch sw)
      (NetCore_Stream.to_stream pol_stream)
      
  let handle_packet_in sw pkt_in = 
    let open Internal in
    let in_port = pkt_in.packetInPort in
    match Packet.parse pkt_in.packetInPacket with
      | None -> 
        let _ = Log.printf "NetCore_Controller" "unparsable packet\n%!" in
        Lwt.return ()
      | Some packet ->
        let inp = Pkt (sw, Internal.Physical in_port, packet,
                       match pkt_in.packetInBufferId with
                         | Some id -> Buf id
                         | None -> Data pkt_in.packetInPacket) in
        let full_action = NetCore_Semantics.eval !pol_now inp in
        let controller_action =
          NetCore_Action.Output.apply_controller full_action
            (sw, Internal.Physical in_port, packet) in
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
        Lwt_list.iter_s (Queries.handle_query_reply sw xid) reps
      | _ -> Lwt.return ()
      in
    handle_switch_messages sw

  let switch_thread
      (sw : switchId)
      (pol_stream : pol NetCore_Stream.t) = 
    switches := SwitchSet.add sw !switches;
    (try_lwt
       install_new_policies sw pol_stream <&> handle_switch_messages sw
     with exn ->
       Log.printf "NetCore_Controller" "%s\n%!" (Printexc.to_string exn);
       Lwt.return ()) >>
     (Log.printf "NetCore_Controller" "thread for switch %Ld terminated.\n" sw;
      switches := SwitchSet.remove sw !switches;
      Lwt.return ())

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    let sw = feats.switch_id in 
    Log.printf "NetCore_Controller" "switch %Ld connected\n%!" sw;
    switch_thread sw pol_stream <&> accept_switches pol_stream

  let add_to_maps bucket counter =
    let upd (map : 'a list IntMap.t ref) (k : int) (v : 'a) : unit =
      let vs = try IntMap.find k !map with Not_found -> [] in
      map := (IntMap.add k (v::vs) !map)
      in
    upd buckets_to_counters bucket counter;
    upd counters_to_buckets counter bucket

  let initialize_query_state pol : unit =
    failwith "NYI: initialize_query_state"

  let spawn_queries pol switch : unit Lwt.t =
    let cb tbl =
      Hashtbl.iter 
        (fun (sw, m, pri) (pc, bc) -> Printf.printf "(%Ld, %s, %d: %s, %s)\n%!" 
          sw (Match.to_string m) pri (Int64.to_string pc) (Int64.to_string bc))
        tbl in
    let () = Queries.register_callback cb in
    let rec loop () : unit Lwt.t =
      if not (Lwt_switch.is_on switch) then
        let _ = Queries.unregister_callback cb in
        Lwt.return ()
      else
        Queries.query_switches !switches >>
        Lwt_unix.sleep 10.0 >>
        loop ()
      in
    loop ()

  let kill_outstanding_queries switch : unit = 
    failwith "NYI: kill_outstanding_queries"

  let accept_policy push_pol pol switch = 
    (* TODO(cole) kill_outstanding_queries !query_kill_switch; *)
    reset_policy_state ();
    let p = NetCore_Desugar.desugar genvlan genbucket get_count_handlers pol in
    Log.printf "[NetCore_Controller.ml]" "got new policy:\n%s\n%!" 
      (Internal.pol_to_string p);
    (* TODO(cole) initialize_query_state p; *)
    pol_now := p;
    push_pol (Some p);
    spawn_queries p switch >>
    Lwt.return ()

  let rec accept_policies push_pol sugared_pol_stream switch =
    lwt pol = Lwt_stream.next sugared_pol_stream in
    Lwt_switch.turn_off switch >>
    let switch' = Lwt_switch.create () in
    accept_policies push_pol sugared_pol_stream switch' <&> 
      accept_policy push_pol pol switch'

  let start_controller pol = 
    let (pol_stream, push_pol) = Lwt_stream.create () in
    try_lwt
      accept_switches (NetCore_Stream.from_stream init_pol pol_stream) <&>  
      accept_policies 
        push_pol (NetCore_Stream.to_stream pol) (Lwt_switch.create ())
    with exn -> 
      Log.printf "NetCore_Controller" "uncaught exception %s\n%!"
        (Printexc.to_string exn);
      Lwt.return ()

end
