open Printf
open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Types.External


module Log = Frenetic_Log
module SwitchSet = Set.Make (Int64)

(* Internal policy type *)
type pol = Internal.pol

let (<&>) = Lwt.(<&>)

let init_pol : pol = Internal.PoFilter Internal.PrNone

(* Internal module for managing individual queries. *)
module Query (Platform : OpenFlow0x01.PLATFORM) = struct

  type counters = (switchId * Match.t * int, Int64.t * Int64.t) Hashtbl.t

  type t = {
    xid : xid;
    cb : counters -> unit;
    counters : counters;
    lock : Lwt_mutex.t;
    kill_switch : Lwt_switch.t;
    switches : SwitchSet.t ref
  }

  let create xid cb kill_switch switches =
    { xid = xid
    ; cb = cb
    ; counters = Hashtbl.create 200
    ; lock = Lwt_mutex.create ()
    ; kill_switch = kill_switch
    ; switches = switches }

  let kill q = Lwt_switch.turn_off q.kill_switch
  let is_dead q = Lwt_switch.is_on q.kill_switch

  let reset q switches =
    Hashtbl.reset q.counters;
    q.switches := switches

  let start q switches =
    let query_msg =
      let open IndividualFlowRequest in
      StatsRequestMsg 
        (IndividualFlowReq 
        {of_match = Match.all; table_id = 0; port = None}) in
    Lwt_mutex.lock q.lock >>
    if not (is_dead q) then
      let _ = reset q switches in
      Lwt_list.iter_p (fun sw -> Platform.send_to_switch sw q.xid query_msg)
        (SwitchSet.elements !(q.switches))
    else
      Lwt.return ()

  let handle_reply q sw rep current_switches =
    let open IndividualFlowStats in
    Hashtbl.replace q.counters
      (sw, rep.of_match, rep.priority) (rep.packet_count, rep.byte_count);
    q.switches := SwitchSet.remove sw !(q.switches);
    if SwitchSet.is_empty (SwitchSet.inter !(q.switches) !current_switches) then
      (q.cb q.counters;
      Lwt_mutex.unlock q.lock)
    else
      ()

end

module type MAKE  = functor (Platform : OpenFlow0x01.PLATFORM) -> 
  sig
    val start_controller : NetCore_Types.Internal.pol NetCore_Stream.t -> unit Lwt.t
  end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

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

(*

  module Queries = struct

    (* Query runtime state. *)
    let query_xid = ref 2l
    let query_locks = Hashtbl.create 200
    let queried_switches = ref SwitchSet.empty
    let callbacks = Hashtbl.create 200
    let counts : (int32, (switchId * Match.t * int, Int64.t * Int64.t) Hashtbl.t) Hashtbl.t =
      Hashtbl.create 200

    let reset_query_state switches =
      queried_switches := switches;
      Hashtbl.reset counts;
      Hashtbl.reset callbacks
      (* TODO(cole): free locks/kill threads *)

    let get_lock xid =
      try
        Lwt_mutex.lock (Hashtbl.find query_locks xid)
      with Not_found ->
        let lock = Lwt_mutex.create () in
        Hashtbl.add query_locks xid lock;
        Lwt_mutex.lock lock

    let release_lock xid =
      try
        Lwt_mutex.unlock (Hashtbl.find query_locks xid)
      with Not_found ->
        ()
  
    let query_switches xid switches : unit Lwt.t =
      if SwitchSet.is_empty switches then
        Lwt.return ()
      else
        let query_all = 
          let open IndividualFlowRequest in
          StatsRequestMsg 
            (IndividualFlowReq 
              {of_match = Match.all; table_id = 0; port = None}) in
        get_lock xid >>
        let _ =
          try
            let tbl = Hashtbl.find counts xid in
            Hashtbl.reset tbl
          with Not_found ->
            Hashtbl.add counts xid (Hashtbl.create 200)
          in
        Lwt_list.iter_p (fun sw -> Platform.send_to_switch sw xid query_all)
          (SwitchSet.elements switches) >>
        Lwt.return ()
  
    let handle_query_reply sw xid rep : unit Lwt.t =
      let open IndividualFlowStats in
      try
        let count = Hashtbl.find counts xid in
        Hashtbl.replace count
          (sw, rep.of_match, rep.priority) (rep.packet_count, rep.byte_count);
        queried_switches := SwitchSet.remove sw !queried_switches;
        if SwitchSet.is_empty (SwitchSet.inter !switches !queried_switches) then
          (* TODO(cole): warn if queried switches have disconnected. *)
          let _ = List.iter (fun cb -> cb counts) !callbacks in
          release_lock xid;
          Lwt.return ()
        else
          Lwt.return ()
      with Not_found ->
        Log.printf "NetCore_Controller.Queries" "bad query xid (%ld)\n%!" xid

    let register_callback xid
      (cb : ((switchId * Match.t * int, Int64.t * Int64.t) Hashtbl.t -> unit)) = 
      try
        Hashtbl.replace callbacks xid (cb :: Hashtbl.find callbacks xid)
      with Not_found ->
        Hashtbl.add callbacks xid [cb]

    let unregister_callback xid cb = 
      try
        let cbs = 
          List.filter (fun cb' -> cb == cb') (Hashtbl.find callbacks xid) in
        Hashtbl.replace callbacks xid cbs
      with Not_found ->
        ()

  end

*)

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    Log.printf "NetCore_Controller" " compiling new policy for switch %Ld\n%!" sw;
    lwt flow_table = Lwt.wrap2 NetCore_Compiler.flow_table_of_policy sw pol in
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
        failwith "TODO: reinstate queries"
        (* Lwt_list.iter_s (Queries.handle_query_reply sw xid) reps *)
      | _ -> Lwt.return ()
      in
    handle_switch_messages sw

  let switch_thread sw pol_stream =
    switches := SwitchSet.add sw !switches;
    (try_lwt
      Lwt.pick [ install_new_policies sw pol_stream;
                 handle_switch_messages sw ]
     with exn ->
       begin
         Log.printf "[NetCore_Controller]" "unhandled exception %s.\n%!"
           (Printexc.to_string exn);
         Lwt.return ()
       end) >>
    begin
      switches := SwitchSet.remove sw !switches;
      Log.printf "NetCore_Controller" "cleaning up switch %Ld.\n%!" sw;
      Lwt.return ()
    end

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    let sw = feats.switch_id in 
    Log.printf "NetCore_Controller" "switch %Ld connected\n%!" sw;
    Lwt.async (fun () -> switch_thread sw pol_stream);
    accept_switches pol_stream

  let add_to_maps bucket counter =
    let upd (map : 'a list IntMap.t ref) (k : int) (v : 'a) : unit =
      let vs = try IntMap.find k !map with Not_found -> [] in
      map := (IntMap.add k (v::vs) !map)
      in
    upd buckets_to_counters bucket counter;
    upd counters_to_buckets counter bucket

  let initialize_query_state pol : unit =
    failwith "NYI: initialize_query_state"

(*
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
        Queries.query_switches 2l !switches >>
        Lwt_unix.sleep 10.0 >>
        loop ()
      in
    loop ()
*)

  let kill_outstanding_queries switch : unit = 
    failwith "NYI: kill_outstanding_queries"

  let accept_policy push_pol pol switch = 
    (* TODO(cole) kill_outstanding_queries !query_kill_switch; *)
    reset_policy_state ();
    (* TODO(cole) initialize_query_state p; *)
    pol_now := pol;
    push_pol (Some pol);
    (* TODO(cole) spawn_queries p !query_kill_switch *)
    Lwt.return ()

  let rec accept_policies push_pol sugared_pol_stream switch =
    lwt pol = Lwt_stream.next sugared_pol_stream in
    Lwt_switch.turn_off switch >>
    let switch' = Lwt_switch.create () in
    accept_policies push_pol sugared_pol_stream switch' <&> 
      accept_policy push_pol pol switch'

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
