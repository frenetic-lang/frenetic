open Printf
open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Types.External

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

  let get_count_handlers : (int, get_count_handler) Hashtbl.t = 
    Hashtbl.create 200

  let counters : (int, Int64.t) Hashtbl.t =
    Hashtbl.create 200

  let buckets_to_counters : int IntMap.t ref = ref IntMap.empty
  let counters_to_buckets : (int list) IntMap.t ref = ref IntMap.empty

  let bucket_cell = ref 0 
  let vlan_cell = ref 0 
  let genbucket () = 
    incr bucket_cell;
    !bucket_cell
  let genvlan () = 
    incr vlan_cell;
    Some !vlan_cell

  (* used to initialize newly connected switches and handle packet-in 
     messages *)
  let pol_now : pol ref = ref init_pol

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    Printf.eprintf "[Controller.ml] compiling new policy for switch %Ld\n%!" sw;
    lwt flow_table = Lwt.wrap2 NetCore_Compiler.flow_table_of_policy sw pol in
    Printf.eprintf "[Controller.ml] flow table is:\n%!";
    List.iter
      (fun (m,a) -> Printf.eprintf "[Controller.ml] %s => %s\n%!"
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
         Printf.eprintf "FAIL %s\n%!" (Printexc.to_string exn);
           raise_lwt exn)
      flow_table >>
    (Printf.eprintf "[Controller.ml] initialized switch %Ld\n%!" sw;
     Lwt.return ())

  let install_new_policies sw pol_stream =
    Lwt_stream.iter_p (configure_switch sw)
      (NetCore_Stream.to_stream pol_stream)
      
  let handle_packet_in sw pkt_in = 
    let open Internal in
    match pkt_in.packetInBufferId with
      | None -> Lwt.return ()
      | Some bufferId ->
        let in_port = pkt_in.packetInPort in
        let inp = Pkt (sw, Internal.Physical in_port,
                       pkt_in.packetInPacket, Buf bufferId ) in
        let full_action = NetCore_Semantics.eval !pol_now inp in
        let controller_action =
          NetCore_Action.Output.apply_controller full_action
            (sw, Internal.Physical in_port, pkt_in.packetInPacket) in
        let action = match pkt_in.packetInReason with
          | ExplicitSend -> controller_action
          | NoMatch -> NetCore_Action.Output.par_action controller_action
            (NetCore_Action.Output.switch_part full_action) in
        let outp = { 
          pktOutBufOrBytes = Buffer bufferId; 
          pktOutPortId = None;
          pktOutActions = 
            NetCore_Action.Output.as_actionSequence (Some in_port) action } in
        Platform.send_to_switch sw 0l (PacketOutMsg outp)  

  let handle_stats_reply sw counter rep = match rep with
    | _ -> failwith "NYI: controller.handle_stats_reply"
    (* TODO: pick up here. *)

  let rec handle_switch_messages sw = 
    lwt v = Platform.recv_from_switch sw in
    lwt _ = match v with
      | (_, PacketInMsg pktIn) -> handle_packet_in sw pktIn
      | (bucket, StatsReplyMsg rep) -> handle_stats_reply sw bucket rep
      | _ -> Lwt.return ()
      in
    handle_switch_messages sw

  let switch_thread
      (sw : switchId)
      (pol_stream : pol NetCore_Stream.t) = 
    switches := SwitchSet.add sw !switches;
    install_new_policies sw pol_stream <&> handle_switch_messages sw >>
    (Printf.eprintf "[Controller.ml] thread for switch %Ld terminated.\n" sw;
     switches := SwitchSet.remove sw !switches;
     Lwt.return ())

  let rec accept_switches pol_stream = 
    lwt feats = Platform.accept_switch () in
    let sw = feats.switch_id in 
    Printf.eprintf "[NetCore_Controller.ml]: switch %Ld connected\n%!" sw;
    switch_thread sw pol_stream <&> accept_switches pol_stream

  let reset_policy_state () =
    bucket_cell := 0;
    vlan_cell := 0;
    Hashtbl.reset get_count_handlers;
    Hashtbl.reset counters

  let spawn_queries pol : unit Lwt.t =
    failwith "NYI: spawn_queries"

  let kill_outstanding_queries () : unit = 
    failwith "NYI: kill_outstanding_queries"

  let consolidate_buckets pol =
    failwith "NYI: consolidate_buckets"

  let accept_policy push_pol pol = 
    kill_outstanding_queries ();
    reset_policy_state ();
    let p = 
      NetCore_Desugar.desugar 
        genbucket genvlan pol get_count_handlers in
    Printf.eprintf "[Controller.ml] got new policy:\n%s\n%!" 
      (Internal.pol_to_string p);
    let p', b2c, c2b = consolidate_buckets p in
    buckets_to_counters := b2c;
    counters_to_buckets := c2b;
    pol_now := p';
    push_pol (Some p);
    spawn_queries p

  let accept_policies push_pol sugared_pol_stream =
    Lwt_stream.iter_s (accept_policy push_pol) sugared_pol_stream

  let start_controller pol = 
    let (pol_stream, push_pol) = Lwt_stream.create () in
    accept_switches (NetCore_Stream.from_stream init_pol pol_stream) <&>  
    accept_policies push_pol (NetCore_Stream.to_stream pol)

end
