open Printf
open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Types.External

(* Internal policy type *)
type pol = Internal.pol

let (<&>) = Lwt.(<&>)

let init_pol : pol = Internal.PoFilter Internal.PrNone

let for_bucket (in_port : portId) (pkt : Internal.value) =
  let open Internal in
  match pkt with
  | Pkt (swId, NetCore_Pattern.Bucket n, pkt, _) -> Some (n, swId, in_port, pkt)
  | _ -> None

module type MAKE  = functor (Platform : OpenFlow0x01.PLATFORM) -> 
  sig
    val start_controller : policy NetCore_Stream.t -> unit Lwt.t
  end

module Make (Platform : OpenFlow0x01.PLATFORM) = struct

  module SwitchSet = Set.Make (Int64)
  module IntMap = Map.Make (struct type t = int let compare = compare end)

  let switches = ref SwitchSet.empty

  let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t = 
    Hashtbl.create 200

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

  let apply_bucket ((bucket_id, to_controller), sw, pt, pk) : unit =
    if Hashtbl.mem get_pkt_handlers bucket_id then
      let handler = Hashtbl.find get_pkt_handlers bucket_id in
      handler sw pt pk
    else ()

  (* used to initialize newly connected switches and handle packet-in 
     messages *)
  let pol_now : pol ref = ref init_pol

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    Printf.eprintf "[Controller.ml] compiling new policy for switch %Ld\n%!" sw;
    let flow_table = NetCore_Compiler.flow_table_of_policy sw pol in
    Printf.eprintf "[Controller.ml] done compiling policy for switch %Ld\n%!" sw;
    Printf.eprintf "[Controller.ml] flow table is:\n%!";
    List.iter
      (fun (m,a) -> Printf.eprintf "[Controller.ml] %s => %s\n%!"
        (OpenFlow0x01.Match.to_string m)
        (OpenFlow0x01.Action.sequence_to_string a))
      flow_table;
    Platform.send_to_switch sw 0l delete_all_flows >>
    Lwt_list.iter_s
      (fun (match_, actions) ->
          Platform.send_to_switch sw 0l (add_flow match_ actions))
      flow_table >>
    (Printf.eprintf "[Controller.ml] initialized switch %Ld\n%!" sw;
     Lwt.return ())

  let install_new_policies sw pol_stream =
    Lwt_stream.iter_s (configure_switch sw)
      (NetCore_Stream.to_stream pol_stream)
      
  let handle_packet_in sw pkt_in = 
    let open Internal in
    match pkt_in.packetInBufferId with
      | None -> Lwt.return ()
      | Some bufferId ->
        let inp = Pkt (sw, NetCore_Pattern.Physical pkt_in.packetInPort,
                       pkt_in.packetInPacket, Buf bufferId ) in
        let outs = classify !pol_now inp in
        let for_buckets = 
          List.fold_right 
            (fun out acc -> 
              match for_bucket pkt_in.packetInPort out with 
              | None -> acc 
              | Some o -> o ::acc) 
            outs [] in
        List.iter apply_bucket for_buckets;
        Lwt.return ()

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

  let rec accept_switches (pol_stream : pol NetCore_Stream.t) = 
    lwt features = Platform.accept_switch () in
    Printf.eprintf "[NetCore_Controller.ml]: switch %Ld connected\n%!"
      features.switch_id;
    switch_thread features.switch_id pol_stream <&> accept_switches pol_stream

  let reset_policy_state () =
    bucket_cell := 0;
    vlan_cell := 0;
    Hashtbl.reset get_pkt_handlers;
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
      NetCore_Types.desugar 
        genbucket genvlan pol get_pkt_handlers get_count_handlers in
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
