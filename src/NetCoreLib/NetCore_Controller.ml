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

  let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t = 
    Hashtbl.create 200

  let apply_bucket (bucket_id, sw, pt, pk) : unit =
    let handler = Hashtbl.find get_pkt_handlers bucket_id in
    handler sw pt pk

  (* used to initialize newly connected switches and handle packet-in 
     messages *)
  let pol_now : pol ref = ref init_pol

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    Printf.eprintf "[Controller.ml] compiling new policy for switch %Ld\n%!" sw;
    let flow_table = NetCore_Compiler.flow_table_of_policy sw pol in
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
        let inp = Pkt (sw, NetCore_Pattern.Physical pkt_in.packetInPort,
                       pkt_in.packetInPacket, Buf bufferId ) in
        let outs = NetCore_Semantics.classify !pol_now inp in
        let for_buckets =
          List.fold_right
            (fun oo acc -> match for_bucket pkt_in.packetInPort oo with
              | None -> acc
              | Some o -> o ::acc) outs [] in
        List.iter apply_bucket for_buckets;
        Lwt.return ()

  let rec handle_switch_messages sw = 
    lwt v = Platform.recv_from_switch sw in
    match v with
      | (_, PacketInMsg pktIn) ->
        handle_packet_in sw pktIn >> handle_switch_messages sw
      | _ -> handle_switch_messages sw

  let switch_thread
      (sw : switchId)
      (pol_stream : pol NetCore_Stream.t) = 
    install_new_policies sw pol_stream <&> handle_switch_messages sw >>
    (Printf.eprintf "[Controller.ml] thread for switch %Ld terminated.\n" sw;
     Lwt.return ())

  let rec accept_switches pol_stream = 
    lwt features = Platform.accept_switch () in
    Printf.eprintf "[NetCore_Controller.ml]: switch %Ld connected\n%!"
      features.switch_id;
    switch_thread features.switch_id pol_stream <&> accept_switches pol_stream

  let bucket_cell = ref 0 
  let vlan_cell = ref 0 
  let genbucket () = 
    incr bucket_cell;
    !bucket_cell
  let genvlan () = 
    incr vlan_cell;
    Some !vlan_cell

  let configure_switches push_pol sugared_pol_stream =
    Lwt_stream.iter
      (fun pol ->
        let p = NetCore_Types.desugar genbucket genvlan pol get_pkt_handlers in
        pol_now := p;
        push_pol (Some p))
      sugared_pol_stream

  let start_controller pol = 
    let (pol_stream, push_pol) = Lwt_stream.create () in
    accept_switches (NetCore_Stream.from_stream init_pol pol_stream) <&>  
    configure_switches push_pol (NetCore_Stream.to_stream pol)

end
