open Printf
open Packet
open OpenFlow0x04
open OpenFlow0x04_Core
open NetCore_Types
open NetCore_Pattern

module Log = Lwt_log
module SwitchSet = Set.Make (Int64)
module NetCoreCompiler = NetCore_Compiler.NetCoreCompiler
module Compat = NetCore_Compat.Compat0x04

let (<&>) = Lwt.(<&>)

let init_pol : pol = Filter Nothing

module type PLATFORM = OpenFlow0x04_Platform.PLATFORM

module type MAKE  = functor (Platform : PLATFORM) -> 
sig
  val start_controller : NetCore_Types.pol NetCore_Stream.t -> unit Lwt.t
end

module Make (Platform : PLATFORM) = struct
  let rec prio_rec prio = function
    | [] -> []
    | p::rest ->
      let pat,act0 = p in ((prio,pat),act0)::(prio_rec (prio - 1) rest)

  (** val prioritize :
      'a1 coq_Classifier -> ((Word16.t*Pattern.pattern)*'a1) list **)

  let prioritize lst =
    prio_rec max_int lst

  let fix_inport in_port act = match act with
    | Output (PhysicalPort pp) ->
      if Some pp = in_port
      then [Output InPort]
      else [Output (PhysicalPort pp)]
    | _ -> [act]


  let get_inport = List.fold_left (fun acc oxm -> 
      match oxm with
      | OxmInPort pp -> Some pp
      | _ -> acc) None

  let to_flow_mod prio ofMatch act0 tableId =
    let inport = get_inport ofMatch in
    { mfTable_id = tableId; mfCommand = AddFlow; mfOfp_match = ofMatch; mfPriority = prio; 
      mfInstructions = [ApplyActions (List.concat (List.map (fix_inport inport) act0))]; 
      mfCookie = val_to_mask (Int64.of_int 0); mfIdle_timeout = Permanent; 
      mfHard_timeout = Permanent; mfOut_group = None;
      mfFlags = {  fmf_send_flow_rem = false; 
                   fmf_check_overlap = false; 
                   fmf_reset_counts = false; 
                   fmf_no_pkt_counts = false;
                   fmf_no_byt_counts = false }; 
      mfBuffer_id = None; mfOut_port = None}


  let rec flowtable_to_group_table1 ft n = match ft with
    | (mat, []) :: ft -> let ft', gt = flowtable_to_group_table1 ft n in
      ((mat, []) :: ft', gt)
    | (mat, [act]) :: ft -> let ft', gt = flowtable_to_group_table1 ft n in
      ((mat, act) :: ft', gt)
    | (mat, acts) :: ft -> let ft', gt = flowtable_to_group_table1 ft (Int32.succ n) in
      ((mat, [Group n]) :: ft', (n, acts) :: gt)
    | [] -> ([], [])


  let flowtable_to_group_table ft = 
    flowtable_to_group_table1 ft 0l

  let rec get_watch_port act = match act with
    | Output (PhysicalPort pp) :: acts -> Some pp
    | _ :: acts -> get_watch_port acts
    | [] -> None

  let acts_to_buckets acts = List.map 
      (fun act ->  { bu_weight = 0; bu_watch_port = get_watch_port act;
                     bu_watch_group = None; bu_actions = act }) acts

  let configure_switch (sw : switchId) (pol : pol) : unit Lwt.t =
    lwt flow_table = Lwt.wrap2 Compat.flow_table_of_policy sw pol in
    let ft, gt = flowtable_to_group_table flow_table in
    Platform.send_to_switch sw 0l (FlowModMsg delete_all_flows) >>
    Lwt_list.iter_s
      (fun (gid, actions) ->
         Platform.send_to_switch sw 0l 
           (GroupModMsg (AddGroup (FF, gid, acts_to_buckets actions))) >>
         Lwt.return ())
      gt >>
    let prio = ref 65535 in
    Lwt_list.iter_s
      (fun (match_, actions) ->
         Platform.send_to_switch sw 0l 
           (FlowModMsg (to_flow_mod !prio match_ actions 0)) >>
         (decr prio; Lwt.return ()))
      ft

  let install_new_policies sw pol_stream =
    Lwt_stream.iter_s (configure_switch sw)
      (NetCore_Stream.to_stream pol_stream)

  let get_in_port_from_oxm = List.fold_left 
      (fun acc x -> match x with
         | OxmInPort pt -> Some pt
         | _ -> acc) None

  let handle_packet_in pol sw pkt_in = 
    let in_port = match get_in_port_from_oxm pkt_in.pi_ofp_match with
      | Some in_port -> in_port
      | _ -> failwith "handle_packet_in: Expected in_port OXM from packet_in" in
    try_lwt
      let pol = NetCore_Stream.now pol in
      let in_packet, payload = match pkt_in.pi_payload with
        | NotBuffered pkt -> Packet.parse pkt, OpenFlow0x01_Core.NotBuffered pkt
        | Buffered _ -> failwith "Buffered packets not supported by NetCore_Controller0x04"
      in
      let in_val = 
        Pkt (sw, Physical in_port, in_packet, payload) in
      (* Evaluate the packet against the full policy. *)
      let policy_out_vals = NetCore_Semantics.eval pol in_val in

      (* Evaluate the packet against the flow table on the switch. *)
      let classifier = NetCoreCompiler.compile_pol pol sw in
      let switch_action =
        NetCore_Action.Group.switch_part
          (NetCoreCompiler.OutputClassifier.scan 
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
          NetCore_Action.Group.par_action
          NetCore_Action.Group.drop
          (List.map (fun v -> NetCore_Action.Group.make_transformer in_val v)
             new_out_vals) in

      let out_payload = 
        { po_payload = pkt_in.pi_payload
        ; po_in_port = (PhysicalPort in_port)
        ; po_actions = 
            match Compat.as_actionSequence (Some in_port) action with
            | acts :: _ -> acts
            | [] -> []
        } in
      Platform.send_to_switch sw 0l (PacketOutMsg out_payload)  
    with Unparsable _ -> 
      Log.warning_f "unparsable packet"

  let rec handle_switch_messages pol sw = 
    let open Message in
    lwt (xid, msg) = Platform.recv_from_switch sw in
    lwt _ = match msg with
      | PacketInMsg pktIn -> handle_packet_in pol sw pktIn
      (* | StatsReplyMsg (Stats.IndividualFlowRep reps) ->  *)
      (*   Queries.handle_reply sw xid reps; *)
      (*   Lwt.return () *)
      (* | StatsReplyMsg r -> *)
      (*   Log.warning_f "received unexpected stats reply type (%s)" *)
      (*     (Stats.reply_to_string r) *)
      | PortStatusMsg msg ->
        (* TODO(arjun): this should be used by NetCore_Topo *)
        Lwt.return ()
      | _ -> Lwt.return () in
    handle_switch_messages pol sw

  let switch_thread features pol_stream =
    let sw = features.datapath_id in
    Log.info_f "switch %Ld connected.\n%!" sw >>
    (* let _ = Queries.add_switch sw in *)
    (try_lwt
       lwt _ = Lwt.wrap2 NetCore_Semantics.handle_switch_events
           (SwitchUp (sw, features))
           (NetCore_Stream.now pol_stream) in
       Lwt.pick [ install_new_policies sw pol_stream;
                  handle_switch_messages pol_stream sw ]
     with exn ->
       begin
         match exn with
         | OpenFlow0x04_Platform.OpenFlowPlatform.SwitchDisconnected _ ->
           (* TODO(arjun): I can assume sw itself disconnected? *)
           Lwt.return ()
         | _ ->
           Log.error_f ~exn:exn "unhandled exception"
       end) >>
    begin
      Lwt.async
        (fun () -> (* TODO(arjun): 
                      confirm this gracefully discards the exception *)
           Lwt.wrap2 NetCore_Semantics.handle_switch_events 
             (SwitchDown sw)
             (NetCore_Stream.now pol_stream));
      (* Queries.remove_switch sw; *)
      Log.info_f "switch %Ld disconnected.\n%!" sw
    end

  let rec accept_switches pol_stream = 
    lwt (feats, ports) = Platform.accept_switch () in
    (* An exception raised by a switch thread does not kill the controller. *)
    Lwt.async (fun () -> switch_thread (Compat.to_nc_features feats ports) pol_stream);
    accept_switches pol_stream

  let rec setup_queries pol_stream = init_pol
  (* lwt pol = Lwt_stream.next pol_stream in *)
  (* Queries.stop () >> *)
  (* Queries.start pol <&> setup_queries pol_stream *)

  (** Emits packets synthesized at the controller. Produced packets
      are _not_ subjected to the current NetCore policy, so they do not
      compose nicely with NetCore operatores. This requires some deep thought,
      which is banned until after PLDI 2013. *)
  let emit_packets pkt_stream = 
    let open PacketOut in
    let emit_pkt (sw, pt, bytes) =
      let msg = {
        po_payload = NotBuffered bytes;
        po_in_port = (Controller 0);
        po_actions = [Output (PhysicalPort pt)]
      } in
      Platform.send_to_switch sw 0l (PacketOutMsg msg) in
    Lwt_stream.iter_s emit_pkt pkt_stream

  let start_controller pkt_stream pol = 
    (* Do not delete the following printf. It is meant to be the minimal output
       from the controller. *)
    Log.info_f "controller started" >>
    Lwt.pick [
      accept_switches pol;
      emit_packets pkt_stream;
      (* setup_queries (NetCore_Stream.to_stream pol) *)
    ]

end
