open Core
open Async

open Frenetic_kernel.OpenFlow
module OF10 = Frenetic_kernel.OpenFlow0x01

(* TODO: See openflow.ml for discussion.  This is transitional.

  IF YOU CHANGE THE PROTOCOL HERE, YOU MUST ALSO CHANGE IT IN openflow.ml
 *)

type rpc_ack = RpcOk | RpcEof

type rpc_command =
  | GetSwitches
  | SwitchesReply of OF10.switchId list
  | GetSwitchFeatures of OF10.switchId
  | SwitchFeaturesReply of OF10.SwitchFeatures.t option
  | Send of OF10.switchId * OF10.xid * OF10.Message.t
  | SendReply of rpc_ack
  | SendBatch of OF10.switchId * OF10.xid * OF10.Message.t list
  | BatchReply of rpc_ack
  | GetEvents
  | EventsReply of event
  | SendTrx of OF10.switchId * OF10.Message.t
  | TrxReply of rpc_ack * OF10.Message.t list
  | Finished of unit  (* This is not sent by the client explicitly *)

let chan = Ivar.create ()

let (events, events_writer) = Pipe.create ()

let server_sock_addr = Ivar.create ()
let server_reader = Ivar.create ()
let server_writer = Ivar.create ()

let read_outstanding = ref false
let read_finished = Condition.create ()

module LowLevel = struct
  module OF10 = Frenetic_kernel.OpenFlow0x01

  let openflow_executable () =
    let prog = Filename.dirname(Sys.executable_name) ^ "/frenetic.openflow" in
    Sys.file_exists prog >>= function
    | `Yes -> return prog
    | _ -> failwith (Printf.sprintf "Can't find OpenFlow executable %s!" prog)


  let start port =
    Logging.info "Calling create!";
    let sock_port = 8984 in
    let sock_addr = `Inet (Unix.Inet_addr.localhost, sock_port) in
    let args = ["-s"; string_of_int sock_port;
                "-p"; string_of_int port;
                "-v"] in
    don't_wait_for (
      Logging.info "Current uid: %n" (Unix.getuid ());
      Logging.flushed () >>= fun () ->
      openflow_executable () >>= fun prog ->
        Process.create ~prog ~args ()
        >>= function
        | Error err -> Logging.error "Failed to launch openflow server %s!" prog;
          raise (Core_kernel.Error.to_exn err)
        | Ok proc ->
          Logging.info "Successfully launched OpenFlow controller with pid %s" (Pid.to_string (Process.pid proc));
          (* Redirect stdout of the child proc to out stdout for logging *)
          let buf = Bytes.create 1000 in
          don't_wait_for (Deferred.repeat_until_finished () (fun () ->
              Reader.read (Process.stdout proc) buf >>| function
              | `Eof -> `Finished ()
              | `Ok n -> `Repeat (Writer.write (Lazy.force Writer.stdout) ~len:n (Bytes.to_string buf))));
          Logging.info "Connecting to first OpenFlow server socket";
          let rec wait_for_server () =
            Monitor.try_with ~extract_exn:true (fun () -> Socket.connect (Socket.create Socket.Type.tcp) sock_addr) >>= function
            | Ok sock -> return sock
            | Error exn -> Logging.info "Failed to open socket to OpenFlow server: %s" (Exn.to_string exn);
              Logging.info "Retrying in 1 second";
              after (Time.Span.of_sec 1.)
              >>= wait_for_server in
          wait_for_server ()
          >>= fun sock ->
          Ivar.fill server_sock_addr sock_addr;
          Logging.info "Successfully connected to first OpenFlow server socket";
          Ivar.fill server_reader (Reader.create (Socket.fd sock));
          Ivar.fill server_writer (Writer.create (Socket.fd sock));
          (* We open a second socket to get the events stream *)
          Logging.info "Connecting to second OpenFlow server socket";
          Socket.connect (Socket.create Socket.Type.tcp) sock_addr
          >>= fun sock ->
          Logging.info "Successfully connected to second OpenFlow server socket";
          let reader = Reader.create (Socket.fd sock) in
          let writer = Writer.create (Socket.fd sock) in
          Writer.write_marshal writer ~flags:[] GetEvents;
          Deferred.repeat_until_finished ()
            (fun () ->
               Reader.read_marshal reader
               >>= function
               | `Eof ->
                 Logging.info "OpenFlow controller closed events socket";
                 Pipe.close events_writer;
                 Socket.shutdown sock `Both;
                 return (`Finished ())
               | `Ok (EventsReply evt) ->
                 Pipe.write events_writer evt >>| fun () ->
                 `Repeat ()
               | `Ok (_) ->
                 Logging.error "Got a message that's not an EventsReply.  WTF?  Dropping.";
                 return (`Repeat ())
            )
          )

  let rec clear_to_read () = if (!read_outstanding)
    then Condition.wait read_finished >>= clear_to_read
    else return (read_outstanding := true)

  let signal_read () = read_outstanding := false;
    Condition.broadcast read_finished ()

  let ready_to_process () =
    Ivar.read server_reader
    >>= fun reader ->
    Ivar.read server_writer
    >>= fun writer ->
    clear_to_read ()
    >>= fun () ->
    let read () = Reader.read_marshal reader >>| function
      | `Eof -> Logging.error "OpenFlow server socket shutdown unexpectedly!";
        failwith "Can not reach OpenFlow server!"
      | `Ok a -> a in
    let write = Writer.write_marshal writer ~flags:[] in
    return (read, write)

  let send swid xid msg =
    ready_to_process ()
    >>= fun (recv, send) ->
    send (Send (swid,xid,msg));
    recv ()
    >>| function
    | SendReply resp ->
      signal_read (); resp
    | _ -> Logging.error "Received a reply that's not SendReply to a Send"; assert false

  let send_batch swid xid msgs =
    ready_to_process ()
    >>= fun (recv, send) ->
    send (SendBatch (swid,xid,msgs));
    recv ()
    >>| function
    | BatchReply resp ->
      signal_read (); resp
    | _ -> Logging.error "Received a reply that's not BatchReply to a SendBatch"; assert false


  (* We open a new socket for each send_txn call so that we can block on the reply *)
  let send_txn swid msg =
    Ivar.read server_sock_addr
    >>= fun sock_addr ->
    Socket.connect (Socket.create Socket.Type.tcp) sock_addr
    >>= fun sock ->
    let reader = Reader.create (Socket.fd sock) in
    let writer = Writer.create (Socket.fd sock) in
    Writer.write_marshal writer ~flags:[] (SendTrx (swid,msg));
    Reader.read_marshal reader >>| fun resp ->
      match resp with
      | `Eof ->
        Socket.shutdown sock `Both;
        TrxReply (RpcEof,[])
      | `Ok (TrxReply (RpcEof,_)) ->
        Socket.shutdown sock `Both;
        TrxReply (RpcEof,[])
      | `Ok (TrxReply (RpcOk, resp)) ->
        Socket.shutdown sock `Both;
        TrxReply (RpcOk, resp)
      | _ ->
        Logging.debug "send_txn returned something unintelligible";
        TrxReply (RpcEof,[])

  let events = events
end

let start port =
  LowLevel.start port

let switch_features (switch_id : switchId)  =
  LowLevel.ready_to_process ()
  >>= fun (recv, send) ->
  send (GetSwitchFeatures switch_id);
  recv ()
  >>= function
  | SwitchFeaturesReply resp ->
    LowLevel.signal_read ();
    (match resp with
    | Some sf -> return (Some (From0x01.from_switch_features sf))
    | None -> return None)
  | _ ->
    Logging.error "Received a reply that's not SwitchFeaturesReply to a GetSwitchFeatures";
    assert false

(* We just brute-force this, even though there's significant overlap with from_action *)
let action_from_policy (pol:Frenetic_netkat.Syntax.policy) : action option =
  match pol with
  | Mod hv ->
    begin
      match hv with
      | Location location ->
        begin
          match location with
          | Physical p -> Some (Output (Physical p))
          | FastFail _ -> None
          | Pipe _ -> Some (Output (Controller 128))
          | Query q -> None
        end
      | EthSrc dlAddr ->
        Some (Modify(SetEthSrc dlAddr))
      | EthDst dlAddr ->
        Some (Modify(SetEthDst dlAddr))
      | Vlan n ->
        Some (Modify(SetVlan (Some n)))
      | VlanPcp pcp ->
        Some (Modify(SetVlanPcp pcp))
      | EthType dlTyp ->
        Some (Modify(SetEthTyp dlTyp))
      | IPProto nwProto ->
        Some (Modify(SetIPProto nwProto))
      | IP4Src (nwAddr, mask) ->
        Some (Modify(SetIP4Src nwAddr))
      | IP4Dst (nwAddr, mask) ->
        Some (Modify(SetIP4Dst nwAddr))
      | TCPSrcPort tpPort ->
        Some (Modify(SetTCPSrcPort tpPort))
      | TCPDstPort tpPort ->
        Some (Modify(SetTCPDstPort tpPort))
      | Switch _ | VSwitch _ | VPort _ | VFabric _ | Meta _
      | From _ | AbstractLoc _ -> None
    end
  | _ -> None

let actions_from_policies pol_list =
  List.filter_map pol_list ~f:action_from_policy

let packet_out
  (swid:int64)
  (ingress_port:portId option)
  (payload:payload)
  (pol_list:Frenetic_netkat.Syntax.policy list) =
  (* Turn this into a generic PktOut event, then run it through OF10 translator *)
  let actions = actions_from_policies pol_list in
  let openflow_generic_pkt_out = (payload, ingress_port, actions) in
  let pktout0x01 = Frenetic_kernel.OpenFlow.To0x01.from_packetOut openflow_generic_pkt_out in
  LowLevel.send swid 0l (OF10.Message.PacketOutMsg pktout0x01) >>= function
    | RpcEof -> return ()
    | RpcOk -> return ()

let bogus_flow_stats = {
  flow_table_id = 66L; flow_pattern = Pattern.match_all;
  flow_actions = []; flow_duration_sec = 0L; flow_duration_nsec = 0L;
  flow_priority = 0L; flow_idle_timeout = 0L; flow_hard_timeout = 0L;
  flow_packet_count = 0L; flow_byte_count = 0L
}

(* We aggregate all the OF10 stats and convert them to a generic OpenFlow at the same time *)
let collapse_stats ifrl =
  let open OF10 in
  { bogus_flow_stats with
    flow_packet_count = List.sum (module Int64) ifrl ~f:(fun stat -> stat.packet_count)
    ; flow_byte_count = List.sum (module Int64) ifrl ~f:(fun stat -> stat.byte_count)
  }

let flow_stats (sw_id : switchId) (pat: Pattern.t) : flowStats Deferred.t =
  let pat0x01 = To0x01.from_pattern pat in
  let req = OF10.IndividualRequest
    { sr_of_match = pat0x01; sr_table_id = 0xff; sr_out_port = None } in
  LowLevel.send_txn sw_id (OF10.Message.StatsRequestMsg req) >>= function
    | TrxReply (RpcEof, _) -> assert false
    | TrxReply (RpcOk, l) -> (match l with
      | [] -> Logging.info "Got an empty list"; return bogus_flow_stats
      | [hd] -> ( match hd with
        | StatsReplyMsg (IndividualFlowRep ifrl) ->
          return (collapse_stats ifrl)
        | _ -> Logging.error "Got a reply, but the type is wrong"; return bogus_flow_stats
      )
      | hd :: tl -> Logging.info "Got a > 2 element list"; return bogus_flow_stats
    )
    | _ -> Logging.error "Received a reply that's not TrxReply to a SendTrx"; assert false

let bogus_port_stats = {
  port_no = 666L
    ; port_rx_packets = 0L ; port_tx_packets = 0L
    ; port_rx_bytes = 0L ; port_tx_bytes = 0L ; port_rx_dropped = 0L
    ; port_tx_dropped = 0L ; port_rx_errors = 0L
    ; port_tx_errors = 0L ; port_rx_frame_err = 0L
    ; port_rx_over_err = 0L ; port_rx_crc_err = 0L
    ; port_collisions = 0L
}

let port_stats (sw_id : switchId) (pid : portId) : portStats Deferred.t =
  let pt = Int32.(to_int_exn pid) in
  let req = OF10.PortRequest (Some (PhysicalPort pt)) in
  LowLevel.send_txn sw_id (OF10.Message.StatsRequestMsg req) >>= function
    | TrxReply (RpcEof, _) -> assert false
    | TrxReply (RpcOk, l) -> (match l with
      | [] -> Logging.info "Got an empty list"; return bogus_port_stats
      | [hd] -> ( match hd with
        | StatsReplyMsg (PortRep psl) ->
          return (Frenetic_kernel.OpenFlow.From0x01.from_port_stats (List.hd_exn psl))
        | _ -> Logging.error "Got a reply, but the type is wrong"; return bogus_port_stats
      )
      | hd :: tl -> Logging.info "Got a > 2 element list"; return bogus_port_stats
    )
    | _ -> Logging.error "Received a reply that's not TrxReply to a SendTrx"; assert false

let get_switches () =
  LowLevel.ready_to_process ()
  >>= fun (recv, send) ->
  send GetSwitches;
  recv ()
  >>| function
  | SwitchesReply resp ->
      LowLevel.signal_read (); resp
  | _ -> Logging.error "Received a reply that's not SwitchesReply to a GetSwitches"; assert false

(* TODO: The following is ripped out of Frenetic_netkat.Updates.  Turns out you can't call
stuff in that because of a circular dependency.  In a later version, we should implement
generic commands in Frenetic_kernel.OpenFlow (similar to events, but going the opposite
directions), and let openflow.ml translate these to the specifc version of OpenFlow.  That
way, we can simply pass a plugin instance where the update can write to. *)

module BestEffortUpdate0x01 = struct
  module Comp = Frenetic_netkat.Local_compiler
  module M = OF10.Message
  open Frenetic_kernel.OpenFlow.To0x01

  exception UpdateError

  let current_compiler_options =
    ref Comp.default_compiler_options

  let restrict sw_id repr =
    Comp.restrict Frenetic_netkat.Syntax.(Switch sw_id) repr

  let install_flows_for sw_id table =
    let to_flow_mod p f = M.FlowModMsg (from_flow p f) in
    let priority = ref 65536 in
    let flows = List.map table ~f:(fun flow ->
        decr priority;
        to_flow_mod !priority flow) in
    LowLevel.send_batch sw_id 0l flows >>= function
    | RpcEof -> raise UpdateError
    | RpcOk -> return ()

  let delete_flows_for sw_id =
    let delete_flows = M.FlowModMsg OF10.delete_all_flows in
    LowLevel.send sw_id 5l delete_flows >>= function
      | RpcEof -> raise UpdateError
      | RpcOk -> return ()

  let bring_up_switch (sw_id : switchId) new_r =
    let table = Comp.to_table ~options:!current_compiler_options sw_id new_r in
    Logging.debug "Setting up flow table\n%s"
      (Frenetic_kernel.OpenFlow.string_of_flowTable ~label:(Int64.to_string sw_id) table);
    Monitor.try_with ~name:"BestEffort.bring_up_switch" (fun () ->
      delete_flows_for sw_id >>= fun _ ->
      install_flows_for sw_id table)
    >>= function
      | Ok x -> return x
      | Error _exn ->
        Logging.debug
          "switch %Lu: disconnected while attempting to bring up... skipping" sw_id;
        Logging.flushed () >>| fun () ->
        Logging.error "%s\n%!" (Exn.to_string _exn)

  let implement_policy repr =
    (get_switches ()) >>= fun switches ->
    Deferred.List.iter switches (fun sw_id ->
      bring_up_switch sw_id repr)

  let set_current_compiler_options opt =
    current_compiler_options := opt
end

let update (compiler: Frenetic_netkat.Local_compiler.t) =
  BestEffortUpdate0x01.implement_policy compiler

let update_switch (swid: switchId) (compiler: Frenetic_netkat.Local_compiler.t) =
  BestEffortUpdate0x01.bring_up_switch swid compiler
