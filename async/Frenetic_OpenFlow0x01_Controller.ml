open Core.Std
open Async.Std

open Frenetic_OpenFlow
module Log = Frenetic_Log
module OF10 = Frenetic_OpenFlow0x01

let chan = Ivar.create ()

let (events, events_writer) = Pipe.create ()

let server_sock_addr = Ivar.create ()
let server_reader = Ivar.create ()
let server_writer = Ivar.create ()

let read_outstanding = ref false
let read_finished = Condition.create ()

let rec clear_to_read () = if (!read_outstanding)
  then Condition.wait read_finished >>= clear_to_read
  else return (read_outstanding := true)

let signal_read () = read_outstanding := false; 
  Condition.broadcast read_finished ()

let openflow_executable () =
  let prog_alt1 = Filename.dirname(Sys.executable_name) ^ "/openflow" in
  let prog_alt2 = Filename.dirname(Sys.executable_name) ^ "/openflow.native" in
  Sys.file_exists prog_alt1 
  >>= function
  | `Yes -> return prog_alt1
  | _ -> Sys.file_exists prog_alt2 
         >>= function
         | `Yes -> return prog_alt2
         | _ -> failwith (Printf.sprintf "Can't find OpenFlow executable %s!" prog_alt2)

let start port =
  Log.info "Calling create!";
  let sock_port = 8984 in
  let sock_addr = `Inet (Unix.Inet_addr.localhost, sock_port) in
  let args = ["-s"; string_of_int sock_port;
              "-p"; string_of_int port;
              "-v"] in
  don't_wait_for (
    Log.info "Current uid: %n" (Unix.getuid ());
    Log.flushed () >>= fun () ->
    openflow_executable () >>= fun prog ->
      Process.create ~prog ~args ()
      >>= function
      | Error err -> Log.error "Failed to launch openflow server %s!" prog;
        raise (Core_kernel.Error.to_exn err)
      | Ok proc ->
        Log.info "Successfully launched OpenFlow controller with pid %s" (Pid.to_string (Process.pid proc));
        (* Redirect stdout of the child proc to out stdout for logging *)
        let buf = String.create 1000 in
        don't_wait_for (Deferred.repeat_until_finished () (fun () ->
            Reader.read (Process.stdout proc) buf >>| function
            | `Eof -> `Finished ()
            | `Ok n -> `Repeat (Writer.write (Lazy.force Writer.stdout) ~len:n buf)));
        Log.info "Connecting to first OpenFlow server socket";
        let rec wait_for_server () = 
          Monitor.try_with ~extract_exn:true (fun () -> Socket.connect (Socket.create Socket.Type.tcp) sock_addr) >>= function
          | Ok sock -> return sock
          | Error exn -> Log.info "Failed to open socket to OpenFlow server: %s" (Exn.to_string exn);
            Log.info "Retrying in 1 second";
            after (Time.Span.of_sec 1.)
            >>= wait_for_server in
        wait_for_server ()
        >>= fun sock ->
        Ivar.fill server_sock_addr sock_addr;
        Log.info "Successfully connected to first OpenFlow server socket";
        Ivar.fill server_reader (Reader.create (Socket.fd sock));
        Ivar.fill server_writer (Writer.create (Socket.fd sock));
        (* We open a second socket to get the events stream *)
        Log.info "Connecting to second OpenFlow server socket";
        Socket.connect (Socket.create Socket.Type.tcp) sock_addr
        >>= fun sock ->
        Log.info "Successfully connected to second OpenFlow server socket";
        let reader = Reader.create (Socket.fd sock) in
        let writer = Writer.create (Socket.fd sock) in
        Writer.write_marshal writer ~flags:[] `Events;
        Deferred.repeat_until_finished ()
          (fun () ->
             Reader.read_marshal reader
             >>= function
             | `Eof ->
               Log.info "OpenFlow controller closed events socket";
               Pipe.close events_writer;
               Socket.shutdown sock `Both;
               return (`Finished ())
             | `Ok (`Events_resp evt) ->
               Pipe.write events_writer evt >>| fun () ->
               `Repeat ()))


let ready_to_process () =
  Ivar.read server_reader
  >>= fun reader ->
  Ivar.read server_writer
  >>= fun writer ->
  clear_to_read ()
  >>= fun () ->
  let read () = Reader.read_marshal reader >>| function
    | `Eof -> Log.error "OpenFlow server socket shutdown unexpectedly!";
      failwith "Can not reach OpenFlow server!"
    | `Ok a -> a in
  let write = Writer.write_marshal writer ~flags:[] in
  return (read, write)

let switch_features (switch_id : switchId)  =
  ready_to_process ()
  >>= fun (recv, send) ->
  send (`Get_switch_features switch_id);
  recv ()
  >>= function
  | `Get_switch_features_resp resp ->
     signal_read ();
     resp >>| fun (feats:OF10.SwitchFeatures.t) ->
     Some {switch_id = feats.switch_id;
       switch_ports = List.filter_map ~f:(fun pd -> Int32.of_int pd.port_no) feats.ports }

let send swid xid msg =
  ready_to_process ()
  >>= fun (recv, send) ->
  send (`Send (swid,xid,msg));
  recv ()
  >>| function
  | `Send_resp resp ->
    signal_read (); resp

let send_batch swid xid msgs =
  ready_to_process ()
  >>= fun (recv, send) ->
  send (`Send_batch (swid,xid,msgs));
  recv ()
  >>| function
  | `Send_batch_resp resp ->
    signal_read (); resp

(* We open a new socket for each send_txn call so that we can block on the reply *)
let send_txn swid msg =
  Ivar.read server_sock_addr
  >>= fun sock_addr ->
  Socket.connect (Socket.create Socket.Type.tcp) sock_addr
  >>= fun sock ->
  let reader = Reader.create (Socket.fd sock) in
  let writer = Writer.create (Socket.fd sock) in
  Writer.write_marshal writer ~flags:[] (`Send_txn (swid,msg));
  Reader.read_marshal reader >>| fun resp ->
    match resp with
    | `Eof ->
      Socket.shutdown sock `Both;
      `Eof
    | `Ok (`Send_txn_resp `Eof) ->
      Socket.shutdown sock `Both;
      `Eof
    | `Ok (`Send_txn_resp (`Ok resp)) ->
      Socket.shutdown sock `Both;
      resp
    | _ ->
      Log.debug "send_txn returned something unintelligible";
      `Eof

let actions_from_first_row compiler =
  (* All the actions we need will be in the first row, the match all row, which
     will always be the only row in the table *)
  let open Frenetic_NetKAT_Compiler in 
  let flow_table = to_table 0L compiler in
  let first_row = List.hd_exn flow_table in
  let group = first_row.action in
  let first_par = List.hd_exn group in
  let first_seq = List.hd_exn first_par in
  first_seq

let packet_out 
  (swid:int64) 
  (ingress_port:portId option) 
  (payload:payload) 
  (compiler:Frenetic_NetKAT_Compiler.t) =
  (* Turn this into a generic PktOut event, then run it through OF10 translator *)
  let actions = actions_from_first_row compiler in 
  let openflow_generic_pkt_out = (payload, ingress_port, actions) in
  let pktout0x01 = Frenetic_OpenFlow.To0x01.from_packetOut openflow_generic_pkt_out in
  send swid 0l (OF10.Message.PacketOutMsg pktout0x01) >>= function
    | `Eof -> return ()
    | `Ok -> return ()  

(* TODO: Implement *)
let flow_stats (swid:switchId) (pred:Frenetic_NetKAT.pred) =
  return {
    flow_table_id = 0L; 
    flow_pattern = Pattern.match_all;
    flow_actions = [];
    flow_duration_sec = 0L;
    flow_duration_nsec = 0L;
    flow_priority = 0L;
    flow_idle_timeout = 0L;
    flow_hard_timeout = 0L;
    flow_packet_count = 0L;
    flow_byte_count = 0L
  }

(* TODO: Implement *)
let port_stats (swid:switchId) (portId:int32) =
  return { 
    port_no = 0L
    ; port_rx_packets = 0L
    ; port_tx_packets = 0L
    ; port_rx_bytes = 0L
    ; port_tx_bytes = 0L
    ; port_rx_dropped = 0L
    ; port_tx_dropped = 0L
    ; port_rx_errors = 0L
    ; port_tx_errors = 0L
    ; port_rx_frame_err = 0L
    ; port_rx_over_err = 0L
    ; port_rx_crc_err = 0L
    ; port_collisions = 0L
  }

let get_switches () =
  ready_to_process ()
  >>= fun (recv, send) ->
  send `Get_switches;
  recv ()
  >>| function
  | `Get_switches_resp resp ->
      signal_read (); resp

(* TODO: The following is ripped out of Frenetic_NetKAT_Updates.  Turns out you can't call
stuff in that because of a circular dependency.  In a later version, we should implement 
generic commands in Frenetic_OpenFlow (similar to events, but going the opposite
directions), and let openflow.ml translate these to the specifc version of OpenFlow.  That
way, we can simply pass a plugin instance where the update can write to. *)

module BestEffortUpdate0x01 = struct
  module Comp = Frenetic_NetKAT_Compiler
  module M = OF10.Message
  open Frenetic_OpenFlow.To0x01

  exception UpdateError

  let current_compiler_options =
    ref Comp.default_compiler_options

  let restrict sw_id repr =
    Comp.restrict Frenetic_NetKAT.(Switch sw_id) repr

  let install_flows_for sw_id table =
    let to_flow_mod p f = M.FlowModMsg (from_flow p f) in
    let priority = ref 65536 in
    let flows = List.map table ~f:(fun flow ->
        decr priority;
        to_flow_mod !priority flow) in
    send_batch sw_id 0l flows >>= function
    | `Eof -> raise UpdateError
    | `Ok -> return ()

  let delete_flows_for sw_id =
    let delete_flows = M.FlowModMsg OF10.delete_all_flows in
    send sw_id 5l delete_flows >>= function
      | `Eof -> raise UpdateError
      | `Ok -> return ()

  let bring_up_switch (sw_id : switchId) new_r =
    let table = Comp.to_table ~options:!current_compiler_options sw_id new_r in
    Log.printf ~level:`Debug "Setting up flow table\n%s"
      (Frenetic_OpenFlow.string_of_flowTable ~label:(Int64.to_string sw_id) table);
    Monitor.try_with ~name:"BestEffort.bring_up_switch" (fun () ->
      delete_flows_for sw_id >>= fun _ ->
      install_flows_for sw_id table)
    >>= function
      | Ok x -> return x
      | Error _exn ->
        Log.debug
          "switch %Lu: disconnected while attempting to bring up... skipping" sw_id;
        Log.flushed () >>| fun () ->
        Log.error "%s\n%!" (Exn.to_string _exn)

  let implement_policy repr =
    (get_switches ()) >>= fun switches ->
    Deferred.List.iter switches (fun sw_id ->
      bring_up_switch sw_id repr)

  let set_current_compiler_options opt =
    current_compiler_options := opt
end

let update (compiler: Frenetic_NetKAT_Compiler.t) =
  BestEffortUpdate0x01.implement_policy compiler

let update_switch (swid: switchId) (compiler: Frenetic_NetKAT_Compiler.t) = 
  BestEffortUpdate0x01.bring_up_switch swid compiler


          
