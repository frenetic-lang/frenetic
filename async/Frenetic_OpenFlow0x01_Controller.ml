open Core.Std
open Async.Std
open Frenetic_OpenFlow0x01
module Log = Frenetic_Log

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * Frenetic_OpenFlow_Header.t * Message.t
]

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

let init port =
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

let get_switches () =
  ready_to_process ()
  >>= fun (recv, send) ->
  send `Get_switches;
  recv ()
  >>| function
  | `Get_switches_resp resp ->
      signal_read (); resp

let get_switch_features (switch_id : switchId) =
  ready_to_process ()
  >>= fun (recv, send) ->
  send (`Get_switch_features switch_id);
  recv ()
  >>| function
  | `Get_switch_features_resp resp ->
    signal_read (); resp

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
