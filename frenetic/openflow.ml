open Core.Std
open Async.Std
open Frenetic_OpenFlow0x01
open Async_parallel
module Log = Frenetic_Log

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * Frenetic_OpenFlow_Header.t * Message.t
]

let (events, events_writer) : event Pipe.Reader.t * event Pipe.Writer.t
  = Pipe.create ()

let openflow_events (r:Reader.t) : (Frenetic_OpenFlow_Header.t * Message.t) Pipe.Reader.t =

  let reader,writer = Pipe.create () in

  let rec loop () =
    let header_str = String.create Frenetic_OpenFlow_Header.size in
    Reader.really_read r header_str >>= function 
    | `Eof _ -> 
      Pipe.close writer;
      return ()
    | `Ok -> 
      let header = Frenetic_OpenFlow_Header.parse (Cstruct.of_string header_str) in
      let body_len = header.length - Frenetic_OpenFlow_Header.size in
      let body_str = String.create body_len in
      Reader.really_read r body_str >>= function
      | `Eof _ -> 
        Pipe.close writer;
        return ()
      | `Ok -> 
        let _,message = Message.parse header body_str in
        Pipe.write_without_pushback writer (header,message);
        loop () in 
  don't_wait_for (loop ());
  reader

type switchState =
  { features : SwitchFeatures.t;
    send : xid -> Message.t -> unit;
    send_txn : Message.t -> (Message.t list) Deferred.t }

let switches : (switchId, switchState) Hashtbl.Poly.t =
  Hashtbl.Poly.create ()

type threadState =
  { switchId : switchId;
    txns : (xid, Message.t list Ivar.t * Message.t list) Hashtbl.t }

type state =
  | Initial
  | SentSwitchFeatures
  | Connected of threadState

let client_handler (a:Socket.Address.Inet.t) (r:Reader.t) (w:Writer.t) : unit Deferred.t =
  let open Message in 

  let serialize (xid:xid) (msg:Message.t) : unit = 
    let header = Message.header_of xid msg in 
    let buf = Cstruct.create header.length in 
    Frenetic_OpenFlow_Header.marshal buf header;
    Message.marshal_body msg (Cstruct.shift buf Frenetic_OpenFlow_Header.size);
    Async_cstruct.schedule_write w buf in 

  let my_events = openflow_events r in

  let rec loop (state:state) : unit Deferred.t =
    Pipe.read my_events >>= fun next_event ->
    match state, next_event with
    (* EchoRequest messages *)
    | _, `Ok (hdr,EchoRequest bytes) ->
      serialize 0l (EchoReply bytes);
      loop state

    (* Initial *)
    | Initial, `Eof -> 
      return () 
    | Initial, `Ok (hdr,Hello bytes) ->
      (* TODO(jnf): check version? *)
      serialize 0l SwitchFeaturesRequest;
      loop SentSwitchFeatures
    | Initial, `Ok(hdr,msg) -> 
      assert false

    (* SentSwitchFeatures *)
    | SentSwitchFeatures, `Eof -> 
      return ()
    | SentSwitchFeatures, `Ok (hdr, SwitchFeaturesReply features) ->
      let switchId = features.switch_id in
      let txns = Hashtbl.Poly.create () in 
      let xid_cell = ref 0l in 
      let next_uid () = xid_cell := Int32.(!xid_cell + 1l); !xid_cell in 
      let threadState = { switchId; txns } in
      let send xid msg = serialize xid msg in 
      let send_txn msg =
        let xid = next_uid () in 
        let ivar = Ivar.create () in 
        Hashtbl.Poly.add_exn txns ~key:xid ~data:(ivar,[]);
        Ivar.read ivar in
      Hashtbl.Poly.add_exn switches ~key:switchId ~data:{ features; send; send_txn };
      Pipe.write_without_pushback events_writer (`Connect (switchId, features));
      loop (Connected threadState)
    | SentSwitchFeatures, `Ok (hdr,msg) -> 
      assert false

    (* Connected *)
    | Connected threadState, `Eof ->
      (* TODO(jnf): log disconnection *)
      Hashtbl.Poly.remove switches threadState.switchId;
      Pipe.write_without_pushback events_writer (`Disconnect threadState.switchId);
      return ()
    | Connected threadState, `Ok (hdr, msg) ->
      Pipe.write_without_pushback events_writer (`Message (threadState.switchId, hdr, msg));
      (match Hashtbl.Poly.find threadState.txns hdr.xid with 
       | None -> ()
       | Some (ivar,msgs) -> 
         Hashtbl.Poly.remove threadState.txns hdr.xid;
         Ivar.fill ivar (msg::msgs));
      loop state in

  serialize 0l (Hello (Cstruct.create 0));
  loop Initial

let get_switches () = 
  Hashtbl.Poly.keys switches

let get_switch_features switchId = 
  match Hashtbl.Poly.find switches switchId with 
  | Some switchState -> Some switchState.features
  | None -> None

let send switchId xid msg = 
  match Hashtbl.Poly.find switches switchId with
  | Some switchState -> 
    switchState.send xid msg; 
    `Ok
  | None ->
    `Eof

let send_txn switchId msg = 
  match Hashtbl.Poly.find switches switchId with
  | Some switchState -> 
    `Ok (switchState.send_txn msg)
  | None -> 
    `Eof

let run_server port sock_addr =
  Log.debug "Called init!";
  don't_wait_for 
    (Tcp.Server.create
       ~on_handler_error:`Raise
       (Tcp.on_port port)
       client_handler >>= fun _ -> 
     return ());
  Socket.(bind (create Socket.Type.unix) (`Unix sock_addr))
  >>| fun sock ->
  let sock = Socket.listen sock in
  Deferred.forever ()
    (fun () ->
       Socket.accept sock
       >>| function
       | `Socket_closed -> Log.error "Socket closed before we started!";
         Socket.shutdown sock `Both;
         failwith "Can not open socket to serve OpenFlow controller interface"
       | `Ok (socket, _) ->
         don't_wait_for (
           begin
             let fd = Socket.fd socket in
             let reader = Reader.create fd in
             let writer = Writer.create fd in
             let read () = Reader.read_marshal reader >>| function
               | `Eof -> Log.error "Upstream socket closed unexpectedly!";
                 Socket.shutdown socket `Both;
                 `Finished ()
               | `Ok a -> a in
             let write = Writer.write_marshal writer ~flags:[] in
             Deferred.repeat_until_finished () (fun () ->
                 read ()
                 >>= function
                 | `Finished () -> return (`Finished ())
                 | `Get_switches ->
                   Log.debug "get_switches";
                   let switches = get_switches () in
                   write (`Get_switches_resp switches);
                   Log.debug "Sent get_switches_resp";
                   return (`Repeat ())
                 | `Get_switch_features sw_id ->
                   Log.debug "get_switch_features";
                   write (`Get_switch_features_resp (get_switch_features sw_id));
                   return (`Repeat ())
                 | `Send (sw_id, xid, msg) ->
                   Log.debug "send";
                   write (`Send_resp (send sw_id xid msg));
                   return (`Repeat ())
                 | `Events ->
                   Log.debug "events";
                   Pipe.iter_without_pushback events
                     ~f:(fun evt ->
                         write (`Events_resp evt)) >>| fun () ->
                   Log.error "Event stream stopped unexpectedly!";
                   `Finished ()
                 | `Send_txn (swid, msg) ->
                   Log.debug "send_txn";
                   write (`Send_txn_resp (send_txn swid msg));
                   return (`Repeat ())) >>| fun () ->
             Log.info "Closing socket";
             Socket.shutdown socket `Both
           end))

let spec =
  let open Command.Spec in
  empty
  +> flag "-p" (optional int) ~doc:"int Port to listen on for OpenFlow switches"
  +> flag "-s" (optional file) ~doc:"file Socket to use for communicating with higher-level controller"
  +> flag "-v" no_arg ~doc:" enable verbose logging (`Debug level)"

let run =
  Command.basic
    ~summary:"Run OpenFlow0x01 controller"
    spec
    (fun port sock_addr verbose () ->
       Log.set_output [(Async.Std.Log.Output.file `Text "/var/log/frenetic/openflow.log")];
       let port = match port with
         | Some port -> port
         | None -> 6634 in
       let sock_addr = match sock_addr with
         | Some sock_addr -> sock_addr
         | None -> "/var/run/frenetic/openflow0x01.socket" in
       let log_level = match verbose with
         | true -> Log.info "Setting log_level to Debug";
           `Debug
         | false -> Log.info "Setting log_level to Info";
           `Info in
       Log.set_level log_level;
       let main () = Deferred.don't_wait_for (
         Signal.(handle terminating
                   ~f:(fun t ->
                       Log.info "Received signal %s" (to_string t);
                       don't_wait_for (Sys.remove sock_addr);
                       shutdown 0));
         Monitor.try_with (fun () ->
             Unix.(openfile ~close_on_exec:true "/var/run/frenetic/openflow0x01.pid" ~mode:[`Creat; `Trunc; `Wronly])
             >>= fun fd -> let writer = Writer.create fd in
             Writer.write writer Pid.(to_string (Unix.getpid ()));
             Writer.close writer) >>= (function
         | Error exn -> Log.error "Failed to create pid file /var/run/frenetic/openflow0x01.pid: %s" (Exn.to_string exn);
           return ()
         | Ok () -> return ()) >>= fun () ->
         Monitor.try_with (fun () -> run_server port sock_addr)
         >>| function
         | Error exn -> Log.error "Caught an exception!";
           Log.error "%s" (Exn.to_string exn)
         | Ok () -> ());
                                               
       in
       (* TODO: 
          (1) register our pid in /var/run/frenetic 
          (2) kill /var/run/frenetic/openflow0x01.socket when we die
       *)
       ignore (main ());
       Core.Std.never_returns (Async.Std.Scheduler.go ()))

let _ = Command.run ~version:"1.0" ~build_info:"RWO" run
