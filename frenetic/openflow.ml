open Core.Std
open Async.Std
open Frenetic_OpenFlow0x01
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

let socket_id = ref 0

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
      (* We start the transaction ID high because Frenetic will send low ID's for flow
      table requests *)
      let xid_cell = ref 100000l in
      let next_uid () = xid_cell := Int32.(!xid_cell + 1l); !xid_cell in
      let threadState = { switchId; txns } in
      let send xid msg = serialize xid msg in
      let send_txn msg =
        let xid = next_uid () in
        let ivar = Ivar.create () in
        Hashtbl.Poly.add_exn txns ~key:xid ~data:(ivar,[]);
        serialize xid msg;
        Ivar.read ivar;
        in
      Hashtbl.Poly.add_exn switches ~key:switchId ~data:{ features; send; send_txn };
      Log.debug "Switch %s connected" (string_of_switchId switchId);
      Pipe.write_without_pushback events_writer (`Connect (switchId, features));
      loop (Connected threadState)
    (* TODO: Queue up these messages and deliver them when we're done connecting *)
    | SentSwitchFeatures, `Ok (hdr,msg) ->
      Log.debug "Dropping unexpected msg received before SwitchFeatures response: %s" (Message.to_string msg);
      loop state   

    (* Connected *)
    | Connected threadState, `Eof ->
      Hashtbl.Poly.remove switches threadState.switchId;
      Log.debug "Switch %s disconnected" (string_of_switchId threadState.switchId);
      Pipe.write_without_pushback events_writer (`Disconnect threadState.switchId);
      return ()
    | Connected threadState, `Ok (hdr, msg) ->
      (* TODO: Am not sure we should be writing a transactional response to the event writer *)
      Pipe.write_without_pushback events_writer (`Message (threadState.switchId, hdr, msg));
      (match Hashtbl.Poly.find threadState.txns hdr.xid with 
       | None -> ()
       | Some (ivar,msgs) -> 
         Hashtbl.Poly.remove threadState.txns hdr.xid;
         (* Am not sure why this is a list, since there will never be more than one.  The
         above line removes the hash entry so it'll never come up again. *)
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

let send_batch switchId xid msgs = 
  match Hashtbl.Poly.find switches switchId with
  | Some switchState ->
    List.iter msgs ~f:(switchState.send xid);
    `Ok
  | None ->
    `Eof

let send_txn switchId msg = 
  match Hashtbl.Poly.find switches switchId with
  | Some switchState ->
    (* The following returns a Deferred which will be fulfilled when a
    matching response is received *)
    `Ok (switchState.send_txn msg)
  | None -> 
    `Eof

let rpc_handler (a:Socket.Address.Inet.t) (reader:Reader.t) (writer:Writer.t) : unit Deferred.t =
  let read () = Reader.read_marshal reader >>| function
    | `Eof -> Log.error "Upstream socket closed unexpectedly!";
      `Finished ()
    | `Ok a -> a in
  let write = Writer.write_marshal writer ~flags:[] in
  Deferred.repeat_until_finished ()
    (fun () -> read () >>= function
     | `Finished () -> return (`Finished ())
     | `Get_switches ->
       let switches = get_switches () in
       write (`Get_switches_resp switches);
       return (`Repeat ())
     | `Get_switch_features sw_id ->
       write (`Get_switch_features_resp (get_switch_features sw_id));
       return (`Repeat ())
     | `Send (sw_id, xid, msg) ->
       write (`Send_resp (send sw_id xid msg));
       return (`Repeat ())
     | `Send_batch (sw_id, xid, msgs) ->
       write (`Send_batch_resp (send_batch sw_id xid msgs));
       return (`Repeat ())
     | `Events ->
       Pipe.iter_without_pushback events
         ~f:(fun evt ->
             write (`Events_resp evt)) >>| fun () ->
       Log.error "Event stream stopped unexpectedly!";
       `Finished ()
     | `Send_txn (swid, msg) ->
       let run_trx = send_txn swid msg in
       let () = match run_trx with
         | `Eof -> write (`Send_trx_rep `Eof)
         | `Ok ivar_deferred ->
            upon ivar_deferred
              (fun collected_responses -> write (`Send_txn_resp (`Ok (`Ok collected_responses))))
          in
       return (`Repeat ()))
  
let run_server port rpc_port =
  don't_wait_for
    (Tcp.Server.create
       ~on_handler_error:`Raise
       (Tcp.on_port port)
       client_handler >>= fun _ -> 
     return ());
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.on_port rpc_port)
    rpc_handler >>= fun _ -> return ()
  
let spec =
  let open Command.Spec in
  empty
  +> flag "-p" (optional_with_default 6633 int) ~doc:"int Port to listen on for OpenFlow switches"
  +> flag "-s" (optional_with_default 8984 int) ~doc:"file TCP port to serve on for communicating with higher-level controller"
  +> flag "-l" (optional_with_default "./openflow.log" file) ~doc:"string log path"
  +> flag "-v" no_arg ~doc:" enable verbose logging (`Debug level)"

let run =
  Command.basic
    ~summary:"Run OpenFlow0x01 controller"
    spec
    (fun port rpc_port log_file verbose () ->
       let log_output = lazy (Async_extended.Std.Log.Output.file `Text log_file) in
       Frenetic_Log.set_output [Lazy.force log_output];
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
                       shutdown 0));
         Monitor.try_with (fun () -> run_server port rpc_port)
         >>| function
         | Error exn -> Log.error "Caught an exception!";
           Log.error "%s" (Exn.to_string exn)
         | Ok () -> ());
                                               
       in
       ignore (main ());
       Core.Std.never_returns (Async.Std.Scheduler.go ()))

let _ = Command.run ~version:"1.0" ~build_info:"RWO" run
