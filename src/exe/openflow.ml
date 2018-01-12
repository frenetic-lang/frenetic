open Core
open Async
open Frenetic.OpenFlow0x01
module Logging = Frenetic.Async.Logging
module ToGeneric = Frenetic.OpenFlow.From0x01

type event = Frenetic.OpenFlow.event

(* TODO: This protocol needs to be cleaned up.  It makes more sense to define the
   RPC protocol in Frenetic.OpenFlow, but we can't at the moment because Send sends
   OpenFlow0x01 messages.  It should be the job of openflow.ml to convert 1.x specific
   messages to the generic OpenFlow messages and pass them over the pipe to Frenetic.

  IF YOU CHANGE THE PROTOCOL HERE, YOU MUST ALSO CHANGE IT IN Frenetic.Async.OpenFlow0x01_Plugin.
  Unfortunately, we can't just plop this in Frenetic.OpenFlow0x01 (because it references
  Frenetic.OpenFlow) or Frenetic.OpenFlow (because it references Frenetic.OpenFlow0x01).  We
  could have put it in its own module, but I dont' want to give legitimacy to something
  that's transitional.
 *)

type rpc_ack = RpcOk | RpcEof

(* Don't send this over RPC.  You'll be sorry! *)
type trx_status = Done | Unfulfilled of Message.t list Deferred.t

type rpc_command =
  | GetSwitches
  | SwitchesReply of switchId list
  | GetSwitchFeatures of switchId
  | SwitchFeaturesReply of SwitchFeatures.t option
  | Send of switchId * xid * Message.t
  | SendReply of rpc_ack
  | SendBatch of switchId * xid * Message.t list
  | BatchReply of rpc_ack
  | GetEvents
  | EventsReply of event
  | SendTrx of switchId * Message.t
  | TrxReply of rpc_ack * Message.t list
  | Finished of unit  (* This is not sent by the client explicitly *)

let (events, events_writer) : event Pipe.Reader.t * event Pipe.Writer.t
  = Pipe.create ()

let openflow_events (r:Reader.t) : (Frenetic.OpenFlow_Header.t * Message.t) Pipe.Reader.t =

  let reader,writer = Pipe.create () in

  let rec loop () =
    let header_str = Bytes.create Frenetic.OpenFlow_Header.size in
    Reader.really_read r header_str >>= function
    | `Eof _ ->
      Pipe.close writer;
      return ()
    | `Ok ->
      let header = Frenetic.OpenFlow_Header.parse (Cstruct.of_bytes header_str) in
      let body_len = header.length - Frenetic.OpenFlow_Header.size in
      let body_str = Bytes.create body_len in
      Reader.really_read r body_str >>= function
      | `Eof _ ->
        Pipe.close writer;
        return ()
      | `Ok ->
        let _,message = Message.parse header (Bytes.to_string body_str) in
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
    Frenetic.OpenFlow_Header.marshal buf header;
    Message.marshal_body msg (Cstruct.shift buf Frenetic.OpenFlow_Header.size);
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
      (* We could check the version here, but OpenFlow will respond with a HELLO of
         the latest supported version.  0x04 sends a bitmap of supported versions in the
         payload, but that requires a 0x04 translation in a 0x01 plugin, which is crazy.
         It's better just to blow up if the switch doesn't support 0x01, which is
         what would happen anyway.  A 0x04 version of openflow.ml will check the version
         correctly because it has a HELLO processor. *)
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
      let generic_sw_f = ToGeneric.from_switch_features features in
      let port_list = generic_sw_f.switch_ports in
      Hashtbl.Poly.add_exn switches ~key:switchId ~data:{ features; send; send_txn };
      Logging.debug "Switch %s connected%!" (string_of_switchId switchId);
      Pipe.write_without_pushback events_writer (SwitchUp (switchId, port_list));
      loop (Connected threadState)
    (* TODO: Queue up these messages and deliver them when we're done connecting *)
    | SentSwitchFeatures, `Ok (hdr,msg) ->
      Logging.debug "Dropping unexpected msg received before SwitchFeatures response: %s%!" (Message.to_string msg);
      loop state

    (* Connected *)
    | Connected threadState, `Eof ->
      Hashtbl.Poly.remove switches threadState.switchId;
      Logging.debug "Switch %s disconnected%!" (string_of_switchId threadState.switchId);
      Pipe.write_without_pushback events_writer (SwitchDown threadState.switchId);
      return ()
    | Connected threadState, `Ok (hdr, msg) ->
      let generic_openflow_event = ToGeneric.event_from_message threadState.switchId msg in
      let goe = Option.value ~default:(SwitchDown 0L) generic_openflow_event in
      Logging.info "Writing event response %s%!" (Frenetic.OpenFlow.string_of_event goe);
      Pipe.write_without_pushback events_writer goe;
      (match Hashtbl.Poly.find threadState.txns hdr.xid with
       | None -> ()
       | Some (ivar ,msgs) ->
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
    RpcOk
  | None ->
    RpcEof

let send_batch switchId xid msgs =
  match Hashtbl.Poly.find switches switchId with
  | Some switchState ->
    List.iter msgs ~f:(switchState.send xid);
    RpcOk
  | None ->
    RpcEof

let send_txn switchId msg =
  match Hashtbl.Poly.find switches switchId with
  | Some switchState ->
    (* The following returns a Deferred which will be fulfilled when a
    matching response is received *)
    Unfulfilled (switchState.send_txn msg)
  | None ->
    Done

let rpc_handler (a:Socket.Address.Inet.t) (reader:Reader.t) (writer:Writer.t) : unit Deferred.t =
  let read () = Reader.read_marshal reader >>| function
    | `Eof -> Logging.error "Upstream socket closed unexpectedly!";
      Finished ()
    | `Ok a -> a in
  let write = Writer.write_marshal writer ~flags:[] in
  Deferred.repeat_until_finished ()
    (fun () -> read () >>= function
     | Finished () -> return (`Finished ())
     | GetSwitches ->
       let switches = get_switches () in
       write (SwitchesReply switches);
       return (`Repeat ())
     | GetSwitchFeatures sw_id ->
       write (SwitchFeaturesReply (get_switch_features sw_id));
       return (`Repeat ())
     | Send (sw_id, xid, msg) ->
       write (SendReply (send sw_id xid msg));
       return (`Repeat ())
     | SendBatch (sw_id, xid, msgs) ->
       write (BatchReply (send_batch sw_id xid msgs));
       return (`Repeat ())
     | GetEvents ->
       Pipe.iter_without_pushback events
         ~f:(fun evt -> write (EventsReply evt))
       >>| fun () ->
       Logging.error "Event stream stopped unexpectedly!";
       `Finished ()   (* You don't need a return here, because of the >>| operator *)
     | SwitchesReply _
     | SwitchFeaturesReply _
     | SendReply _
     | BatchReply _
     | EventsReply _
     | TrxReply (_, _) ->
        Logging.error("Reply sent from client.  Wrong direction!");
        return (`Repeat ())
     | SendTrx (swid, msg) ->
       let run_trx = send_txn swid msg in
       let () = match run_trx with
         | Done -> write (TrxReply (RpcEof, []))
         | Unfulfilled ivar_deferred ->
            Logging.info "Wrote Transaction repsonse";
            upon ivar_deferred
              (fun collected_responses -> write (TrxReply (RpcOk, collected_responses)))
      in
      return (`Repeat ()))

let run_server port rpc_port =
  don't_wait_for
    (Tcp.Server.create
       ~on_handler_error:`Raise
       (Tcp.Where_to_listen.of_port port)
       client_handler >>= fun _ ->
     return ());
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port rpc_port)
    rpc_handler >>= fun _ -> return ()

let spec =
  let open Command.Spec in
  empty
  +> flag "-p" (optional_with_default 6633 int) ~doc:"int Port to listen on for OpenFlow switches"
  +> flag "-s" (optional_with_default 8984 int) ~doc:"file TCP port to serve on for communicating with higher-level controller"
  +> flag "-l" (optional_with_default "./openflow.log" file) ~doc:"string log path"
  +> flag "-v" no_arg ~doc:" enable verbose logging (`Debug level)"

let run =
  Command.basic_spec
    ~summary:"Run OpenFlow0x01 controller"
    spec
    (fun port rpc_port log_file verbose () ->
       let log_output = lazy (Async_extended.Std.Log.Output.file `Text log_file) in
       Frenetic.Async.Logging.set_output [Lazy.force log_output];
       let log_level = match verbose with
         | true -> Logging.info "Setting log_level to Debug";
           `Debug
         | false -> Logging.info "Setting log_level to Info";
           `Info in
       Logging.set_level log_level;
       let main () = Deferred.don't_wait_for (
         Signal.(handle terminating
                   ~f:(fun t ->
                       Logging.info "Received signal %s" (to_string t);
                       shutdown 0));
         Monitor.try_with (fun () -> run_server port rpc_port)
         >>| function
         | Error exn -> Logging.error "Caught an exception!";
           Logging.error "%s" (Exn.to_string exn)
         | Ok () -> ());

       in
       ignore (main ());
       Core.never_returns (Async.Scheduler.go ()))

let _ = Command.run ~version:"1.0" ~build_info:"RWO" run
