open Core.Std
open Async.Std
open OpenFlow0x01

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * Message.t
]

module Serialization = Async_OpenFlow_Message.MakeSerializers (Message)

let (events, events_writer) = Pipe.create ()

let of_messages (bytes:Reader.t) : Message.t Pipe.Reader.t =
  let reader,writer = 
    Pipe.create () in

  let rec loop () =
    Serialization.deserialize bytes >>= function
      | `Ok msg ->
         Pipe.write_without_pushback writer msg;
         loop ()
      | `Eof ->
         Pipe.close writer;
         return () in

  don't_wait_for (loop ());
  reader

type switchState =
  { features : SwitchFeatures.t;
    send : Message.t -> unit;
    send_txn : Message.t -> (Message.t list) Deferred.t }

let switches : (switchId, switchState) Hashtbl.Poly.t =
  Hashtbl.Poly.create ()

type state =
  | Initial
  | SentSwitchFeatures
  | Connected

type threadState =
  { switchId : switchId;
    txns : (xid, Message.t list) Hashtbl.t }

let client_handler (a:Socket.Address.t) (r:Reader.t) (w:Writer.t) : unit Deferred.t =
  let msgs = of_messages r in
  let send msg = Writer.write_without_pushback w (Serialization.serialize msg) in
  let send_txn msg = assert false in
  let rec loop state =
    Pipe.read msgs >>= fun msg ->
    match state, msg with
    | _, `Ok (EchoRequest bytes) ->
      send (EchoReply bytes);
      loop state
    | Initial, `Ok (Hello _) ->
      (* TODO(jnf): check version? *)
      send SwitchFeaturesRequest;
      loop SentSwitchFeatures
    | SentSwitchFeatures, `Ok (SwitchFeaturesReply features) ->
      let switchId = features.switchId in
      let txns = Hashtbl.Poly.create () in 
      let threadState = { switchId; txns } in 
      Hashtbl.Poly.add switches ~key:switchId ~data:{ features; send; send_txn };
      Pipe.write_without_pushback events_writer (`Connected switchId);
      loop (Connected threadState)
    | Connected threadState, `Eof ->
      (* TODO(jnf): log disconnection *)
      Pipe.write_without_pushback events_writer (`Disconnected threadState.switchId);
      return ()
    | _, `Eof ->
      (* TODO(jnf): log disconnection *)
      return ()
    | Connected threadState, `Ok msg ->
      assert false in 
  loop Initial

let init ?max_pending_connections
         ?buffer_age_limit
         ~port () =
  don't_wait_for
    (Tcp.Server.create
     ?max_pending_connections
     ?buffer_age_limit
     ~on_handler_error:`Raise
     (Tcp.on_port port)
     client_handler)

let get_switches () = assert false

let get_switch_features switchId = assert false

let send switchId msg = assert false

let send_txn send_txn switchId msg = assert false
