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

type state =
  | ConenctedAndSentHello
  | ReceivedHelloAndSentSwitchFeaturesRequest
  | ReceivedSwitchFeaturesReply

let of_messages (bytes:Reader.t) : Message.t Pipe.Reader.t =
  let reader,writer = Pipe.create () in
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



let client_handler (addr:Socket.Address.t)
  (reader:Reader.t) (writer:Writer.t) : unit Deferred.t =
  let msgs = of_messages reader in
  let send msg = Writer.write_without_pushback writer (Serialization.serialize msg) in
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
      Hashtbl.Poly.add switches ~key:switchId ~data:{ features; send; send_txn };
      Pipe.write_without_pushback events_writer (`Connected switchId);
      loop (Connected switchId)
    | Connected switchId, `Eof ->
      (* TODO(jnf): log disconnection *)
      Pipe.write_without_pushback events_writer (`Disconnected switchId);
      return ()
    | _, `Eof ->
      (* TODO(jnf): log disconnection *)
      return ()
    | Connected switchId, `Ok msg ->


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

val get_switches : unit -> switchId list

val get_switch_features : switchId -> SwitchFeatures.t option

val events : event Pipe.Reader.t

val send : switchId -> Message.t -> unit

val send_txn : switchId -> Message.t -> Message.t Deferred.t