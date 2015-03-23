open Core.Std
open Async.Std
open OpenFlow_Header
open OpenFlow0x01

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * Message.t
]

module Serialization = Async_OpenFlow_Message.MakeSerializers (struct 
  type t = xid * Message.t with sexp

  let header_of (xid,msg) = 
    Message.header_of xid msg

  let parse hdr bytes = 
    Message.parse hdr (Cstruct.to_string bytes) 

  let marshal (xid,msg) bytes = 
    Message.marshal_body msg bytes 
end)

let (events, events_writer) = Pipe.create ()

let of_messages (bytes:Reader.t) : (xid * Message.t) Pipe.Reader.t =
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

type threadState =
  { switchId : switchId;
    txns : (xid, Message.t list) Hashtbl.t }

type state =
  | Initial
  | SentSwitchFeatures
  | Connected of threadState

let client_handler (a:Socket.Address.Inet.t) (r:Reader.t) (w:Writer.t) : unit Deferred.t =
  let open Message in 
  let msgs = of_messages r in
  let send msg = Serialization.serialize w (0l,msg) in
  let send_txn msg = assert false in
  let rec loop state =
    Pipe.read msgs >>= fun msg ->
    match state, msg with
    | _, `Ok (_, EchoRequest bytes) ->
      send (EchoReply bytes);
      loop state
    | Initial, `Ok (_, Hello _) ->
      (* TODO(jnf): check version? *)
      send SwitchFeaturesRequest;
      loop SentSwitchFeatures
    | SentSwitchFeatures, `Ok (_, SwitchFeaturesReply features) ->
      let switchId = features.switch_id in
      let txns = Hashtbl.Poly.create () in 
      let threadState = { switchId; txns } in 
      Hashtbl.Poly.add_exn switches ~key:switchId ~data:{ features; send; send_txn };
      Pipe.write_without_pushback events_writer (`Connect (switchId, features));
      loop (Connected threadState)
    | Connected threadState, `Eof ->
      (* TODO(jnf): log disconnection *)
      Pipe.write_without_pushback events_writer (`Disconnect threadState.switchId);
      return ()
    | _, `Eof ->
      (* TODO(jnf): log disconnection *)
      return ()
    | Connected threadState, `Ok (xid, msg) ->
      assert false 
    | _ -> 
      assert false in 
  
  loop Initial

let init ?max_pending_connections ~port =
  Tcp.Server.create
    ?max_pending_connections
    ~on_handler_error:`Raise
    (Tcp.on_port port)
    client_handler >>= fun server -> 
    return ()

let close switchId = assert false

let get_switches () = assert false

let get_switch_features switchId = assert false

let send switchId msg = assert false

let send_txn switchId msg = assert false
