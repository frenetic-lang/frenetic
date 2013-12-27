open Async.Std
open Core.Std

module Log = Async_OpenFlow_Log
module Header = OpenFlow_Header
module Message = Async_OpenFlow_Message

(* Uses OCaml's built-in digest module *)
let readable_md5 (buf : string) : string =
  Digest.to_hex (Digest.string buf)

module type S = sig

  type t

  val listen
    : ([< Socket.Address.t ] as 'a, 'b) Tcp.Where_to_listen.t
    -> (t Pipe.Reader.t -> t Pipe.Writer.t -> unit Deferred.t)
    -> ('a, 'b) Tcp.Server.t Deferred.t
       
  val connect
    :  'addr Tcp.where_to_connect
    -> (([ `Active ], 'addr) Socket.t -> t Pipe.Reader.t -> t Pipe.Writer.t -> 
        'a Deferred.t)
    -> 'a Deferred.t

end

module Make (M : Message.Message) : S with type t = M.t = struct

  type t = M.t

  module Serialization = Message.MakeSerializers (M)

  let deserialize ?(label="") (raw_reader : Reader.t) : M.t Pipe.Reader.t =
    let (msg_reader_r, msg_reader_w) = Pipe.create () in
    let _ = Deferred.repeat_until_finished () (fun () ->
      Serialization.deserialize ~tags:[("openflow", "serialization")] 
        ~label:label raw_reader
      >>= function
      | `Eof ->
        Pipe.close msg_reader_w;
        Log.info ~tags:[("openflow", "socket")]
          "[%s] Socket reader closed, thus message also reader closed." label;
        return (`Finished ())
      | `Ok m -> Pipe.write_without_pushback msg_reader_w m; 
                 return (`Repeat ())) in
    msg_reader_r    

  let serializer ?(label="") (raw_writer : Writer.t) : M.t Pipe.Writer.t =
    let (msg_writer_r, msg_writer_w) = Pipe.create () in
    (* We close msg_writer_w, which prevents any further writes. *)
    Stream.iter (Monitor.errors (Writer.monitor raw_writer)) ~f:(fun exn -> 
      Pipe.close msg_writer_w; (* TODO(arjun): what if already closed? will error! *)
      Log.error ~tags:[("openflow", "socket")]
        "[%s] socket write failed, thus message writer also closed." label);
    let _ = Pipe.iter_without_pushback msg_writer_r (fun msg ->
      Serialization.serialize ~tags:[("openflow", "serialization")] ~label:label
        raw_writer msg) in
    msg_writer_w

  let listen where_to_listen msg_handler_receiver =
    Tcp.Server.create ~on_handler_error:`Ignore where_to_listen (fun sock r w ->
      let sock_str = Socket.Address.to_string sock in
      Log.info ~tags:[("openflow", "socket")] 
        "new client %s" sock_str;
      msg_handler_receiver (deserialize ~label:sock_str r)
                           (serializer ~label:sock_str w))

  let connect (addr : 'addr Tcp.where_to_connect) 
    (handler : ([ `Active ], 'addr) Socket.t ->
                M.t Pipe.Reader.t -> M.t Pipe.Writer.t -> 
                'a Deferred.t) 
    : 'a Deferred.t =
    Tcp.with_connection addr (fun sock sock_reader sock_writer ->
      Log.info ~tags:[("openflow", "socket")] 
        "new connection %s (peer is %s)"
        (Socket.Address.to_string (Socket.getsockname sock))
        (Socket.Address.to_string (Socket.getpeername sock));
      handler sock (deserialize ~label:sock_str sock_reader)
                     (serializer ~label:sock_str sock_writer))

end