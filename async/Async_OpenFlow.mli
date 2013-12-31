open Core.Std
open Async.Std

(** By default, displays untagged info messages on stderr. *)
module Log : sig

  include Log.Global_intf

  val make_colored_filtered_output : (string * string) list ->
    Log.Output.t

end

module type Message = sig
  type t
  include Sexpable with type t := t

  val header_of : t -> OpenFlow_Header.t

  val parse : OpenFlow_Header.t -> Cstruct.t -> t

  val marshal : t -> Cstruct.t -> unit

  val marshal' : t -> (OpenFlow_Header.t * Cstruct.t)

  val to_string : t -> string
end

module Platform : sig

  module type S = sig

    type t
    type m

    module Client_id : Unique_id

    type result = [
      | `Connect of Client_id .t
      | `Disconnect of Client_id .t * Sexp.t
      | `Message of Client_id .t * m
    ]

    val create
      :  ?max_pending_connections:int
      -> ?verbose:bool
      -> ?log_disconnects:bool
      -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
      -> port:int
      -> t Deferred.t

    val listen : t -> result Pipe.Reader.t

    val close : t -> Client_id .t -> unit

    val has_switch_id : t -> Client_id .t -> bool

    val send
      :  t
      -> Client_id .t
      -> m
      -> [ `Drop of exn | `Sent of Time.t ] Deferred.t

    val send_to_all : t -> m -> unit

    val client_addr_port
      :  t
      -> Client_id .t
      -> (Unix.Inet_addr.t * int) option

    val listening_port : t -> int

  end

  module Make(Message : Message) : S with type m = Message.t

  module Trans : sig
    type ('t, 'a, 'b) stage = 't -> 'a -> 'b option Deferred.t

    val compose : ('t, 'b, 'c) stage -> ('t, 'a, 'b) stage -> ('t, 'a, 'c) stage
    val (>=>) : ('t, 'a, 'b) stage -> ('t, 'b, 'c) stage -> ('t, 'a, 'c) stage
    val (<=<) : ('t, 'b, 'c) stage -> ('t, 'a, 'b) stage -> ('t, 'a, 'c) stage

    val local : ('t1 -> 't2) -> ('t2, 'a, 'b) stage -> ('t1, 'a, 'b) stage
    val run : ('t, 'a, 'b) stage -> 't -> 'a Pipe.Reader.t -> 'b Pipe.Reader.t
  end
end

(** Lower-level interface. *)
module ClientServer : sig

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

  module Make (M : Message) : S with type t = M.t

end

module Chunk : sig

  module Message : Message
    with type t = (OpenFlow_Header.t * Cstruct.t)

end

module OpenFlow0x01 : sig

  module Message : Message
    with type t = (OpenFlow_Header.xid * OpenFlow0x01.Message.t)

  val chunk_conv
    :  Chunk.Message.t Pipe.Reader.t * Chunk.Message.t Pipe.Writer.t
    -> [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Reader.t * 
       [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Writer.t

end

module OpenFlow0x04 : sig

  module Message : Message
    with type t = (OpenFlow_Header.xid * OpenFlow0x04.Message.t)

  val chunk_conv
    :  Chunk.Message.t Pipe.Reader.t * Chunk.Message.t Pipe.Writer.t
    -> [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Reader.t * 
       [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Writer.t

end
