open Core.Std
open Async.Std

module type Message = sig
  type t
  include Sexpable with type t := t

  val header_of : t -> OpenFlow_Header.t

  val parse : OpenFlow_Header.t -> Cstruct.t -> t

  val marshal : t -> Cstruct.t -> int

  val to_string : t -> string
end

module Platform : sig

  module type S = sig

    type t
    type m

    module Switch_id: Unique_id

    type result = [
      | `Connect of Switch_id.t
      | `Disconnect of Switch_id.t * Sexp.t
      | `Message of Switch_id.t * m
    ]

    val create
      :  ?max_pending_connections:int
      -> ?verbose:bool
      -> ?log_disconnects:bool
      -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
      -> port:int
      -> t Deferred.t

    val listen : t -> result Pipe.Reader.t

    val close : t -> Switch_id.t -> unit

    val has_switch_id : t -> Switch_id.t -> bool

    val send
      :  t
      -> Switch_id.t
      -> m
      -> [ `Drop of exn | `Sent of Time.t ] Deferred.t

    val send_to_all : t -> m -> unit

    val client_addr_port
      :  t
      -> Switch_id.t
      -> (Unix.Inet_addr.t * int) option

    val listening_port : t -> int

  end

  module Make(Message : Message) : S with type m = Message.t

end

module OpenFlow0x01 : sig

  module Message : Message
   with type t = (OpenFlow_Header.xid * OpenFlow0x01.Message.t)

end

module OpenFlow0x04 : sig

  module Message : Message
    with type t = (OpenFlow_Header.xid * OpenFlow0x04.Message.t)

end

module Chunk : sig

  module Message : Message
    with type t = (OpenFlow_Header.t * Cstruct.t)

end
