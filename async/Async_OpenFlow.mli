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

    type e = [
      | `Connect of Client_id.t
      | `Disconnect of Client_id.t * Sexp.t
      | `Message of Client_id.t * m
    ]

    val create
      :  ?max_pending_connections:int
      -> ?verbose:bool
      -> ?log_disconnects:bool
      -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
      -> port:int
      -> unit
      -> t Deferred.t

    val listen : t -> e Pipe.Reader.t

    val close : t -> Client_id.t -> unit

    val has_client_id : t -> Client_id.t -> bool

    val send
      :  t
      -> Client_id.t
      -> m
      -> [ `Drop of exn | `Sent of Time.t ] Deferred.t

    val send_to_all : t -> m -> unit

    val client_addr_port
      :  t
      -> Client_id.t
      -> (Unix.Inet_addr.t * int) option

    val listening_port : t -> int

  end

  module Make(Message : Message) : S with type m = Message.t

  module Trans : sig
    type ('t, 'a, 'b) stage = 't -> 'a -> 'b list Deferred.t

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

  (* XXX(seliopou): Due to the way that this was implemented, it is currently
   * not clear how to expose the event type of the Chunk controller. Right now,
   * it remains abstract, and therefore the Chunk controller is mostly (pretty
   * much entirely) useless.
   * *)
  module Controller : sig
    include Platform.S
      with type m = Message.t

    type h = [
      | `Connect of Client_id.t * int
      | `Disconnect of Client_id.t * Sexp.t
      | `Message of Client_id.t * m
    ]

    val echo : (t, e, e) Platform.Trans.stage
    val handshake : int -> (t, e, h) Platform.Trans.stage
  end

end

module Node : Network.VERTEX
module Edge : Network.EDGE
  with type t = unit

module Net : Network.NETWORK
  with module Topology.Vertex = Node
   and module Topology.Edge = Edge

module OpenFlow0x01 : sig

  module Message : Message
    with type t = (OpenFlow_Header.xid * OpenFlow0x01.Message.t)

  module Controller : sig
    include Platform.S
      with type m = Message.t

    type f = [
      | `Connect of Client_id.t * OpenFlow0x01.SwitchFeatures.t
      | `Disconnect of Client_id.t * SDN_Types.switchId * Sexp.t
      | `Message of Client_id.t * m
    ]

    val switch_id_of_client : t -> Client_id.t -> SDN_Types.switchId
    val client_id_of_switch : t -> SDN_Types.switchId -> Client_id.t

    val nib : t -> Net.Topology.t

    val features : (t, e, f) Platform.Trans.stage
    val topology : (t, f, f) Platform.Trans.stage
    val switch_topology : (t, f, f) Platform.Trans.stage
    val host_discovery  : (t, f, f) Platform.Trans.stage
  end

  val chunk_conv
    :  Chunk.Message.t Pipe.Reader.t * Chunk.Message.t Pipe.Writer.t
    -> [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Reader.t * 
       [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Writer.t

end

module OpenFlow0x04 : sig

  module Message : Message
    with type t = (OpenFlow_Header.xid * OpenFlow0x04.Message.t)

  module Controller : Platform.S
    with type m = Message.t

  val chunk_conv
    :  Chunk.Message.t Pipe.Reader.t * Chunk.Message.t Pipe.Writer.t
    -> [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Reader.t * 
       [`Ok of Message.t | `Chunk of Chunk.Message.t] Pipe.Writer.t

end

module Highlevel : sig
  type t

  val create
    :  ?max_pending_connections:int
    -> ?verbose:bool (** default is [false] *)
    -> ?log_disconnects:bool (** default is [true] *)
    -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
    -> port:int
    -> unit
    -> t Deferred.t
  val accept_switches : t -> SDN_Types.switchFeatures Pipe.Reader.t

  val setup_flow_table
    :  t
    -> SDN_Types.switchId
    -> SDN_Types.flowTable
    -> unit Deferred.t
end
