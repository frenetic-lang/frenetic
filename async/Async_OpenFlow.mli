open Core.Std
open Async.Std


(** By default, displays untagged info messages on stderr. *)
module Log : sig

  include Log.Global_intf

  val make_filtered_output : (string * string) list ->
    Log.Output.t

  val add_output : Log.Output.t list -> unit

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

end

(** A stage is an effectful computation that takes an environment and produces
 * zero or more results.
 * *)
module Stage : sig
  (** [('r, 'a, 'b') t] is type for stages that take an environment of type 'r,
   * and argument of type 'a, and produces values of type 'b.
   * *)
  type ('r, 'a, 'b) t = 'r -> 'a -> 'b list Deferred.t

  (** [compose s2 s1] is a stage that, when run, will run the s2 stage first,
   * and pass any results into the s1 stage. The enviornment will be threaded
   * through both stages in the same order as the stages are run.
   * *)
  val compose : ('r, 'b, 'c) t -> ('r, 'a, 'b) t -> ('r, 'a, 'c) t

  (** [s1 >=> s2] is equivalent to [compose s2 s1] *)
  val (>=>) : ('r, 'a, 'b) t -> ('r, 'b, 'c) t -> ('r, 'a, 'c) t

  (** [s2 <=< s1] is equivalent to [compose s2 s1] *)
  val (<=<) : ('r, 'b, 'c) t -> ('r, 'a, 'b) t -> ('r, 'a, 'c) t

  (** [combine s1 s2] is a stage that, when run, will pass the environment and
   * argument to both stages. The result of [combine s1 s2] is the result of
   * [s1] and [s2], concatenated.
   * *)
  val combine : ('r, 'a, 'b) t -> ('r, 'a, 'b) t -> ('r, 'a, 'b) t

  (** [s1 <|> s2] is equivalent to [combine s1 s2]. *)
  val (<|>) : ('r, 'a, 'b) t -> ('r, 'a, 'b) t -> ('r, 'a, 'b) t

  (** [local f] transforms a stage that expects an evironment type of ['r2] into
   * a stage that expects an environment type of ['r1]. It does this by using
   * [f] to recover a value of type ['r2] from a value of type ['r1].
   * *)
  val local : ('r1 -> 'r2) -> ('r2, 'a, 'b) t -> ('r1, 'a, 'b) t

  (** [run s t] turns the stage into a function that will transform a
   * [Pipe.Reader.t] that prodcues values of type ['a] to a [Pipe.Reader.t]
   * that produces values of type ['b]. In essence, [run s t] lifts the stage
   * into a pipe transformer, and uses the [t] value as the environment on each
   * invocation of the stage. *)
  val run : ('r, 'a, 'b) t -> 'r -> 'a Pipe.Reader.t -> 'b Pipe.Reader.t
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

    val echo : (t, e, e) Stage.t
    val handshake : int -> (t, e, h) Stage.t
  end

end

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

    val features : (t, e, f) Stage.t
  end

end

module OpenFlow0x04 : sig

  module Message : Message
    with type t = (OpenFlow_Header.xid * OpenFlow0x04.Message.t)

  module Controller : Platform.S
    with type m = Message.t

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
