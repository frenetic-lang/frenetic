open Core.Std
open Async.Std

open NetKAT_Types

(** [node] is an entity in the network, currently either a switch with a
    datapath id, or a host with a MAC and IPv4 address. *)
type node =
  | Switch of SDN_Types.switchId
  | Host of Packet.dlAddr * Packet.nwAddr

module Node : Network.VERTEX
  with type t = node
module Link : Network.EDGE
  with type t = unit

(** A representation of the network, with [node] as a label for vertices, and
    [unit] as labels for edges. *)
module Net : Network.NETWORK
  with module Topology.Vertex = Node
   and module Topology.Edge = Link

(** The set of pipe names that an application is listening on. *)
module PipeSet : Set.S
  with type Elt.t = string

type ('phantom, 'a) pipes = {
  pkt_out : (switchId * SDN_Types.pktOut, 'phantom) Pipe.t;
  update  : ('a, 'phantom) Pipe.t
}

type 'a send = (Pipe.Writer.phantom, 'a) pipes
type 'a recv = (Pipe.Reader.phantom, 'a) pipes

type 'a callback = event -> 'a option Deferred.t

(** A [handler] is a function that's used to both create basic reactive [app]s as
    well as run them. The [unit] argument indicates a partial application point. *)
type ('r, 'a) handler
  =  'r -> (switchId * SDN_Types.pktOut) Pipe.Writer.t -> unit -> 'a callback

(** [asycn_handler] is a function that's used to build reactive [app]s that
    are also capable of pushing asynchronous value updates. The [unit] argument
    indicates a partial application point. *)
type ('r, 'a) async_handler
  =  'r -> 'a send -> unit -> 'a callback

module Raw : sig

  (** [t] is an opaque application type.  The user can use constructors and
      combinators defined below to build up complex applications from simple
      parts. *)
  type ('r, 'a) t

  (** [create ?pipes valu handler] returns a [t] that listens to the pipes
      included in [pipes], uses [val] as the initial default value and [handler]
      as the function to handle network events. *)
  val create : ?pipes:PipeSet.t -> 'a -> ('r, 'a) handler -> ('r, 'a) t

  (** [create_async ?pipes val async_handler] returns a [t] that listens to
      the pipes included in [pipes], uses [val] as the initial value and
      [async_handler] as the function used to handle network events.

      In addition to a [pktOut] pipe, the [async_handler] is also given an ['a]
      pipe that it can use to push asychronous value updates. *)
  val create_async : ?pipes:PipeSet.t -> 'a -> ('r, 'a) async_handler -> ('r, 'a) t

  (** [create_static val] returns a static [t] that will only ever take on the
       value [val]. *)
  val create_static : 'a -> ('r, 'a) t
end

module Pred : sig
  type t = (Net.Topology.t ref, pred) Raw.t

  type handler = Net.Topology.t -> event -> pred option Deferred.t
  type async_handler = pred Pipe.Writer.t -> unit -> handler

  val create : pred -> handler -> t
  val create_async : pred -> async_handler -> t

  val create_static : pred -> t
  val create_from_string : string -> t
  val create_from_file : string -> t

  val neg : t -> t
  val conj : t -> t -> t
  val disj : t -> t -> t
end


(** [default t] returns the current value of the app

    Note that this may not be the same default value used to construct the
    application. It is the last value that the application generated in
    response to an event. *)
val default : ('r, 'a) Raw.t -> 'a

(** [run t] returns a [handler] that implements [t]. The [unit] argument
 * indicates a partial application point. *)
val run : ('r, 'a) Raw.t -> 'r -> unit -> ('a recv * (event -> unit Deferred.t))

(** [lift f t] returns a [Raw.t] that will updates its value to [b = f a]
    whenever [t] updates its value to be [a]. *)
val lift : ('a -> 'b) -> ('r, 'a) Raw.t -> ('r, 'b) Raw.t

(** [ap t1 t2] returns a [Raw.t] that will update its value whenever [t1] or
    [t2] update their value. If [t1] has the value [f] and [t2] has the value
    [a], then the value of the returned [Raw.t] will be [f a]. In other words
    this is application of function-valued [Raw.t]s.

    The [?how] optional parameter detemrines how the callbacks for each [Raw.t]
    should be executed. *)
val ap
  :  ?how:[`Sequential | `Parallel]
  -> ('r, 'a -> 'b) Raw.t
  -> ('r, 'a) Raw.t
  -> ('r, 'b) Raw.t

(** [app] is an opaque application type.  The user can use constructors and
    combinators defined below to build up complex applications from simple
    parts. *)
type app = (Net.Topology.t ref, policy) Raw.t

(** [create ?pipes pol handler] returns an [app] that listens to the pipes
    included in [pipes], uses [pol] as the initial default policy to install,
    and [handler] as the function to handle network events. *)
val create : ?pipes:PipeSet.t -> policy -> (Net.Topology.t ref, policy) handler -> app

(** [create_async ?pipes pol async_handler] returns an [app] that listens to
    the pipes included in [pipes], uses [pol] as the initial default policy to
    install, and [async_handler] as the function used to handle network events.

    NOTE: It's assumed that [handler nib send () e] will not become determined
    until either an [Event(pol)] or [EventNoop] [update] has been written to
    the [send.update] pipe. It is very important that you satisfy this
    assumption! Not doing so will cause your application to lock up the
    controller. *)
val create_async : ?pipes:PipeSet.t -> policy -> (Net.Topology.t ref, policy) async_handler -> app

(** [create_static pol] returns a static app for the NetKAT syntax tree [pol] *)
val create_static : policy -> app

(** [create_from_string str] returns a static app for the NetKAT policy [str] *)
val create_from_string : string -> app

(** [create_from_file f] returns a static app from the NetKAT policy contained
    in the file [f]. *)
val create_from_file : string -> app

(** [union ?how app1 app2] returns the union of [app1] and [app2].

    The returned app listens on the union of [app1] and [app2]'s [PipeSet.t]s,
    distributes events across the two apps, unions reactive updates to policies,
    and concatenates the list of [(switchId * pktOut)]s that they produce.

    If the app produce side effects, you may want to control the order of their
    execution using the optional [how] argument to sequence them from left to
    right, or to have them run concurrently.
    *)
val union : ?how:[ `Parallel | `Sequential ] -> app -> app -> app

exception Sequence_error of PipeSet.t * PipeSet.t

(** [seq app1 app2] returns the sequence of [app1] and [app2].

    The returned app listens on the disjoint union of [app1] and [app2]'s
    [PipeSet.t]s. If the two [PipeSet.t]s are not disjoint, then this function
    will raise a [Sequence_error]. If they are disjoint, then the returned app
    will distribute events across the two apps, sequence reactive updates to
    policies, and concatenates the list of [packet_outs] that they produce.  *)
val seq : app -> app -> app

(** [guard pred app] returns an app that is equivalent to [app] except it will
    drop packets that do not satisfy [pred]. *)
val guard  : pred   -> app -> app
val guard' : Pred.t -> app -> app

(** Lift a predicate to the [app] type. [filter p] returns an app that filters
    packets according to the predicate app. *)
val filter  : pred   -> app
val filter' : Pred.t -> app

(** [slice pred app1 app2] returns an application where packets that
    satisfy [pred] will be handled by [app1] and packets that do not satisfy
    [pred] will be handled by [app2].

    The returned application will enforce the pipes that [app1] and [app2]
    listen to, so if a packet matches [pred] but is at a pipe that [app1] is not
    listening on, the packet will be dropped. *)
val slice  : pred   -> app -> app -> app
val slice' : Pred.t -> app -> app -> app
