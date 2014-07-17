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

(** [app] is an opaque application type.  The user can use constructors and
    combinators defined below to build up complex applications from simple
    parts. *)
type app

(** [update] represents a policy upate for asychronous applications. When
    responding to a network event, applications must write either an
    [Event(pol)] or [EventNoop] [update] before allow the returned
    [unit Deferred.t] to become determined. A good way to do this is to have
    that write be in the the return position of your event callback.

    All asychronous policy updates should exclusively use the [Async(pol)]
    variant of [update]. *)
type update = Raw_app.update

type send = {
  pkt_out : (switchId * SDN_Types.pktOut) Pipe.Writer.t;
  update  : policy Pipe.Writer.t
}

type recv = {
  pkt_out : (switchId * SDN_Types.pktOut) Pipe.Reader.t;
  update  : policy Pipe.Reader.t
}

(** The set of pipe names that an application is listening on. *)
module PipeSet : Set.S
  with type Elt.t = string

(** [result] is the result of a handler, which is just an optional policy. *)
type result = policy option

(** A [handler] is a function that's used to both create basic reactive [app]s as
    well as run them. The [unit] argument indicates a partial application point. *)
type handler
  = Net.Topology.t ref
  -> (switchId * SDN_Types.pktOut) Pipe.Writer.t
  -> unit
  -> event
  -> result Deferred.t

(** [asycn_handler] is a function that's used to build reactive [app]s that
    are also capable of pushing asynchronous policy updates. The [unit] argument
    indicates a partial application point. *)
type async_handler
  = Net.Topology.t ref
  -> send
  -> unit
  -> event -> result Deferred.t

(** [create ?pipes pol handler] returns an [app] that listens to the pipes
    included in [pipes], uses [pol] as the initial default policy to install,
    and [handler] as the function to handle network events. *)
val create : ?pipes:PipeSet.t -> policy -> handler -> app

(** [create_async ?pipes pol async_handler] returns an [app] that listens to
    the pipes included in [pipes], uses [pol] as the initial default policy to
    install, and [async_handler] as the function used to handle network events.

    NOTE: It's assumed that [handler nib send () e] will not become determined
    until either an [Event(pol)] or [EventNoop] [update] has been written to
    the [send.update] pipe. It is very important that you satisfy this
    assumption! Not doing so will cause your application to lock up the
    controller. *)
val create_async : ?pipes:PipeSet.t -> policy -> async_handler -> app

(** [create_static pol] returns a static app for the NetKAT syntax tree [pol] *)
val create_static : policy -> app

(** [create_from_string str] returns a static app for the NetKAT policy [str] *)
val create_from_string : string -> app

(** [create_from_file f] returns a static app from the NetKAT policy contained
    in the file [f]. *)
val create_from_file : string -> app

(** [default app] returns the current default policy for the app.

    Note that this may not be the same default policy that the user used to
    construct the application. It is the last policy that the application
    generated in response to an event.  *)
val default : app -> policy

(** [run app] returns a [handler] that implements [app]. The [unit] argument
 * indicates a partial application point. *)
val run
  :  app
  -> Net.Topology.t ref
  -> unit
  -> (recv * (event -> unit Deferred.t))

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

(** [slice pred app1 app2] returns an application where packets that
 * satisfy [pred] will be handled by [app1] and packets that do not satisfy
 * [pred] will be handled by [app2]. In addition the returned application will
 * enforce the pipes that [app1] and [app2] listen to, so if a packet matches
 * [pred] but is at a pipe that [app1] is not listening on, the packet will be
 * dropped.
 *)
val slice : pred -> app -> app -> app
