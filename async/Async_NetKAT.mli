open Core.Std
open Async.Std

open NetKAT_Types
open Async_OpenFlow.Net


(** [app] is an opaque application type.  The user can use constructors and
    combinators defined below to build up complex applications from simple
    parts. *)
type app

(** The set of pipe names that an application is listening on. *)
module PipeSet : Set.S
  with type Elt.t = string

(** [result] is the result of a handler, a list of [packet_outs] to send to
    switches, and an optional policy update. *)
type result = packet_out list * policy option

(** [handler] is a function that's used to both create basic reactive [app]s as
    well as run them. *)
type handler = Topology.t -> event -> result Deferred.t

(** [create ?pipes pol handler] returns an [app] that listens to the pipes
    included in [pipes], uses [pol] as the initial default policy to install,
    and [handler] as the function to handle network events. *)
val create : ?pipes:PipeSet.t -> policy -> handler -> app

(** [create_static pol] returns a static app for the NetKAT syntax tree [pol] *)
val create_static : policy -> app

(** [create_from_file f] returns a static app from the NetKAT policy contained
    in the file [f]. *)
val create_from_file : string -> app

(** [default app] returns the current default policy for the app.

    Note that this may not be the same default policy that the user used to
    construct the application. It is the last policy that the application
    generated in response to an event.  *)
val default : app -> policy

(** [run app] returns a [handler] that implements [app]. *)
val run : app -> Topology.t -> event -> result Deferred.t

(** [union ?how app1 app2] returns the union of [app1] and [app2].

    The returned app listens on the union of [app1] and [app2]'s [PipeSet.t]s,
    distributes events across the two apps, unions reactive updates to policies,
    and concatenates the list of [packet_out]s that they produce.

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
