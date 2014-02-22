open Core.Std
open Async.Std

open NetKAT_Types
open Topology


module PipeSet : Set.S
  with type Elt.t = string

exception Sequence_error of PipeSet.t * PipeSet.t

type app

type result = packet_out list * policy option
type handler = Topology.t -> event -> result Deferred.t

val create : ?pipes:PipeSet.t -> policy -> handler -> app
val create_static : policy -> app
val create_from_file : string -> app

val default : app -> policy

val run : app -> Topology.t -> event -> result Deferred.t

(** Union two apps together. *)
val union : app -> app -> app

(** Sequence two apps together. This may fail if the apps' PipeSets have a
    non-empty intersection. In that case, this function will raise a
    Sequence_error containing the two incompatible PipeSets. *)
val seq : app -> app -> app
