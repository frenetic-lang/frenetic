open Core.Std
open Async.Std

open NetKAT_Types
open Topology


module PipeSet : Set.S
  with type Elt.t = string

exception Sequence_error of PipeSet.t * PipeSet.t

type app
type handler = Topology.t -> event -> result Deferred.t

val create : ?pipes:PipeSet.t -> policy -> handler -> app
val create_static : policy -> app
val create_from_file : string -> app

val run : app -> Topology.t -> event -> result Deferred.t

val union : app -> app -> app
val seq : app -> app -> app
