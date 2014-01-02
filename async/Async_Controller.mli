open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

val start : f:('a -> VInt.t -> SDN_Types.flowTable) -> port:int -> pols:'a Pipe.Reader.t -> unit Deferred.t
