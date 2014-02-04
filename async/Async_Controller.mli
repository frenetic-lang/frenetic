open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

val start
  :  f:('a -> SDN_Types.switchId -> SDN_Types.flowTable)
  -> port:int
  -> init_pol:'a
  -> pols:'a Pipe.Reader.t
  -> unit Deferred.t

val start_static
  :  f:('a -> SDN_Types.switchId -> SDN_Types.flowTable)
  -> port:int
  -> pol:'a
  -> unit Deferred.t
