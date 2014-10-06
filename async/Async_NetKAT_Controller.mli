open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

type t

val start
  : Async_NetKAT.Policy.t
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> ?policy_queue_size:int
  -> unit -> t Deferred.t

val enable_discovery  : t -> unit Deferred.t
val disable_discovery : t -> unit Deferred.t
