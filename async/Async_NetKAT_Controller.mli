open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

val start
  : Async_NetKAT.Policy.t
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> ?policy_queue_size:int
  -> unit -> unit
