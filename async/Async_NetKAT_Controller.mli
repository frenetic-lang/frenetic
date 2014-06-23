open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

val start
  : Async_NetKAT.app
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> unit -> unit
