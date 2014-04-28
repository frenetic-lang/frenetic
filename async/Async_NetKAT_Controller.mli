open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

type how = [`BestEffort | `PerPacketConsistent]
val start : Async_NetKAT.app -> ?port:int -> ?update:how -> unit -> unit
