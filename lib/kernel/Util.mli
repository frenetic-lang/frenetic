(** Utility functions for Frenetic_kernel.  These are used in multiple modules and don't have an obvious home type. *)

open Core

(** Given a user-defined formatter and a type, make a string.  This is a lot like sprintf, and encourages sharing the formatting code. *)
val make_string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

(** register pretty printer for located exceptions Location.exception *)
val pp_exceptions : unit -> unit

val map_fst : ('a * 'b) list -> f:('a -> 'c) -> ('c * 'b) list
val map_snd : ('a * 'b) list -> f:( 'b -> 'c) -> ('a * 'c) list
