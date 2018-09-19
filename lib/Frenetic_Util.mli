(** Utility functions for Frenetic.  These are used in multiple modules and don't have an obvious home type. *)

open Core 

(** Given a user-defined formatter and a type, make a string.  This is a lot like sprintf, and encourages sharing the formatting code. *)
val make_string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

