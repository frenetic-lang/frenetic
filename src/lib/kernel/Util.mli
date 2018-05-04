(** Utility functions for Frenetic_kernel.  These are used in multiple modules and don't have an obvious home type. *)

open Core

(** Given a user-defined formatter and a type, make a string.  This is a lot like sprintf, and encourages sharing the formatting code. *)
val make_string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

(** register pretty printer for located exceptions Location.exception *)
val pp_exceptions : unit -> unit

val map_fst : ('a * 'b) list -> f:('a -> 'c) -> ('c * 'b) list
val map_snd : ('a * 'b) list -> f:( 'b -> 'c) -> ('a * 'c) list

(* Compiles provided dot-string using `graphviz` and dumps the output into a
   temporary file. The temporary file is returned.
   Requires graphviz to be installed and on the PATH.

   ## Optional parameters
   * `format`: Output format graphviz should use, e.g., "pdf", "svg", "png", ...
   * `title`: An optional title.
   * `engine`: The graphviz engine that should be used. Defaults to "dot".
 *)
val compile_dot : ?format:string -> ?engine:string -> ?title:string -> string -> string


(* Compiles provided dot-string using `graphviz`, then opens the resulting file
   using the associated default application. The optional parameters `format`,
   `title`, and `engine` have the same semantics as for the `compile_dot`
   function above.

   Requires graphviz to be installed and on the PATH.
 *)
val show_dot : ?format:string -> ?title:string -> ?engine:string -> string -> unit

(* Like `show_dot`, but takes a file name (containing dot code) instead of taking
   dot code directly.
*)
val show_dot_file : ?format:string -> ?title:string -> ?engine:string -> string -> unit
