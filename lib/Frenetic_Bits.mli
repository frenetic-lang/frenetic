(** Bit manipulation for integers

  Though Std has bitwise and's, or's and shifting, Frenetic needs some more 
  bit-specific primitives.  Other libraries like bitv have them, but they require 
  converting back and forth to abstract data types ... this approach is faster and
  less complex.    
*)

(* {1 Bit-Level Operations} *)

(** [clear_bit n x] sets bit [n] in integer [x] to 0 *)
val clear_bit : int -> int32 -> int32

(** [set_bit n x] sets bit [n ]in integer [x] to 1 *)
val set_bit : int -> int32 -> int32

(** [bit x n set_bit] sets bit [n] of integer [x] to 1 if [set_bit]=true, 0 otherwise. *)
val bit : int32 -> int -> bool -> int32

(** [test_bit n x] returns true if bit [n] in integer [x] is 1 *)
val test_bit : int -> int32 -> bool

(* {1 Byte-Extraction Operations} *)

(** [get_byte32 x n] returns the [n]'th byte of [int32] [x].   The least significant
  byte is 0, the most is 3.  Raises error if [n] is not in 0..3  *)
val get_byte32 : int32 -> int -> int

(** [get_byte x n] returns the [n]'th byte of [int64] [x].  The least significant byte is 0,
  the most is 7.  Raises error if [n] is not in 0..7  *)
val get_byte : int64 -> int -> int
