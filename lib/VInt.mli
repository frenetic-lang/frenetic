(** Variable-width integers. *)

type t =
  | Int64 of Int64.t
  | Int48 of Int64.t
  | Int32 of Int32.t
  | Int16 of int
  | Int8 of int
  | Int4 of int

(** [get_int v] raises [Invalid_argument] if [v] cannot be coerced to an [int] *)
val get_int : t -> int

(** [get_int64 v] raises [Invalid_argument] if [v] is not [Int64_ ] *)
val get_int64 : t -> Int64.t

val get_int48 : t -> Int64.t

val get_int32 : t -> Int32.t

val get_int16 : t -> int

val get_int8 : t -> int

val get_int4 : t -> int

val format : Format.formatter -> t -> unit

val get_string : t -> string
