type t [@@deriving eq, sexp, compare, hash]

val zero : t
val one : t

val min : t -> t -> t

val (/) : t -> t -> t
val ( * ) : t -> t -> t
val (+) : t -> t -> t
val (-) : t -> t -> t
val (//) : int -> int -> t

val pp : Format.formatter -> t -> unit
val to_string : t -> string
val of_string : string -> t

val to_q : t -> Q.t
val to_float : t -> float
val of_float : float -> t
