open Core

module Make(Dom: Vlr.HashCmp) : sig
  type t [@@deriving sexp, hash, compare, eq]

  val to_string : t -> string

  val empty : t
  val is_empty : t -> bool

  val dirac : Dom.t -> t

  val sum : t -> t -> t
end
