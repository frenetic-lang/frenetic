open Core

module Make(Dom: Vlr.HashCmp) : sig
  type t [@@deriving sexp, hash, compare, eq]

  val to_string : t -> string

  val empty : t
  val is_empty : t -> bool

  val dirac : Dom.t -> t

  (** pointwise sum *)
  val sum : t -> t -> t

  (** pushforward along f of the product distribution *)
  val prod_with : t -> t -> f:(Dom.t -> Dom.t -> Dom.t) -> t
end
