open Core

module Make(Dom: Vlr.HashCmp) : sig
  type t [@@deriving sexp, hash, compare, eq]

  val support : t -> Dom.t list

  val to_string : t -> string
  val to_alist : t -> (Dom.t * Prob.t) list

  val empty : t
  val is_empty : t -> bool

  val dirac : ?weight:Prob.t -> Dom.t -> t

  val scale : t -> scalar:Prob.t -> t

  (** pointwise sum *)
  val sum : t -> t -> t

  val add : t -> Prob.t -> Dom.t -> t

  (** pushforward along f *)
  val pushforward : t -> f:(Dom.t -> Dom.t) -> t

  (** pushforward along f of the product distribution *)
  val prod_with : t -> t -> f:(Dom.t -> Dom.t -> Dom.t) -> t
end
