open Core

module Make(Dom: Vlr.HashCmp) : sig
  type t [@@deriving sexp, hash, compare, eq]

  val support : t -> Dom.t list
  val mass : t -> Prob.t

  val to_string : t -> string
  val to_alist : t -> (Dom.t * Prob.t) list

  val empty : t
  val is_empty : t -> bool

  val dirac : Dom.t -> t
  val is_dirac: t -> Dom.t option

  (* DO NOT EXPOSE: we want full distributions only *)
  (* val scale : t -> scalar:Prob.t -> t *)

  (** pointwise sum *)
  (* SJS: do not expose *)
  (* val sum : t -> t -> t *)

  (* this is safe to expose *)
  val convex_sum : t -> Prob.t -> t -> t

  (* SJS: Unsafe!! *)
  val unsafe_add : t -> Prob.t -> Dom.t -> t

  (** pushforward along f *)
  val pushforward : t -> f:(Dom.t -> Dom.t) -> t

  (** pushforward along f of the product distribution *)
  val prod_with : t -> t -> f:(Dom.t -> Dom.t -> Dom.t) -> t

  (** normalize probabilities so they sum up to 1 *)
  val unsafe_normalize : t -> t

  (** probability that predicate holds *)
  val prob : t -> f:(Dom.t -> bool) -> Prob.t

  (** expected value *)
  val expectation : t -> f:(Dom.t -> Q.t) -> Q.t

  (** Monad primitives *)
  val bind  : t -> (Dom.t -> t) -> t
  val (>>=) : t -> (Dom.t -> t) -> t
  val return : Dom.t -> t

  (** UNSAFE  *)
  val to_alist : t -> (Dom.t * Prob.t) list
  val of_alist_exn : (Dom.t * Prob.t) list -> t
end
