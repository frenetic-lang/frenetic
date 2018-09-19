exception Empty

open Core
open Frenetic_Decide_Util
open Frenetic_Decide_Ast

module type DerivTerm = sig

  type t [@@deriving sexp]

  module EMatrix : sig
    type t [@@deriving sexp]
    val fold : t -> init:'a -> f:('a -> point -> 'a) -> 'a
    val run : t -> point -> bool
    val compare : t -> t -> int
    val empty : t
    val intersection_empty : t -> t -> bool
    val union : t -> t -> t
  end

  module DMatrix : sig
    type t [@@deriving sexp]
    val run : t -> point -> TermSet.t
    val compare : t -> t -> int
    val equivalent : (TermSet.t -> TermSet.t -> bool) -> t -> t -> bool
    val points : t -> EMatrix.t
  end

  val make_term : TermSet.t -> t
  val get_termset : t -> TermSet.t
  val get_e : t -> EMatrix.t
  val get_d : t -> DMatrix.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
  val to_string : t -> string
end

(* module KostasDeriv : DerivTerm *)
module BDDDeriv : DerivTerm
