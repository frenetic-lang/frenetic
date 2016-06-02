open Core.Std
open Frenetic_Decide_Util

module FieldMap : sig
  include Map.S with type Key.t = Field.t
end

type packet = Value.t FieldMap.t [@@deriving sexp, compare]
type point = packet * packet [@@deriving sexp, compare]

val packet_to_string : packet -> string
val point_to_string : point -> string

module PacketSet : sig
  include Set.S with type Elt.t = packet
end


module rec Term : sig
  type t = term Hashcons.hash_consed and
  term =
    | Assg of Field.t * Value.t
    | Test of Field.t * Value.t
    | Dup
    | Plus of TermSet.t
    | Times of t list
    | Not of t
    | Star of t
    | Zero
    | One

  val compare: t -> t -> int
  val compare_term: term -> term -> int
  val sexp_of_t: t -> Sexplib.Sexp.t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_term: term -> Sexplib.Sexp.t
  val term_of_sexp: Sexplib.Sexp.t -> term

  val assg : Field.t -> Value.t -> t
  val test : Field.t -> Value.t -> t
  val dup : t
  val plus : TermSet.t -> t
  val times : t list -> t
  val not : t -> t
  val star : t -> t
  val zero : t
  val one : t

  val equal : t -> t -> bool
  val compare_ab : t -> point -> bool
  val eval : t -> packet -> PacketSet.t
  val to_string : t -> string
  val values : t -> UnivMap.t
  val size : t -> int
end and TermSet : sig
  include Set.S with type Elt.t = Term.t
  val to_string : t -> string
end

module Formula : sig
  type t =
    | Eq of Term.t * Term.t
    | Le of Term.t * Term.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val terms : t -> Term.t * Term.t
end
