module Field : sig
  type t [@@deriving sexp]
  val compare : t -> t -> int
  val hash : t -> int
  val as_int : t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t

end

module FieldSet : sig
  include Set.S with type elt = Field.t
  val of_list : Field.t list -> t
end

module Value : sig
  type t [@@deriving sexp]
  val compare : t -> t -> int
  val hash : t -> int
  val as_int : t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val of_string : string -> t
  val extra_val : t
end

module ValueSet : sig
  include Set.S with type elt = Value.t
  val elt_of_sexp : Sexplib.Sexp.t -> elt
  val sexp_of_elt : elt -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val of_list : Value.t list -> t
end

module SetMapF :
  functor (K : Map.OrderedType) ->
  functor (V : Set.OrderedType) -> sig
    type t
    type elt = V.t
    module Values : Set.S with type elt = elt
    type eltSet = Values.t
    type key = K.t
    val empty : t
    val add : key -> elt -> t -> t
    val is_empty : t -> bool
    val union : t -> t -> t
    val keys : t -> key list
    val find_all : key -> t -> eltSet
    val filter : (key -> elt -> bool) -> t -> t
    val to_string : t -> (key -> string -> string, unit, string) format ->
      (elt list -> string list) -> string
  end

module UnivMap : sig
  type t = SetMapF(Field)(Value).t
end

val all_fields : (unit -> FieldSet.t) ref
val all_values : (unit -> (Field.t -> ValueSet.t)) ref
(* returns true if universe is non-empty *)
val set_univ : UnivMap.t list -> bool

open Core.Std

module UnionFind(Ord: Map.Key) : sig
  type t [@@deriving sexp]
  module Class : sig
    type t [@@deriving sexp]
    val members : t -> Ord.t list
    val canonical_element : t -> Ord.t
  end
  val create : unit -> t
  val eq : t -> Ord.t -> Ord.t -> bool
  val find : t -> Ord.t -> Ord.t
  val union : t -> Ord.t -> Ord.t -> unit
  val validate : t -> unit
  val equivalence_classes : t -> Class.t list
end
