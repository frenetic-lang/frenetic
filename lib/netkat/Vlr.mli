open Core

(** The signature for a type that can be compared and hashed *)
module type HashCmp = sig
  include Hashtbl.Key

  val to_string : t -> string
  (** [to_string t] returns a string representation of the value. *)
end

(** The signature for a type that has a lattice structure. *)
module type Lattice = sig
  include HashCmp

  val subset_eq : t -> t -> bool
  (** [subset_eq a b] returns [true] if [a] and [b] in the partial ordering of
      the lattice. This relation should be reflexive, transitive, and
      antisymmetric. *)

  val meet : ?tight:bool -> t -> t -> t option
  (** [meet ~tight a b] returns the greatest lower bound of the elements [a]
      and [b], if one exists. This operation should be associative, commutative,
      and idempotent. If the optional argument [tight] is set to [true], then
      the result [c] should satisfy the additional property:

          ∀x, [subset_eq c x] <=> [subset_eq a x || subset_eq b x || equal c x].

      In other words, elements related to the greatest lower bound should be
      related transitively through [a] and [b], or be equal to the greatest
      lower bound itself.

      TODO: tightness doesn't seem to be used anywhere in Frenetic, and can probably
      be removed.  *)

  val join : ?tight:bool -> t -> t -> t option
  (** [join ~tight a b] returns the least upper bound of the elements [a] and
      [b], if one exists. This operation should be associative, commutative, and
      idempotent. If the optional argument [tight] is set to [true], then the
      result [c] should satisfy the additional property:

          ∀x, [subset_eq x c] <=> [subset_eq x a || subset_eq x b || equal x c].

      In other words, elements related to the least upper bound should be
      related transitively through [a] and [b], or be equal to the least upper
      bound itself. *)

end

(** The type for a result that has a semi-ring structure *)
module type Result = sig
  include HashCmp

  val sum : t -> t -> t
  (** An associative and commutative binary operation over the type [t]. The
      following should hold:

      {ul
      {- [sum a (sum b c)] = [sum (sum a b) c].}
      {- [sum a b] = [sum b a].}} *)

  val prod : t -> t -> t
  (** An associative binary operation over the type [t]. The following should
      hold:

      {ul
      {- [prod a (prod b c)] = [prod (prod a b) c]. }
      {- [prod a (sum b c)] = [sum (prod a b) (prod a c)].}} *)

  val one : t
  (** The identity for the [prod] operation. The following should hold:

      {ul
      {- [prod one t] = [t].}
      {- [prod t one] = [t].}}

      As an example, if [t] where the type [bool] and [prod] and [sum] were [&&]
      and [||], respectively, then [one] should be the value [true]. *)

  val zero : t
  (** The identity for the [sum] operation. The following should hold:

      {ul
      {- [sum zero t] = [t].}
      {- [sum t zero] = [t].}
      {- [prod zero t] = [zero].}
      {- [prod t zero] = [zero].}}

      As an example, if [t] where the type [bool] and [prod] and [sum] were [&&]
      and [||], respectively, then [zero] should be the value [false]. *)
end

module IntPairTbl : Hashtbl.S with type key = (int * int)

(** Variable-Lattice-Result

    This module implements a variant of a binary decision diagrams. Rather than
    representing boolean-valued functions over boolean variables, this data
    structure represents functions that take on values in a semi-ring, and whose
    variables are assigned values from a lattice, i.e., that are partially
    ordered. *)
module Make(V:HashCmp)(L:Lattice)(R:Result) : sig

  type t = private int
  (** A decision diagram index.  All diagrams and subdiagrams within it are given an
  index.  You can convert this to a tree with [unget], and from a tree with [get]. *)

  type v = V.t * L.t
  (** The type of a variable in the decision diagram. *)

  type r = R.t
  (** The type of the result of a decision diagram *)

  type d
    = private
    | Leaf of r
    | Branch of v * t * t
  (* A tree structure representing the decision diagram. The [Leaf] variant
   * represents a constant function. The [Branch(v, l, t, f)] represents an
   * if-then-else. When variable [v] takes on the value [l], then [t] should
   * hold. Otherwise, [f] should hold.
   *
   * [Branch] nodes appear in an order determined first by the total order on
   * the [V.t] value with with ties broken by the total order on [L.t]. The
   * least such pair should appear at the root of the diagram, with each child
   * nodes being strictly greater than their parent node. This invariant is
   * important both for efficiency and correctness.
   * *)

  module Tbl : Hashtbl.S with type key = t
  module BinTbl : Hashtbl.S with type key = (t * t)

  val get : d -> t
  (* Given a tree structure, return the cache index for it *)

  val unget : t -> d

  val get_uid : t -> int (* get_uid t is equivalent to (t : t :> int) *)

  val drop : t (* zero *)
  (** [drop] returns the leaf for a drop operation, which is always present as a leaf node *)

  val id : t (* one *)
  (** [id] returns the leaf for the identity operation, which is always present as a leaf node *)

  val const : r -> t
  (** [const r] creates a constant diagram out of [r]. It's essentially a leaf node with a constant.  *)

  val atom : v -> r -> r -> t
  (** [atom v t f] creates a diagram that checks the variable assignment
      [v] holds and returns the result [t] if it does hold, and the result [f]
      otherwise. *)

  val cond : v -> t -> t -> t
  (** [cond v t f] creates a diagram with pattern v, true-branch t and false-branch f. *)

  val unchecked_cond : v -> t -> t -> t
  (** Unsafe!! [unchecked_cond v t f] behaves like [cond v t f], but always puts the pattern [v]
      in the root node, without ensuring the FDD-ordering invariant is enforced. Only use this if you know what you are doing! *)


  val restrict : v list -> t -> t
  (** [restrict vs t] returns a diagram derived from [t] and that agrees with
      [t] when every variable assignment [v] in [vs] is true. This will eliminate
      the variables in [vs] from the diagram, if present.

      This function assumes that a variable will only appear once in the list of
      variable assignments. If the list assigns multiple values to a variable,
      then the behavior is unspecified. *)

  val sum : t -> t -> t
  (** [sum a b] returns the disjunction of the two diagrams. The [sum]
      operation on the [r] type is used to combine leaf nodes. *)

  val prod : t -> t -> t
  (** [prod a b] returns the conjunction of the two diagrams. The [prod]
      operation on the [r] type is used to combine leaf nodes. *)

  val map : f:(r -> t) -> g:(v -> t -> t -> t) -> t -> t
  (** [map f h t] traverses t in post order and first maps the leaves using
      f, and then the internal nodes using h, producing a modified diagram. *)

  val dp_map : f:(r -> t) -> g:(v -> t -> t -> t) -> t
             -> find_or_add:(t -> default:(unit -> t) -> t)
             -> t
  (** [dp_map f h cache t] is equal to [map f h t], but uses [cache] for memoization *)

  val map_r : f:(r -> r) -> t -> t
  (** [map_r f t] returns a diagram with the same structure but whose leaf
      nodes have been modified according the function [f].

      This function can be used as a general form of negation. For example, if
      the [r] type were [bool], one could implement negation in the following
      way:

          [let neg = map_r (fun r -> not r)] *)

  val fold
    :  f:(r -> 'a)
    -> g:(v -> 'a -> 'a -> 'a)
    -> t
    -> 'a
  (** [fold f g t] traverses the diagram, replacing leaf nodes with
      applications of [f] to the values that they hold, and branches on
      variables with applications of [g]. *)

  val equal : t -> t -> bool
  (** [equal a b] returns whether or not the two diagrams are structurally
      equal.
      If two diagrams are structurally equal, then they represent the
      same combinatorial object. However, if two diagrams are not equal, they
      still may represent the same combinatorial object. Whether or not this is
      the case depends on they behavior of the type [v]. *)

  val sum : t -> t -> t
  (** [sum a b] returns the disjunction of the two diagrams. The [sum]
      operation on the [r] type is used to combine leaf nodes. *)

  val prod : t -> t -> t
  (** [prod a b] returns the conjunction of the two diagrams. The [prod]
      operation on the [r] type is used to combine leaf nodes. *)

  val compare : t -> t -> int

  val to_string : t -> string
  (** [to_string t] returns a string representation of the diagram. *)

  val clear_cache : preserve:Int.Set.t -> unit
  (** [clear_cache ()] clears the internal cache of diagrams. *)

  val compressed_size : t -> int
  (** [compressed_size t] returns the number of nodes in the diagram, duplicates not counted *)

  val uncompressed_size : t -> int
  (** [uncompressed_size t] returns the number of nodes in the diagram, duplicates counted *)

  val to_dot : t -> string
  (** [to_dot t] returns a string representation of the diagram using the DOT
      graph description language. The result of this function can be rendered
      using Graphviz or any other program that supports the DOT language. *)

  val refs : t -> Int.Set.t
  (** [refs t] returns set of subdiagrams in this diagram. *)
end
