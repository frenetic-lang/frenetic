(** The signature for a type that can be compared and hashed *)
module type HashCmp = sig
  type t

  val hash : t -> int
  (** [hash t] assigns an interger to each value of type [t]. This assignment
      must be consistent with the {!compare} operation in the following way:

          if [compare a b = 0] then [hash a = hash b] *)

  val compare : t -> t -> int
  (** [compare a b] returns one of three values:

      {ul
      {- [0] when [a] and [b] are equal;}
      {- [1] when [a] is greater than [b]; and}
      {- [-1] when [a] is less than [b].}} *)

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
      lower bound itself. *)

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

(** Variable-Lattice-Result

    This module implements a variant of a binary decision diagrams. Rather than
    representing boolean-valued functions over boolean variables, this data
    structure represents functions that take on values in a semi-ring, and whose
    variables are assigned values from a lattice, i.e., that are partially
    ordered. *)
module Make(V:HashCmp)(L:Lattice)(R:Result) : sig

  type t
  (** The type of a decision diagram *)

  type v  = V.t * L.t
  (** The type of a variable in the decision diagram. *)
  type r = R.t
  (** The type of the result of a decision diagram *)

  type d
    = Leaf of r
    | Branch of v * t * t

  val get : d -> t
  val unget : t -> d
  val mk_branch : v -> t -> t -> t
  val mk_leaf : r -> t

  val const : r -> t
  (** [const r] creates a constant diagram out of [r]. *)

  val atom : v -> r -> r -> t
  (** [atom v t f] creates a diagram that checks the variable assignment
      [v] holds and returns the result [t] if it does hold, and the result [f]
      otherwise. *)

  val restrict : v list -> t -> t
  (** [restrict vs t] returns a diagram derived from [t] and that agrees with
      [t] when every variable assignment [v] in [vs] is true. This will eliminate
      the variables in [vs] from the diagram, if present.

      This function assumes that a variable will only appear once in the list of
      variable assignments. If the list assigns multiple values to a variable,
      then the behavior is unspecified. *)

  val peek : t -> r option
  (** [peek t] check if the diagram is a leaf node. If it is, it will return
      the value at the leaf, and [None] otherwise.

      [peek], combined with {!restrict} are useful when extracting information
      from a diagram. Through multiple applications of [restrict] the programmer
      can attempt to reduce the diagram to a value, and then use [peek] to
      extract that value. *)

  val sum : t -> t -> t
  (** [sum a b] returns the disjunction of the two diagrams. The [sum]
      operation on the [r] type is used to combine leaf nodes. *)

  val sum_generalized : (r -> r -> r) -> r -> t -> t -> t

  val prod : t -> t -> t
  (** [prod a b] returns the conjunction of the two diagrams. The [prod]
      operation on the [r] type is used to combine leaf nodes. *)

  val map_r : (r -> r) -> t -> t
  (** [map_r f t] returns a diagram with the same structure but whose leaf
      nodes have been modified according the function [f].

      This function can be used as a general form of negation. For example, if
      the [r] type were [bool], one could implement negation in the following
      way:

          [let neg = map_r (fun r -> not r)] *)

  val fold
    :  (r -> 'a)
    -> (v -> 'a -> 'a -> 'a)
    -> t
    -> 'a
  (** [fold f g t] traverses the diagram, replacing leaf nodes with
      applications of [f] to the values that they hold, and branches on
      variables with applications of [g]. *)

  (* SJS: quick and dirty -- this shouldn't be in here *)
  val to_table : t -> (v list * r) list

  val equal : t -> t -> bool
  (** [equal a b] returns whether or not the two diagrams are structurally
      equal.

      If two diagrams are structurally equal, then they represent the
      same combinatorial object. However, if two diagrams are not equal, they
      still may represent the same combinatorial object. Whether or not this is
      the case depends on they behavior of the type [v]. *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of the diagram. *)

  val clear_cache : unit -> unit
  (** [clear_cache ()] clears the internal cache of diagrams. *)

  val compressed_size : t -> int
  val uncompressed_size : t -> int

  val to_dot : t -> string
  (** [to_dot t] returns a string representation of the diagram using the DOT
      graph description language. The result of this function can be rendered
      using Graphviz or any other program that supports the DOT language. *)

  val map_values : (r -> r) -> t -> t

end

