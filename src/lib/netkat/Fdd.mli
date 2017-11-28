(** Forwarding Decision Diagrams

  FDD's are an extension of Binary Decision Diagrams (BDD's).  NetKAT predicates,
  which are mostly based on OpenFlow matches, are just binary formulas after all.
  An FDD extends a BDD by (1) using OpenFlow actions at the leaves instead of binary
  values True and False (2) using complete header matches instead of individual bit
  matches.

  As in BDD's, field ordering is important to prevent combinatorial explosions.
  Fortunately we can exploit what goes in real FDD's to get decent
  heuristics.  Nevertheless, the field ordering is part of the data structure and
  can also be set manually by the programming through compiler options.

  Basically, the flow goes: you turn NetKAT into an FDD, then an FDD into a Flow Table.
  See paper "A Fast Compiler for NetKAT" (http://www.cs.cornell.edu/~jnfoster/papers/netkat-compiler.pdf)
  for details and theory behind it.  FDD's are nice because you can operate on them
  using the full well-established theory of BDD's.
*)

open Core

module Field : sig

  (** Fields are analogous to binary variables in a BDD.
   These are pretty much the same as matchable fields in OpenFlow.  To this list, we
   add VSwitch, VPort and VFabric for the virtual compiler.  These are not actual matchable
   fields in OpenFlow, but are converted to Switch and Port in the compilation process.

   The constructors in type t are listed in the default order, which is acceptable for many
   NetKAT programs.

   This module implements the the [HashCmp] signature from the Vlr package, so it
   becomes the "V" in VLR. *)
  type t
    = Switch
    | Location
    | From
    | AbstractLoc
    | VSwitch
    | VPort
    | Vlan
    | VlanPcp
    (* SJS: for simplicity, support only up to 5 meta fields for now *)
    | Meta0
    | Meta1
    | Meta2
    | Meta3
    | Meta4
    | EthType
    | IPProto
    | EthSrc
    | EthDst
    | IP4Src
    | IP4Dst
    | TCPSrcPort
    | TCPDstPort
    | VFabric
    [@@deriving sexp, enumerate]
  type field = t
  include Vlr.HashCmp with type t := t

  (** environment, mapping meta field identifiers to static FDD fields, and,
      for convenience, the initializer and mutability of the identifier *)
  module Env : sig
    type t
    val empty : t
    exception Full

    (** may raise Full *)
    val add : t -> string -> Syntax.meta_init -> bool -> t

    (** may raise Not_found *)
    val lookup : t -> string -> field * (Syntax.meta_init * bool)
  end

  (** [all] returns the default field field ordering *)
  val all : t list

  (** [compare f1 f2] compares two fields in the current ordering in the usual way. *)
  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  (** [of_hv header_value] converts a NetKAT header_value pair to a field *)
  val of_hv : ?env:Env.t -> Syntax.header_val -> t

  (** [of_string str] converts a field string to an abstract field.  Throws an exception for unrecognized strings. *)
  val of_string : string -> t

  (** [to_string field] returns a human-readable representation *)
  val to_string : t -> string

  (** [set_order field_list] sets the field ordering to the given list.  Any non-listed fields are given low priority in random order. *)
  val set_order : t list -> unit

  (** [get_order] returns the current field ordering.  All fields are returned. *)
  val get_order : unit -> t list

  (** [auto_order policy] heurisitically determines the field ordering given a policy *)
  val auto_order : Syntax.policy -> unit
end

module Value : sig

  (** In a BDD, each node is an implicit predicate, "variable = true".  In a FDD, each node is a test of a
  field against a particular value, as in EthSrc = "FE:89:00:12:34:12".  The edges are either true or false
  just like in a BDD.  But we also use values in modifcations - as in "port := 2".  Note only Const and Mask
  are really used by OpenFlow.  Both  Pipe and Query are translated to the pseudoport Controller.

   Each packet field can take on a certain range of values that in general have a lattice structure.
   This sometimes enables multiple tests on fields to be compressed into a single test. This module implements the [Lattice]
   signature from Vlr.

   All integer bit widths are represented by an
      [Int64.t] and will be cast to the appropriate bit width for use during
      final translation to flowtables.

      A simple bitmask variant is also supported. [Mask(n, m)] indicates that
      the first [m] bits of the value [n] are fixed, while the rest should be
      treated as wildcards.

      Because this is a big union of possible value types, it's possible for the
      programmer to construct [(Field.t, Value.t)] pairs that do not make any
      sense, e.g., [(Field.EthSrc, Value.Pipe "learn")]. This will be detected
      during flowtable generation, though the syntax of the NetKAT language will
      prevent programs from generating these ill-formed predicates.  *)

  type t =
      Const of Int64.t
    | Mask of Int64.t * int
    | AbstractLocation of string
    | Pipe of string
    | Query of string
    (* TODO(grouptable): HACK, should only be able to fast fail on ports.
     * Put this somewhere else *)
    | FastFail of Int32.t list
    [@@deriving sexp]

  include Vlr.Lattice with type t := t
  val compare : t -> t -> int
  val equal : t -> t -> bool

  val to_string : t -> string

  (** [of_int i]  converts an integer to a Const value *)
  val of_int : int -> t

  (** [of_int64 i]  converts a 64-bit integer to a Const value *)
  val of_int64 : int64 -> t

  (** [to_int64_exn value] returns just the integer for Const values, or an exception otherwise *)
  val to_int64_exn : t -> int64
end

exception FieldValue_mismatch of Field.t * Value.t

module Pattern : sig
  (* A Pattern is a predicate - "switch = 2" used as a node value in an FDD.  Note this is a lot like
  Syntax.header_val, uses a tuple instead of constructors.  Not sure why ... maybe it's easier
  to treat them uniformly.

  *)
  type t = Field.t * Value.t

  (** [compare p1 p2] compares two patterns - the field ordering is used for differing fields, and the natural order of integers for
  patterns with the same field *)
  val compare : t -> t -> int

  val equal : t -> t -> bool

  (** [of_hv header_value] converts a NetKAT header_value pair to a pattern *)
  val of_hv : ?env:Field.Env.t -> Syntax.header_val -> t

  (** [to_hv p] converts a pattern to a NetKAT header_value pair *)
  val to_hv : t -> Syntax.header_val

  (** [to_pred p] converts a pattern to a NetKAT predicate *)
  val to_pred : t -> Syntax.pred

  (* [to_sdn p] Converts a [Pattern.t] into a function that will modify a [SDN.Pattern.t]
    to check the condition represented by the [Pattern.t].  This function is used to glue
    OpenFlow match patterns into a complete match spec.  *)
  val to_sdn : t -> Frenetic_kernel.OpenFlow.Pattern.t -> Frenetic_kernel.OpenFlow.Pattern.t
end

module Action : sig
  (* This module impelements packet actions for Syntax. They are modeled as a set
     of maps from fields to values/continuations. *)

  type field_or_cont =
    | F of Field.t
    | K
  [@@deriving sexp]

  module Seq : sig
    (* List of modifications applied to fields, listed in field order.   There's one modification per
    field.  The continuation psuedo-field K is used for global compilation only, and is always the last field.
    So it looks like { F tcpSrc => 80; F ethSrc => ff:ff:ff:ff:ff:ff:ff; K => ??? }.  Note that this
    really isn't applied sequentially, but two Seq's applied sequentially will have last-modification-wins semantics,
    as it is in Syntax.  *)
    include Map.S with type Key.t = field_or_cont

    val compare : Value.t t -> Value.t t -> int
    val compare_mod_k : Value.t t -> Value.t t -> int

    (* [equal_mod_k s1 s2] Compares two sequences for equality, ignoring K pseudo-field if it exists *)
    val equal_mod_k : Value.t t -> Value.t t -> bool

    (** [to_hvs s] converts to a sequence to an HVS list, removing K pseudo-field *)
    val to_hvs : Value.t t -> (Field.t * Value.t) list

    val to_string : Value.t t -> string
  end

  module Par : sig
    (* Action sequences excuted in parallel, like an OpenFlow 1.3 action list.  Equal actions should be applied
     only once, which is why we use a Set here.  *)
    include Set.S with type Elt.t = Value.t Seq.t

   (** [to_hvs s] converts to a sequence to an HVS list applying sequences in the right order *)
    val to_hvs : t -> (Field.t * Value.t) list
    val mod_k : t -> t
    val compare_mod_k : t -> t -> int
    val equal_mod_k : t -> t -> bool

    val to_string : t -> string
  end


  type t = Par.t [@@deriving sexp]

  include Vlr.Result with type t := t

  (** [one] returns identify action, which is a modification representing "no modifications" *)
  val one : t

  (** [zero] returns empty action, which consistently with OpenFlow, means "drop" *)
  val zero : t

  (** [negate action] implements negation for the [zero] and [one] actions. Any
       non-[zero] action will be mapped to [zero] by this function. *)
  val negate : t -> t

  (** [to_policy a] converts an action to a NetKAT policy.  Since it's only modifications, it will have no predicates *)
  val to_policy : t -> Syntax.policy

  (** [demod pattern] removes any patterns from actions *)
  val demod : Pattern.t -> t -> t

  (** [to_sdn switch action] converts a NetKAT action to an SDN action. At the moment this function
   assumes that fields are assigned to proper bitwidth integers, and does
   no validation along those lines. If the input is derived from a NetKAT
   surface syntax program, then this assumption likely holds.    *)
  val to_sdn : ?group_tbl:Frenetic_kernel.GroupTable0x04.t
            -> Int64.t option -> t -> Frenetic_kernel.OpenFlow.par

  (** [get_queries action] returns a list of queries used in actions.  May have dupes. *)
  val get_queries : t -> string list

  (** [get_pipes action] returns a set of pipes used in actions *)
  val pipes : t -> String.Set.t

  (** [queries action] returns a de-duped set of queries referenced in action *)
  val queries : t -> string list

  val to_string : t -> string
end

module FDD : sig
  (** An FDD is an instance of a Variable-Lattice-Result (VLR) structure.  *)
  include module type of Vlr.Make(Field)(Value)(Action)
    with type r = Action.t
    and  type v = Field.t * Value.t

  val mk_cont : int64 -> t
  val conts : t -> Int64.Set.t
  val map_conts : t -> f:(int64 -> int64) -> t
end
