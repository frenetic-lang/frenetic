open Core.Std
open NetKAT_Types
open NetKAT_Semantics

open SDN_Types

module Field : sig

  type t = NetKAT_FDD.Field.t
    = Switch
    | Vlan
    | VlanPcp
    | EthType
    | IPProto
    | EthSrc
    | EthDst
    | IP4Src
    | IP4Dst
    | TCPSrcPort
    | TCPDstPort
    | Location

  val get_order : unit -> t list

  val to_string : t -> string

end

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]


type t
(** The type of the intermediate compiler representation. *)

type cache
  = [ `Keep
    | `Empty
    | `Preserve of t ]

(** {2 Compilation} *)

exception Non_local
(** The exception that's thrown when the compiler is given a policy with a
    [Link] term in it. [Link] terms are currently not supported by this
    compiler. *)

val compile : ?order:order -> ?cache:cache -> policy -> t
(** [compile p] returns the intermediate representation of the policy [p].
    You can generate a flowtable from [t] by passing it to the {!to_table}
    function below.

    The optional [order] flag determines the variable order. If unset,
    it uses a static default ordering.

    The optional [cache] flag determines if the cache should be cleared
    before compilation. By default, the cache is cleared.
 *)

val restrict : header_val -> t -> t
(** [restrict hv t] returns the fragment of [t] that applies when the assignment
    [hv] is true. The result will no longer make any reference to the header
    named in [hv]. This is equivalent to traversing the original NetKAT
    syntax tree and replacing all occurrences of [Test(hv)] with [True].

    This function is called by {!to_table} to restrict [t] to the portion that
    should run on a single switch. *)

val to_table : ?opt:bool -> switchId -> t -> flowTable
(** [to_table sw t] returns a flowtable that implements [t] for switch [sw]. *)


(** {2 Composition} *)

val seq : t -> t -> t
(** [seq p q] returns the sequential composotion of the two intermediate
    representations [p] and [q]. The result is semantically equivalent to the
    seqential composition of the two policies from which [p] and [q] were
    derived. *)

val union : t -> t -> t
(** [union p q] returns the parallel composition of the two intermediate
    representations [p] and [q]. The result is semantically equivalent to the
    parallel composition of the two policies from which [p] and [q] were
    derived. *)

val star : t -> t
(** [star p] returns the star of the intermediate representation [p]. The result
    is semantically equivalent to the star of the policy from which [p] was
    derived. *)


(** {2 Utilities} *)

val pipes : t -> string list
(** [pipes t] returns the list of pipe names that occur in [t]. *)

val queries : t -> (string * pred) list
(** [queries t] returns the list of queries that occur in [t] along with the
    predicates associated with the query. Packet and byte counts of flows that
    match the predicate should count towards its associated query. *)

val equal : t -> t -> bool
(** [equal a b] returns whether or not the two intermediate representations are
    structurally equal.

    If the two representations are structurally equal, then the policies they
    derived from are semantically equivalent. However, if the two
    representations are not equal, the policies they were derived from may still
    be semantically equivalent. *)

val size : t -> int
(** [size t] returns the size of [t]. *)

(* compressed_size / uncompressed_size *)
val compression_ratio : t -> int * int

val to_policy : t -> policy
(** [to_policy t] returns a NetKAT policy that is semantically equivalent to
    [t]. If was generated from compiling a policy [p], it is not guarateed that
    [to_policy t] will be identical to [p]. *)

val to_string : t -> string
(** [to_string t] returns a string representation of [t]. This will be a
    representation of the Vlr diagram from the tdk package. *)


(** {2 Interpreter} *)

val eval : packet -> t -> PacketSet.t
(** [eval pkt t] returns a [PacketSet.t] that is the result of the packet [pkt]
    being run through the policy represented by [t]. *)

val eval_pipes
  : packet -> t -> (string * packet) list * (string * packet) list * packet list
(** [eval_pipes pkt t] returns the result of running the packet [pkt] through
    the policy represented by [t], with packets grouped according to the type of
    location to which the policy assigns them. The result is a triple whose
    first component is a list of packets and corresponding pipe location, whose
    second is a list of packets and corresponding query location, and whose
    third is a list of packets that are at physical locations. *)

val to_dotfile : t -> string -> unit
