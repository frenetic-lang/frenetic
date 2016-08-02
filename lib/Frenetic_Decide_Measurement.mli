module Ast = Frenetic_Decide_Ast
module Util = Frenetic_Decide_Util

(** The abstract syntax of our path query language. *)
module Predicate : sig
  type t =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of t * t
    | And of t * t
    | Not of t

  val to_string: t -> string
end

module Query: sig
  type t =
    | Pred of Predicate.t * Predicate.t
    | Plus of t * t
    | Times of t * t
    | Star of t

  val to_string: t -> string
end

(** [term_of_policy p] converts a NetKAT policy [p] from the core frenetic
 * library into an Ast term: our representation of NetKAT policies. *)
val term_of_policy: Frenetic_NetKAT.policy -> Ast.Term.t

(** [terms_of_policy_ipdst p] converts a NetKAT policy [p] from the core frenetic
 * library into a list of Ast terms, each of which have a unique ipdst. *)
val terms_of_policy_ipdst: Frenetic_NetKAT.policy -> Ast.Term.t list

(** [term_of_topology t] converts a NetKAT topology [t] into an AST term. *)
val term_of_topology: Frenetic_Network.Net.Topology.t -> Ast.Term.t

(** [in_of_topology t] gathers the [in] term of a NetKAT topology [t]. *)
val in_of_topology: Frenetic_Network.Net.Topology.t -> Ast.Term.t

(** [out_of_topology t] gathers the [out] term of a NetKAT topology [t]. *)
val out_of_topology: Frenetic_Network.Net.Topology.t -> Ast.Term.t

(** A network is described by [in; (p; t)*; p; out]. *)
type network = {
  ingress:  Ast.Term.t; (* in  *)
  outgress: Ast.Term.t; (* out *)
  p:        Ast.Term.t; (* p   *)
  t:        Ast.Term.t; (* t   *)
}

(** [compile n q] compiles a network [n] and query [q] into a NetKAT term [t]
 *  of which we take the E-matrix *)
val compile: network -> Query.t -> Ast.Term.t
