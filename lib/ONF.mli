(** Compiler for NetKAT *)

(** {2 OpenFlow Normal Form}

  This is OpenFlow Normal Form, as defined in the NetKAT submission, with a
  few differences: (1) there is no star-compilation, (2) the grammar is for
  a particular switch, not all switches. Star compilation is forthcoming, but
  a whole-network ONF depends on knowning all switches in advance, which is
  not how we build our system.

 *)

(* A set of maps from header names to header values *)
module HdrValSet : Set.S
  with type elt = NetKAT_Types.hdrValMap

(** A conjunction of tests, where each test tests a distinct field and all
    fields are tested. Formally:

  Let [k] be the number of headers (12 for OpenFlow 1.0).

  [pred ::= h_1 = v_1 ; .. ; h_k = v_k]

  where all [k] headers are tested. *)
type pred = NetKAT_Types.hdrValMap

(** A sequence of updates, where each update affects a distinct field.
    Therefore, they all commute with each other and can be represented by
    a set. Formally:

  [seq  ::= h_1 -> v_1 ; ... ; h_1 -> v_n]

  where all headers [h] are distinct. *)
type seq = NetKAT_Types.hdrValMap

(** A sum of update sequences, where each sequence is distinct. Formally:

	[sum ::= seq_1; ...; seq_n]

  where all subterms [seq] are distinct. *)
type sum = HdrValSet.t


(** A cascase of [if .. then .. else] expressions nested under the [else]
    branch.

  [local ::= sum | if pred then sum else sum]

 *)
type local =
  | Action of sum
  | ITE of pred * sum * local

(** {2 NetKAT Compiler}

  Applying [compile pol] will fail if [pol] tries to update [Switch]. However,
  testing the [Switch] is fine. We produce a policy where some clauses are
  guarded by switch tests. These clauses are only emitted to those particular
  switches. {i I think the NetKAT submission's compiler section could
  be refactored to follow this pattern too.}

 *)

val compile : NetKAT_Types.pol -> local

(** {2 Utilities} *)

(** Converts the ONF term to an isomorphic NetKAT term, but simplifies
[x + Id] to [x] when possible. *)
val to_netkat : local -> NetKAT_Types.pol