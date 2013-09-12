(** Compiler for NetKAT *)

(** {2 OpenFlow Normal Form}

    This is OpenFlow Normal Form, as defined in the NetKAT submission, with a
    few differences: (1) there is no star-compilation, (2) the grammar is for
    a particular switch, not all switches. Star compilation is forthcoming, but
    a whole-network ONF depends on knowning all switches in advance, which is
    not how we build our system.

*)

(* A set of maps from header names to header values *)
module ActSet : Set.S
  with type elt = NetKAT_Types.header_val_map

(** A conjunction of tests, where each test tests a distinct field and all
    fields are tested. Formally:

    Let [k] be the number of headers (12 for OpenFlow 1.0).

    [pred ::= h_1 = v_1 ; .. ; h_k = v_k]

    where all [k] headers are tested. *)
type pat = NetKAT_Types.header_val_map

(** A sequence of updates, where each update affects a distinct field.
    Therefore, they all commute with each other and can be represented by
    a set. Formally:

    [act  ::= h_1 -> v_1 ; ... ; h_1 -> v_n]

    where all headers [h] are distinct. *)
type act = NetKAT_Types.header_val_map

(** A sum of update sequences, where each sequence is distinct. Formally:

    	[acts ::= act_1; ...; act_n]

    where all subterms [seq] are distinct. *)
type acts = ActSet.t

(** A cascase of [if .. then .. else] expressions nested under the [else]
    branch.

    [local ::= (pat,acts) list]

*)
type local = (pat * acts) list

(** {2 NetKAT Compiler}

    Applying [compile pol] will fail if [pol] tries to update [Switch]. However,
    testing the [Switch] is fine. We produce a policy where some clauses are
    guarded by switch tests. These clauses are only emitted to those particular
    switches. {i I think the NetKAT submission's compiler section could
    be refactored to follow this pattern too.}

*)

val compile : NetKAT_Types.policy -> local

(** {2 Utilities} *)

(** Converts the ONF term to an isomorphic NetKAT term, but simplifies
    [x + Id] to [x] when possible. *)
val to_netkat : local -> NetKAT_Types.policy
