open NetKAT_Types

val compile : pred -> pred -> policy -> policy
(** [compile ing eg p] returns a loal policy (containing no links) given a global policy [p]
    (that may contain links) together with predicates [ing] and [eg] describing the ingress and
    egress of the network.
    The compiler assumes that on any packet, either [ing] and [eg] or topo is equivalent to drop,
    where topo denotes the union over all links contained in [p].
    The compiler furthermore assumes that [p] does not contain tests or modifications of the vlan
    field. Violating this assumption will result in undefined behavior.
    The compiler uses the vlan field to maintain internal state across links, i.e. the
    policy retuned by the compiler may change the vlan field of a packet in arbitrary ways.  *)

(** The policy must be of the form [Seq (Filter ingress, Seq (pol, Filter egress))] *)
val compile_pol : pol:policy -> policy