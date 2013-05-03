Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
Require Import Network.NetworkPacket.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import Classifier.Classifier.

Import ListNotations.
Local Open Scope list_scope.

Module Type ACTION_DEF.

  (** An action, which has type [t], when applied to an input packet
     produces (possibly several) output packets. In constrast, an
     atomic action, which has type [e], produces just one packet. Each
     action consists of a fixed list of atomic actions, which
     determine the action's semantics.

     In the simplest case, an action can be thought of as a list of
     atomic actions.  However, actions may have other elements that do
     not affect forwarding. (e.g., queries). *)
  Parameter t e : Type.

  Parameter atoms : t -> list e.

  (** An action that drops packets. i.e., [atoms zero = nil]. *)
  Parameter zero : t.

  Parameter one : t.

  (** Determines how an atomic action forwards packets. *)
  Parameter apply_atom : e -> portId * packet -> portId * packet.

  (** [mask_pat atom pat] transforms [pat] to account for how [atom]
       modifies packets. If [pat] matches an input packet [(pt,pk)],
       then the transformed pattern matches the output [apply_atom
       atom (pt,pk)], and vice versa.

       Therefore, in a flow table, if [pat] is the pattern in a rule
       and [atom] is an action, [mask_pat] can be used to match the
       packets that [atom] produces. *)
  Parameter mask_pat : e -> pattern -> pattern.

  (** Parallel composition with [zero] as the identify. *)
  Parameter par_action : t -> t -> t.

  (** Sequential composition with [zero] as its annihilator. *)
  Parameter seq_action : t -> t -> t.

  (** Some atomic actions, such as modifications, only apply to
      certain packets. Such conditional actions can only be realized
      in OpenFlow using flow tables. The compile function produces a
      classifier with the same semantics as the given action. However,
      each rule in the classifier is predicated so that they can
      safely apply unconditionally when the predicate holds. *)
  Parameter compile : t -> Classifier t.

End ACTION_DEF.

Module Type ACTION_SPEC (Import Action : ACTION_DEF).

  Parameter atoms_zero : atoms zero = nil.

  Parameter mask_pat_spec : 
    forall (a : e) (pat : pattern) pt pk pt0 pk0,
      (pt0,pk0) = apply_atom a (pt,pk) ->
      Pattern.match_packet pt pk (mask_pat a pat) = 
      Pattern.match_packet pt0 pk0 pat.

  Parameter seq_distr : 
    forall a0 a1 a2, 
      par_action (seq_action a0 a1) (seq_action a0 a2) = 
      seq_action a0 (par_action a1 a2).

  Parameter par_zero_l : forall a, par_action zero a = a.

  Parameter par_zero_r : forall a, par_action a zero = a.

  Parameter seq_zero_l : forall a, seq_action zero a = zero.

  Parameter seq_zero_r : forall a, seq_action a zero = zero.


End ACTION_SPEC.
