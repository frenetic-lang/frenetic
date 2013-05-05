Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
Require Import Network.NetworkPacket.
Require Import OpenFlow.OpenFlow0x01Types.

Import ListNotations.
Local Open Scope list_scope.

Module Type ACTION.

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

  (** An action that drops packets. *)
  Parameter drop : t.

  (** An action that leaves packets unmodified. Thus, only applying
      pass to a packet will send it back out of its current port. *)
  Parameter pass : t.

  (** Determines how an atomic action forwards packets. *)
  Parameter apply_atom : e -> portId * packet -> option (portId * packet).

  Definition apply_action (action : t) (ptpk : portId * packet) :=
    filter_map (fun a => apply_atom a ptpk) (atoms action).

  (** [mask_pat atom pat] transforms [pat] to account for how [atom]
       modifies packets. If [pat] matches an input packet [(pt,pk)],
       then the transformed pattern matches the output [apply_atom
       atom (pt,pk)], and vice versa.

       Therefore, in a flow table, if [pat] is the pattern in a rule
       and [atom] is an action, [mask_pat] can be used to match the
       packets that [atom] produces. *)
  Parameter mask_pat : e -> pattern -> pattern.

  (** Parallel composition with [drop] as the identify. *)
  Parameter par_action : t -> t -> t.

  (** Sequential composition with [drop] as its annihilator. *)
  Parameter seq_action : t -> t -> t.

  (** Some atomic actions, such as modifications, only apply to
      certain packets. Such conditional actions can only be realized
      in OpenFlow using flow tables. The compile function produces a
      classifier with the same semantics as the given action. However,
      each rule in the classifier is predicated so that they can
      safely apply unconditionally when the predicate holds. *)
  Parameter guard : e -> pattern.

End ACTION.

Module Type MAKE_ACTION_SPEC (Action_ : ACTION).
  
  Module Action := Action_.
  Import Action.

  Parameter atoms_drop : atoms drop = nil.

  (** If the applying the atom maps pk to pk0 and pk matches the mask, then
      pk0 matches the original pattern.
  
      The idea is that after being transformed to pk0, the packet matches the pattern.
      However, we can use the mask to instead match the pre-image of the action. *)
  Parameter mask_pat_spec : 
    forall (a : e) (pat : pattern) pt pk pt0 pk0,
      Some (pt0,pk0) = apply_atom a (pt,pk) ->
      Pattern.match_packet pt pk (mask_pat a pat) = 
      Pattern.match_packet pt0 pk0 pat.

  (** If pk is not in the domain of the pattern, then mask excludes it entirely. *)
  Parameter mask_pat_spec2 : 
    forall (a : e) (pat : pattern) pt pk,
      None = apply_atom a (pt,pk) ->
      Pattern.match_packet pt pk (mask_pat a pat) = false.

  Parameter seq_distr : 
    forall a0 a1 a2, 
      par_action (seq_action a0 a1) (seq_action a0 a2) = 
      seq_action a0 (par_action a1 a2).

  Parameter par_drop_l : forall a, par_action drop a = a.

  Parameter par_drop_r : forall a, par_action a drop = a.

  Parameter seq_drop_l : forall a, seq_action drop a = drop.

  Parameter seq_drop_r : forall a, seq_action a drop = drop.

End MAKE_ACTION_SPEC.

Module Type CLASSIFIER.

  Declare Module Action : ACTION.

  Definition action := Action.t.

  Definition t := list (pattern * action).

  Parameter scan : t -> portId -> packet -> action.

  Parameter inter : t -> t -> t.

  Parameter union : t -> t -> t.

  Parameter sequence : t -> t -> t.

End CLASSIFIER.

Module Type MAKE_CLASSIFIER (Import Action_ : ACTION).

  Module Action := Action_.
  Import Action.

  Definition action := Action.t.

  Definition t := list (pattern * action).

  Parameter scan : t -> portId -> packet -> action.

  Parameter inter : t -> t -> t.

  Parameter union : t -> t -> t.

  Parameter sequence : t -> t -> t.

End MAKE_CLASSIFIER.
  

Module Type CLASSIFIER_SPEC.

  Declare Module Classifier : CLASSIFIER.
  
  Import Classifier.

  Parameter union_spec :
    forall tbl1 tbl2 pt pk,
      scan (union tbl1 tbl2) pt pk =
      Action.par_action (scan tbl1 pt pk)
                        (scan tbl2 pt pk).

End CLASSIFIER_SPEC.  
