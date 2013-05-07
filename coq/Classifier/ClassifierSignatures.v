Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Pattern2.PatternSignatures.
Require Import Network.NetworkPacket.

Import ListNotations.
Local Open Scope list_scope.

Module Type ACTION.
  Declare Module Pattern : PATTERN.
  Definition pattern := Pattern.t.
  Definition port := Pattern.port.

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
  Parameter apply_atom : e -> port * packet -> option (port * packet).

  Definition apply_action (action : t) (ptpk : port * packet) :=
    filter_map (fun a => apply_atom a ptpk) (atoms action).

  (** Parallel composition with [drop] as the identity. *)
  Parameter par_action : t -> t -> t.

  (** Sequential composition with [drop] as its annihilator. *)
  Parameter seq_action : t -> t -> t.

  (** [restrict_range atom pat] transforms [pat] to account for how [atom]
       modifies packets. If [pat] matches an input packet [(pt,pk)],
       then the transformed pattern matches the output [apply_atom
       atom (pt,pk)], and vice versa.

       Therefore, in a flow table, if [pat] is the pattern in a rule
       and [atom] is an action, [restrict_range] can be used to match the
       packets that [atom] produces. *)
  Parameter restrict_range : e -> pattern -> pattern.


  (** Some atomic actions, such as modifications, only apply to
      certain packets. Such conditional actions can only be realized
      in OpenFlow using flow tables. The [domain] function produces a
      classifier with the same semantics as the given action. However,
      each rule in the classifier is predicated so that they can
      safely apply unconditionally when the predicate holds. *)
  Parameter domain : e -> pattern.

End ACTION.

Module Type ACTION_SPEC.

  Declare Module PatternSpec : PATTERN_SPEC.  
  Declare Module Action : ACTION with Module Pattern := PatternSpec.Pattern.
  Import Action.

  Parameter atoms_drop : atoms drop = nil.

  (** If the applying the atom maps pk to pk0 and pk matches the mask, then
      pk0 matches the original pattern.
  
      The idea is that after being transformed to pk0, the packet matches the pattern.
      However, we can use the mask to instead match the pre-image of the action. *)
  Parameter restrict_range_spec : 
    forall (a : e) (pat : pattern) pt pk pt0 pk0,
      Some (pt0,pk0) = apply_atom a (pt,pk) ->
      Pattern.match_packet pt pk (restrict_range a pat) = 
      Pattern.match_packet pt0 pk0 pat.

  (** If pk is not in the domain of the pattern, then mask excludes it entirely. *)
  Parameter restrict_range_spec2 : 
    forall (a : e) (pat : pattern) pt pk,
      None = apply_atom a (pt,pk) ->
      Pattern.match_packet pt pk (restrict_range a pat) = false.

  Parameter restrict_domain_spec1 :
    forall e pt pk pk',
      Some pk' = apply_atom e (pt, pk) <->
      Pattern.match_packet pt pk (domain e) = true.

  Parameter restrict_domain_spec2 : forall e pt pk,
      None = apply_atom e (pt, pk) <->
      Pattern.match_packet pt pk (domain e) = false.


  Parameter seq_distr : 
    forall a0 a1 a2, 
      par_action (seq_action a0 a1) (seq_action a0 a2) = 
      seq_action a0 (par_action a1 a2).

  Parameter par_drop_l : forall a, par_action drop a = a.

  Parameter par_drop_r : forall a, par_action a drop = a.

  Parameter seq_drop_l : forall a, seq_action drop a = drop.

  Parameter seq_drop_r : forall a, seq_action a drop = drop.

End ACTION_SPEC.

Module Type CLASSIFIER.
 
  Declare Module Action : ACTION.
  Definition pattern := Action.pattern.
  Definition port := Action.port.
  Definition action := Action.t.

  Definition t := list (pattern * action).

  Parameter scan : t -> port -> packet -> action.

  Parameter inter : t -> t -> t.

  Parameter union : t -> t -> t.

  Parameter sequence : t -> t -> t.

  Fixpoint par_actions (lst : list action) :=
    match lst with
      | nil => Action.drop
      | act :: lst' => Action.par_action act (par_actions lst')
    end.

End CLASSIFIER.

Module Type CLASSIFIER_SPEC.

  Declare Module Classifier : CLASSIFIER.
  
  Import Classifier.

  Parameter union_spec :
    forall tbl1 tbl2 pt pk,
      scan (union tbl1 tbl2) pt pk =
      Action.par_action (scan tbl1 pt pk)
                        (scan tbl2 pt pk).

  Parameter sequence_spec :
      forall tbl1 tbl2 pt pk, 
        scan (sequence tbl1 tbl2) pt pk =
        Action.seq_action
          (scan tbl1 pt pk)
          (Classifier.par_actions 
             (map (fun (ptpk : port * packet) => let (pt,pk) := ptpk in scan tbl2 pt pk)
                  (Action.apply_action (scan tbl1 pt pk) (pt,pk)))).

End CLASSIFIER_SPEC.

Module Type MAKE (Action_ : ACTION) :=
  CLASSIFIER
    with Module Action := Action_.