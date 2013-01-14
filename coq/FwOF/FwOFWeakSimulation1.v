Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.Bag.
Require Import FwOF.FwOF.
Require FwOF.FwOFRelationLemmas.


Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS) (MakeControllerLemmas : CONTROLLER_LEMMAS).

  Module Export RelationLemmas := 
    FwOF.FwOFRelationLemmas.Make (Atoms) (MakeControllerLemmas).

  Theorem weak_sim_1 :
    weak_simulation concreteStep abstractStep bisim_relation.
  Proof with auto with datatypes.
    unfold weak_simulation.
    intros.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct s. simpl in *.
    unfold concreteStep in H0.
    destruct s'. simpl in *.
    inversion H0; subst; simpl in *.
    (* Switch replaced with an equivalent switch. *)
    idtac "Proving weak_sim_1 (Case 1 of 12)...".
    unfold Equivalence.equiv in H1.
    destruct H1.
    simpl in *.
    unfold Equivalence.equiv in H1.
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    (* Another annoying lemma. Since SwitchesEquiv sws1 sws2 holds, each pair
       of equivalent switches relate to equivalent abstract states. We have to
       prove this by induction on sws1 and sws2. *)
    admit.
    apply multistep_nil.
    (* Switch is processing a packet from the input buffer *)
    idtac "Proving weak_sim_1 (Case 2 of 12) ...".
    autorewrite with bag in H using (simpl in H).
    match goal with
      | [ H : t === ({|(swId0,pt,pk)|}) <+> ?t1 |- _ ] => remember t1
    end.
    exists (Bag.unions (map (transfer swId0) (abst_func swId0 pt pk)) <+> b).
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> Heqb.
    assert (FlowTableSafe swId0 tbl0) as J.
      refine (concreteState_flowTableSafety1
        swId0 pts0 tbl0 inp0 (FromList outp' <+> outp0) ctrlm0
          (FromList (map (PacketIn pt) pksToCtrl) <+> switchm0) _)...
      simpl. left. apply reflexivity.
    unfold FlowTableSafe in J.
    pose (J0 := J pt pk outp' pksToCtrl H1).
    subst.
    (** Full autorewrite never worked (needed to explicitly call Bag.unions_app)
        But, this got worse after the bag-refactoring. No idea why. *)
    rewrite <- J0.
    autorewrite with bag using simpl.
    bag_perm 100. (* #winning *)
    apply multistep_tau with (a0 := ({|(swId0, pt, pk)|}) <+> b).
    apply AbstractStepEquiv...
    apply multistep_obs with
      (a0 := (Bag.unions (map (transfer swId0) (abst_func swId0 pt pk)) <+> b)).
    apply AbstractStep.
    subst.
    apply multistep_nil.
    (* Switch processes a flow-mod *)
    idtac "Proving weak_sim_1 (Case 3 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch sends/receives packets on the network. *)
    idtac "Proving weak_sim_1 (Case 4 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch sends a packet out. *)
    idtac "Proving weak_sim_1 (Case 5 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    destruct dst0.
    rename concreteState_consistentDataLinks0 into X.
    simpl in X.
    assert (In (DataLink (swId0,pt) pks0 (s,p)) 
               (links0 ++ (DataLink (swId0,pt) pks0 (s,p))  :: links1)) as J...
    apply X in J.
    simpl in J.
    autorewrite with bag using simpl.
    rewrite -> J.
    bag_perm 100.
    apply multistep_nil.
    (* Switch reads a packet off a data-link *)
    idtac "Proving weak_sim_1 (Case 6 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Controller takes an internal step. *)
    idtac "Proving weak_sim_1 (Case 7 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    rewrite -> (ControllerRemembersPackets H1).
    apply reflexivity.
    apply multistep_nil.
    (* Controller recvs an OpenFlow message. *)
    idtac "Proving weak_sim_1 (Case 8 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    autorewrite with bag using simpl.
    rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks0)).
    autorewrite with bag using simpl.
    rewrite -> (ControllerRecvRemembersPackets H1).
    bag_perm 100.
    apply multistep_nil.
    (* Controller sends an OpenFlow message. *)
    idtac "Proving weak_sim_1 (Case 9 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    autorewrite with bag using simpl.
    rewrite -> (ControllerSendForgetsPackets H1).
    bag_perm 100.
    apply multistep_nil.
    (* Switch sends an OpenFlow message. *)
    idtac "Proving weak_sim_1 (Case 10 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch responds to a barrier. *)
    idtac "Proving weak_sim_1 (Case 11 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    do 2 (rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks0))).
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch responds to a non-barrier message. *)
    idtac "Proving weak_sim_1 (Case 12 of 12) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    do 2 (rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks0))).
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
  Qed.

End Make.
