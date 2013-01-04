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
Require FwOF.FwOFRelation.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module Relation := FwOF.FwOFRelation.Make (Atoms).
  Import Relation.
  Import Relation.Concrete.

  Theorem weak_sim_1 :
    weak_simulation concreteStep abstractStep bisim_relation.
  Proof with auto with datatypes.
    unfold weak_simulation.
    intros.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct s. simpl in *.
    split; intros.
    unfold concreteStep in H0.
    destruct s'. simpl in *.
    inversion H0; subst; simpl in *.
    (* Switch is processing a packet from the input buffer *)
    idtac "Proving weak_sim_1 (Case 1 of 11) ...".
    autorewrite with bag in H using (simpl in H).
    rewrite -> Bag.union_comm in H.
    rewrite -> Bag.union_assoc in H.
    match goal with
      | [ H : t === ?t0 <+> ?t1 |- _ ] => remember t1
    end.
    exists (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> b).
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> Heqb.
    assert (FlowTableSafe swId tbl) as J.
      refine (concreteState_flowTableSafety1
        (Switch swId pts tbl inp (Bag.FromList outp' <+> outp) ctrlm
          (Bag.FromList (map (PacketIn pt) pksToCtrl) <+> switchm)) _)...
      simpl. auto with datatypes.
    unfold FlowTableSafe in J.
    pose (J0 := J pt pk outp' pksToCtrl H3).
    subst.
    rewrite <- J0.
    autorewrite with bag using simpl.
    rewrite -> (Bag.unions_app _ (map relate_switch sws)).
    autorewrite with bag using simpl.
    bag_perm 100. (* #winning *)
    apply multistep_tau with (a0 := ({|(swId, pt, pk)|}) <+> b).
    apply AbstractStepEquiv...
    apply multistep_obs with
      (a0 := (Bag.unions (map (transfer swId) (abst_func swId pt pk)) <+> b)).
    apply AbstractStep.
    subst.
    apply multistep_nil.
    (* steps with no observations. *)
    unfold concreteStep in H0.
    destruct s'. simpl in *.
    inversion H0; subst; simpl in *.
    (* Switch processes a flow-mod *)
    idtac "Proving weak_sim_1 (Case 2 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch sends/receives packets on the network. *)
    idtac "Proving weak_sim_1 (Case 3 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch sends a packet out. *)
    idtac "Proving weak_sim_1 (Case 4 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    destruct dst.
    rename concreteState_consistentDataLinks0 into X.
    simpl in X.
    assert (In (DataLink (swId,pt) pks (s,p)) 
               (links ++ (DataLink (swId,pt) pks (s,p))  :: links0)) as J...
    apply X in J.
    simpl in J.
    autorewrite with bag using simpl.
    rewrite -> J.
    bag_perm 100.
    apply multistep_nil.
    (* Switch reads a packet off a data-link *)
    idtac "Proving weak_sim_1 (Case 5 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Controller takes an internal step. *)
    idtac "Proving weak_sim_1 (Case 6 of 11) ...".
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
    idtac "Proving weak_sim_1 (Case 7 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    simpl.
    autorewrite with bag using simpl.
    rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks)).
    autorewrite with bag using simpl.
    rewrite -> (ControllerRecvRemembersPackets H1).
    bag_perm 100.
    apply multistep_nil.
    (* Controller sends an OpenFlow message. *)
    idtac "Proving weak_sim_1 (Case 8 of 11) ...".
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
    idtac "Proving weak_sim_1 (Case 9 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch responds to a barrier. *)
    idtac "Proving weak_sim_1 (Case 10 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    do 2 (rewrite -> (Bag.unions_app _ (map relate_switch sws))).
    do 2 (rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks))).
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
    (* Switch responds to a non-barrier message. *)
    idtac "Proving weak_sim_1 (Case 11 of 11) ...".
    exists t.
    split.
    unfold bisim_relation.
    unfold relate.
    rewrite -> H.
    autorewrite with bag using simpl.
    do 2 (rewrite -> (Bag.unions_app _ (map relate_switch sws))).
    do 2 (rewrite -> (Bag.unions_app _ (map relate_openFlowLink ofLinks))).
    autorewrite with bag using simpl.
    bag_perm 100.
    apply multistep_nil.
  Qed.

End Make.