Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Arguments to_list _ _ _ : simpl never.

Module Make (Import RelationDefinitions : RELATION_DEFINITIONS).

  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Theorem weak_sim_1 :
    weak_simulation concreteStep abstractStep bisim_relation.
  Proof with auto with datatypes.
    unfold weak_simulation.
    intros.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct s. simpl in *.
    unfold concreteStep in H0.
    destruct s'.
    { inversion H0; subst; simpl in *; subst; simpl in *.
      + idtac "Proving weak_sim_1 (Case 2 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context[({|(swId0,pt,pk)|}) <+> ?t1] ] => remember t1
        end.
        exists (unions (map (transfer swId0) (abst_func swId0 pt pk)) <+> b).
        split.
        unfold bisim_relation.
        unfold relate.
        rewrite -> Heqb.
        assert (FlowTableSafe swId0 tbl0) as J.
        { assert (FlowModSafe swId0 tbl0 ctrlm0) as J.
          refine (concreteState_flowTableSafety1
                    swId0 pts0 tbl0 inp0 (from_list outp' <+> outp0) ctrlm0
                    (from_list (map (PacketIn pt) pksToCtrl) <+> switchm0) _)...
          rewrite -> Bag.in_union; simpl...
          inversion J... }
        unfold FlowTableSafe in J.
        pose (J0 := J pt pk outp' pksToCtrl H1).
        subst.
        rewrite <- J0.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_obs with
        (a0 := (unions (map (transfer swId0) (abst_func swId0 pt pk)) <+> b)).
        apply AbstractStep.
        subst.
        apply multistep_nil.
      (* Switch processes a flow-mod *)
      + idtac "Proving weak_sim_1 (Case 3 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
        (* Switch sends/receives packets on the network. *)
      + idtac "Proving weak_sim_1 (Case 4 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
      (* Switch sends a packet out. *)
      + idtac "Proving weak_sim_1 (Case 5 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        destruct dst0.
        rename concreteState_consistentDataLinks0 into X.
        simpl in X.
        assert (In (DataLink (swId0,pt) pks0 (s,p)) 
                   (links0 ++ (DataLink (swId0,pt) pks0 (s,p))  :: links1)) as J...
        apply X in J.
        simpl in J.
        rewrite -> J.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
      (* Switch reads a packet off a data-link *)
      + idtac "Proving weak_sim_1 (Case 6 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
      (* Controller takes an internal step. *)
      + idtac "Proving weak_sim_1 (Case 7 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        rewrite -> (ControllerRemembersPackets H1)...
        apply multistep_nil.
      (* Controller recvs an OpenFlow message. *)
      + idtac "Proving weak_sim_1 (Case 8 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        rewrite -> (ControllerRecvRemembersPackets H1).
        bag_perm 100.
        apply multistep_nil.
      (* Controller sends an OpenFlow message. *)
      + idtac "Proving weak_sim_1 (Case 9 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        rewrite -> (ControllerSendForgetsPackets H1).
        bag_perm 100.
        apply multistep_nil.
      (* Switch sends an OpenFlow message. *)
      + idtac "Proving weak_sim_1 (Case 10 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
      (* Switch responds to a barrier. *)
      + idtac "Proving weak_sim_1 (Case 11 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
      (* Switch responds to a non-barrier message. *)
      + idtac "Proving weak_sim_1 (Case 12 of 12) ...".
        autorewrite with bag using simpl.
        match goal with
          | [ |- context [multistep _ ?X _ _] ] => remember X as t
        end.
        exists t.
        split.
        unfold bisim_relation.
        unfold relate.
        subst.
        simpl.
        autorewrite with bag using simpl.
        bag_perm 100.
        apply multistep_nil.
    }
  Qed.

End Make.
