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
Require Import Common.AllDiff.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Atoms : ATOMS).

  Module Relation := FwOF.FwOFRelation.Make (Atoms).
  Module Concrete := Relation.Concrete.

  Import Relation.
  Import Relation.Concrete.

  Lemma LinksHaveSrc_untouched : forall 
    {swId tbl pts sws sws0 links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveSrc 
      (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)  links ->
    LinksHaveSrc 
      (sws ++ (Switch swId pts tbl' inp' outp' ctrlm' switchm') :: sws0)
      links.
  Admitted.

  Lemma LinksHaveDst_untouched : forall 
    {swId tbl pts sws sws0 links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveDst
      (sws ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws0)  links ->
    LinksHaveDst 
      (sws ++ (Switch swId pts tbl' inp' outp' ctrlm' switchm') :: sws0)
      links.
  Admitted.

   Lemma LinkTopoOK_inv : forall {links links0 src dst} pks pks',
     ConsistentDataLinks (links ++ (DataLink src pks dst) :: links0) ->
     ConsistentDataLinks (links ++ (DataLink src pks' dst) :: links0).
   Proof with auto with datatypes.
     intros.
     unfold ConsistentDataLinks in *.
     intros.
     apply in_app_iff in H0.
     simpl in H0.
     destruct H0 as [H0 | [H0 | H0]]...
     pose (lnk' := (DataLink src0 pks0 dst0)).
     remember (H lnk').
     assert (In lnk' (links0 ++ lnk' :: links1))...
     apply e in H1.
     simpl in H1.
     inversion H0.
     simpl...
   Qed.

  Lemma FlowTablesSafe_equiv: forall 
    { sw sw' sws sws0 },
    sw === sw' ->
    FlowTablesSafe (sws ++ sw :: sws0) ->
    FlowTablesSafe (sws ++ sw' :: sws0).
  Proof with eauto with datatypes.
    intros.
    unfold FlowTablesSafe in *.
    intros.
    apply in_app_iff in H1.
    simpl in H1.
    destruct H1 as [HIn | [HIn | HIn]].
    eapply H0...
    (* Detailed case. *)
    unfold Equivalence.equiv in H.
    destruct H.
    inversion HIn.
    subst.
    clear HIn.
    remember (H0 swId0 pts0 tbl0 inp1 outp1 ctrlm1 switchm1) as X.
    clear HeqX H0.
    apply X...
    eapply H0...
  Qed.

  Lemma FlowTablesSafe_untouched : forall {sws sws0 swId pts tbl inp inp'
    outp outp' ctrlm ctrlm' switchm switchm' },
    FlowTablesSafe (sws++(Switch swId pts tbl inp outp ctrlm switchm)::sws0) ->
    FlowTablesSafe 
      (sws++(Switch swId pts tbl inp' outp' ctrlm' switchm')::sws0).
  Proof with eauto.
    intros.
  Admitted.

  Lemma LinkHasSrc_equiv : forall {sws sws' link},
    sws === sws' ->
    LinkHasSrc sws link ->
    LinkHasSrc sws' link.
  Proof with auto with datatypes.
    intros.
    destruct link.
    inversion H0.
    destruct src.
    simpl in H1.
    destruct H1 as [HInSw [HSwEq HInPt]].
    destruct x; simpl in *; subst.
    unfold Equivalence.equiv in H.
    unfold LinkHasSrc in *.
    destruct H0 as [switch0 [HIn [HSwEq HInPt0]]].
    simpl in *.
    generalize dependent sws'.
    induction sws.
    (* Contradiction *)
    intros. destruct sws'. inversion HInSw. simpl in H. contradiction.
    (* Outer inductive case. *)
    intros. destruct sws'. simpl in H. contradiction.
    (* Outer + inner induction *)
    simpl in H.
    destruct H as [H Hrec].
    simpl in HInSw.
    destruct HInSw as [HInSw | HInSw];
    destruct HIn as [HIn | HIn].
    (* Case 1 *)
    subst.
    rewrite -> HInSw in *.
    simpl in *.
    exists s.
    split...
    destruct s.
    unfold Equivalence.equiv in H.
    inversion H.
    subst.
    simpl.
    split...
    (* Case 2 *)
    clear IHsws.
    admit. (* requires unique IDs *)
    (* Case 3 *)
    admit. (* requires unique IDs *)
    (* Case 4 *)
    remember (IHsws HIn HInSw sws' Hrec) as J eqn:X.
    clear IHsws X.
    destruct J as [theSwitch0 [HIn0 [HSwEq0 HPtsIn0]]].
    exists theSwitch0...
  Qed.

  Lemma LinksHaveSrc_inv : forall {sws links links0 src dst} pks pks',
    LinksHaveSrc sws (links ++ (DataLink src pks dst) :: links0) ->
    LinksHaveSrc sws (links ++ (DataLink src pks' dst) :: links0).
  Proof with auto with datatypes.
  Admitted.

  Lemma LinksHaveDst_inv : forall {sws links links0 src dst} pks pks',
    LinksHaveDst sws (links ++ (DataLink src pks dst) :: links0) ->
    LinksHaveDst sws (links ++ (DataLink src pks' dst) :: links0).
  Admitted.

  Lemma UniqSwIds_map_swId_eq : forall {sws0 sws1 swId0 pts tbl inp outp ctrlm switchm
    pts' tbl' inp' outp' ctrlm' switchm'},
    map swId (sws0 ++ (Switch swId0 pts tbl inp outp ctrlm switchm) :: sws1) =
    map swId (sws0 ++ (Switch swId0 pts' tbl' inp' outp' ctrlm' switchm') :: sws1).
  Proof with auto with datatypes.
    intros.
    repeat rewrite -> map_app.
    simpl.
    trivial.
  Qed.


  Lemma UniqSwIds_pres : forall {sws0 sws1 swId pts tbl inp outp ctrlm switchm
    pts' tbl' inp' outp' ctrlm' switchm'},
    UniqSwIds (sws0 ++ (Switch swId pts tbl inp outp ctrlm switchm) :: sws1) ->
    UniqSwIds (sws0 ++ (Switch swId pts' tbl' inp' outp' ctrlm' switchm') :: sws1).
  Proof with auto with datatypes.
    intros.
    unfold UniqSwIds in *.
    eapply AllDiff_preservation.
    exact H.
    apply UniqSwIds_map_swId_eq.
  Qed.

  Lemma SwId_eq_Switch : forall (sw1 sw2 : switch) (sws : list switch),
    UniqSwIds sws ->
    In sw1 sws ->
    In sw2 sws ->
    swId sw1 = swId sw2 ->
    sw1 = sw2.
  Proof with eauto.
    intros.
    unfold UniqSwIds in H.
    eapply AllDiff_uniq...
  Qed.

  Hint Unfold UniqSwIds.

  Lemma simpl_step : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1)),
    step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2,
      concreteStep
        (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1)
        obs
        (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2).
  Proof with eauto with datatypes.
    intros.
    unfold concreteStep.
    simpl.
    inversion H; subst.
    (* Case 1. *)
    (* idiotic case. *)
    admit.
    (* Case 2. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    trivial.
    (* Case 3. *)
    idtac "TODO(arjun): critical case skipping -- flowmod".
    admit.
    (* Case 4. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1)...
    (* Case 5. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists (LinkTopoOK_inv pks0 (pk::pks0) linksTopoOk1).
    exists (LinksHaveSrc_inv pks0 (pk::pks0) (LinksHaveSrc_untouched haveSrc1)).
    exists (LinksHaveDst_inv pks0 (pk::pks0) (LinksHaveDst_untouched haveDst1)).
    exists (UniqSwIds_pres uniqSwIds1).
    trivial.
    (* Case 6. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists (LinkTopoOK_inv (pks0 ++ [pk]) pks0 linksTopoOk1).
    exists 
      (LinksHaveSrc_inv (pks0 ++ [pk]) pks0 (LinksHaveSrc_untouched haveSrc1)).
    exists 
      (LinksHaveDst_inv (pks0 ++ [pk]) pks0 (LinksHaveDst_untouched haveDst1)).
    exists (UniqSwIds_pres uniqSwIds1).
    trivial.
    (* Case 7. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    trivial.
    (* Case 8. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    trivial.
    (* Case 9. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    trivial.
    (* Case 10. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    trivial.
    (* Case 11. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    trivial.
    (* Case 12. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    trivial.
  Qed.

  Lemma simpl_multistep : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1)),
    multistep step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2,
      multistep concreteStep
                (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1)
                obs
                (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2).
  Proof with eauto with datatypes.
    intros.
    induction H.
    (* zero steps. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    auto.
    (* tau step *)
    destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1 H)
             as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 step]]]]].
    destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2)
             as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3 [uniqSwIds3 stepN]]]]].
    exists tblsOk3. 
    exists linksTopoOk3.
    exists haveSrc3.
    exists haveDst3.
    exists uniqSwIds3.
    eapply multistep_tau...
    destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1 H)
             as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 step]]]]].
    destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2)
             as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3 [uniqSwIds3 stepN]]]]].
    exists tblsOk3. 
    exists linksTopoOk3.
    exists haveSrc3.
    exists haveDst3.
    exists uniqSwIds3...
  Qed.

  Lemma relate_step_simpl_tau : forall st1 st2,
    concreteStep st1 None st2 ->
    relate (devices st1) === relate (devices st2).
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
    (* Case 1. *)
    unfold Equivalence.equiv in H0.
    destruct H0.
    unfold relate.
    simpl.
    unfold Equivalence.equiv in H0.
    admit. (* TODO(arjun): dumb lemma needed. *)
    (* Case 2. *)
    destruct st1.
    destruct st2.
    subst.
    unfold relate.
    simpl.
    idtac "TODO(arjun): skipping critical flowmod case.".
    admit.
    (* PacketOut case. *)
    destruct st1. destruct st2. subst. unfold relate. simpl.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* SendDataLink case. *)
    destruct st1. destruct st2. subst. unfold relate. simpl.
    autorewrite with bag using simpl.
    destruct dst0.
    rewrite -> Bag.from_list_cons.
    (* relies on linkTopo consistency *)
    admit.
    (* RecvDataLink case. *)
    (* relies on linkTopo consistency *)
    admit.
    (* Controller steps *)
    unfold relate.
    simpl.
    rewrite -> (ControllerRemembersPackets H2).
    apply reflexivity.
    (* Controller receives. *)
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    rewrite -> (ControllerRecvRemembersPackets H2).
    bag_perm 100.
    (* Controller sends *)
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    rewrite -> (ControllerSendForgetsPackets H2).
    bag_perm 100.
    (* Switch sends to controller *)
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* Switch receives a barrier. *)
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* Switch receives a non-barrier. *)
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
  Qed.

  Lemma relate_multistep_simpl_tau : forall st1 st2,
    multistep concreteStep st1 nil st2 ->
    relate (devices st1) === relate (devices st2).
  Proof with eauto.
    intros.
    remember nil.
    induction H...
    apply reflexivity.
    apply relate_step_simpl_tau in H. 
    eapply transitivity...
    inversion Heql.
  Qed.

  Lemma relate_step_simpl_obs : forall  sw pt pk lps st1 st2,
    relate (devices st1) === ({| (sw,pt,pk) |} <+> lps) ->
    concreteStep st1 (Some (sw,pt,pk)) st2 ->
    relate (devices st2) === 
      (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).
  Proof with eauto with datatypes.
    intros.
    inversion H0.
    destruct st1.
    destruct st2.
    destruct devices0.
    destruct devices1.
    subst.
    simpl in *.
    inversion H1. subst. clear H1.
    inversion H6. subst. clear H6.

    assert (FlowTableSafe sw tbl0) as Z.
      unfold FlowTablesSafe in concreteState_flowTableSafety0.
      eapply concreteState_flowTableSafety0...
    unfold FlowTableSafe in Z.
    remember (Z pt pk outp' pksToCtrl H3) as Y eqn:X. clear X Z.
    rewrite <- Y. clear Y.

    unfold relate in *.
    simpl in *.
    rewrite -> map_app.
    simpl.
    rewrite -> Bag.bag_unions_app.
    repeat rewrite -> Bag.union_assoc.
    simpl.
    repeat rewrite -> Bag.union_assoc.
    repeat rewrite -> map_app.
    repeat rewrite -> Bag.bag_unions_app.
    repeat rewrite -> Bag.union_assoc.
    apply Bag.unpop_unions with (b := ({|(sw,pt,pk)|})).
    apply symmetry.
    rewrite -> Bag.union_comm.
    repeat rewrite -> Bag.union_assoc.
    rewrite -> (Bag.union_comm _ lps).
    rewrite <- H.
    rewrite -> map_app.
    simpl.
    rewrite -> Bag.bag_unions_app.
    simpl.
    rewrite -> Bag.from_list_cons.
    repeat rewrite -> Bag.union_assoc.
    bag_perm 100.
  Qed.


  Lemma relate_multistep_simpl_obs : forall  sw pt pk lps st1 st2,
    relate (devices st1) === ({| (sw,pt,pk) |} <+> lps) ->
    multistep concreteStep st1 [(sw,pt,pk)] st2 ->
    relate (devices st2) === 
      (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).
  Proof with eauto.
    intros.
    remember [(sw,pt,pk)] as obs.
    induction H0; subst.
    inversion Heqobs.
    apply IHmultistep...
    apply relate_step_simpl_tau in H0.
    apply symmetry in H0.
    eapply transitivity...
    destruct obs; inversion Heqobs.
    subst.
    clear Heqobs.
    apply relate_multistep_simpl_tau in H1.
    apply relate_step_simpl_obs with (lps := lps) in H0 .
    rewrite <- H0.
    apply symmetry.
    trivial.
    trivial.
  Qed.


  Lemma simpl_weak_sim : forall devs1 devs2 sw pt pk lps
    (tblsOk1 : FlowTablesSafe (switches devs1))
    (linksTopoOk1 : ConsistentDataLinks (links devs1))
    (haveSrc1 : LinksHaveSrc (switches devs1) (links devs1))
    (haveDst1 : LinksHaveDst (switches devs1) (links devs1))
    (uniqSwIds1 : UniqSwIds (switches devs1)),
    multistep step devs1 [(sw,pt,pk)] devs2 ->
    relate devs1 === ({| (sw,pt,pk) |} <+> lps) ->
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps) ->
   exists t : concreteState,
     inverse_relation 
       bisim_relation
       (Bag.unions (map (transfer sw) (abst_func sw pt pk)) <+> lps)
       t /\
     multistep concreteStep
               (ConcreteState devs1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1)
               [(sw,pt,pk)]
               t.
  Proof with eauto.
    intros.
    Check simpl_multistep.
    destruct (simpl_multistep tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1 H)
             as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 Hmultistep]]]]].
    match goal with
      | [ _ : multistep _ ?s1 _ ?s2 |- _ ] =>
        remember s1 as st1; remember s2 as st2
    end.
    assert (relate (devices st1) === ({| (sw,pt,pk) |} <+> lps)) as Hrel.
      subst. simpl...
    exists st2.
    split.
    unfold inverse_relation.
    unfold bisim_relation.
    apply symmetry.
    exact (relate_multistep_simpl_obs Hrel Hmultistep).
    trivial.
  Qed.

End Make.