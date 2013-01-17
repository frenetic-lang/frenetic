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

Module Make (AtomsAndController : ATOMS_AND_CONTROLLER).

  Import AtomsAndController.
  Import Atoms.
  Import FwOF.
  Module Import Relation := FwOF.FwOFRelation.Make (AtomsAndController).

  Lemma LinksHaveSrc_untouched : forall 
    {swId tbl pts sws links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveSrc 
      ({| Switch swId pts tbl inp outp ctrlm switchm |} <+> sws)  links ->
    LinksHaveSrc 
      ({| Switch swId pts tbl' inp' outp' ctrlm' switchm' |} <+> sws)
      links.
  Proof with auto.
  Admitted.

  Lemma LinksHaveDst_untouched : forall 
    {swId tbl pts sws links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveDst
      ({| Switch swId pts tbl inp outp ctrlm switchm |} <+>  sws)  links ->
    LinksHaveDst 
      ({| Switch swId pts tbl' inp' outp' ctrlm' switchm' |} <+> sws)
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
    { sw sw' sws },
    sw === sw' ->
    FlowTablesSafe ({| sw |} <+> sws) ->
    FlowTablesSafe ({| sw' |} <+> sws).
  Proof with eauto with datatypes.
    intros.
    unfold FlowTablesSafe in *.
    intros.
    intros.
    apply Bag.mem_union in H1.
    simpl in H1.
    destruct H1 as [HIn | HIn].
    apply H0 with (pts := pts0) (inp := inp0) (outp := outp0) (ctrlm := ctrlm0)
      (switchm := switchm0).
    apply Bag.mem_union.
    left.
    simpl.
    eapply transitivity. apply HIn. apply symmetry. exact H.
    (* Detailed case. *)
    remember (H0 swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) as X.
    clear HeqX H0.
    apply X.
    apply Bag.mem_union.
    right...
  Qed.

  Lemma FlowTablesSafe_untouched : forall {sws swId pts tbl inp inp'
    outp outp' ctrlm ctrlm' switchm switchm' },
    FlowTablesSafe
      ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws) ->
    FlowTablesSafe 
      ({|Switch swId pts tbl inp' outp' ctrlm' switchm'|} <+> sws).
  Proof with eauto.
    intros.
  Admitted.

  Lemma LinkHasSrc_equiv : forall {sws sws' link},
    sws === sws' ->
    LinkHasSrc sws link ->
    LinkHasSrc sws' link.
  Proof with eauto.
    intros.
    unfold LinkHasSrc in H0.
    destruct H0 as [sw0 [HMem [HSwEq HInPts]]].
    apply Bag.mem_equiv with (x := sw0) in H.
    destruct H as [sw1 [HMem1 HEquiv]].
    unfold LinkHasSrc.
    destruct HEquiv...
    trivial.
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

  Lemma UniqSwIds_pres : forall {sws swId pts tbl inp outp ctrlm switchm
    pts' tbl' inp' outp' ctrlm' switchm'},
    UniqSwIds ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws) ->
    UniqSwIds ({|Switch swId pts' tbl' inp' outp' ctrlm' switchm'|} <+> sws).
  Proof with auto with datatypes.
    intros.
    unfold UniqSwIds in *.
    simpl in *...
  Qed.

  Section FMS.

    Hint Constructors FMS SafeWire SwitchEP.

    Lemma SwitchEP_equiv : forall swId pts tbl inp0 inp1 outp0 outp1
      ctrlm0 ctrlm1 switchm0 switchm1 ep,
      ctrlm0 === ctrlm1 ->
      SwitchEP (Switch swId pts tbl inp0 outp0 ctrlm0 switchm0) ep ->
      SwitchEP (Switch swId pts tbl inp1 outp1 ctrlm1 switchm1) ep.
    Proof with eauto.
      intros.
      inversion H0; subst.
      apply NoFlowModsInBuffer.
      intros. apply H9. eapply Bag.Mem_equiv... apply symmetry...
      eapply OneFlowModInBuffer...
      eapply transitivity... apply symmetry...
    Qed.

    Lemma FMS_untouched : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
      switchmLst0 ctrlmLst0 inp1 outp1 switchm1 switchmLst1,
      FMS (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)
          (OpenFlowLink swId0 switchmLst0 ctrlmLst0) ->
      FMS (Switch swId0 pts0 tbl0 inp1 outp1 ctrlm0 switchm1)
          (OpenFlowLink swId0 switchmLst1 ctrlmLst0).
    Proof.
      intros.
      inversion H; inversion H2; subst; eauto.
    Qed.

    Hint Resolve FMS_untouched SwitchEP_equiv.

    Lemma AllFMS_untouched1 : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
      inp1 outp1 switchm1 sws ofLinks0,
      AllFMS ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws)
             ofLinks0 ->
      AllFMS ({|Switch swId0 pts0 tbl0 inp1 outp1 ctrlm0 switchm1|} <+> sws)
             ofLinks0.
    Proof with simpl;eauto.
      intros.
      unfold AllFMS.
      intros.
      simpl in H0.
      destruct H0 as [H0 | H0].
      destruct sw.
      inversion H0.
      subst. clear H0.
      assert (Mem (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)
                  ({|(Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)|} <+>
                   sws)) as J.
        simpl. left. apply reflexivity.
      apply H in J.
      simpl in J.
      destruct J as [lnk [HIn [HEq FMSsw0]]].
      simpl.
      exists lnk.
      split...
      split...
      remember FMSsw0 as FMSorig eqn:X; clear X.
      inversion FMSsw0. 
      clear HEq.
      subst.
      eapply MkFMS...
      eapply SwitchEP_equiv...
      apply symmetry...
      assert (Mem sw
             ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws))
             as X...
    Qed.

  End FMS.

  Lemma SwId_eq_Switch : forall (sw1 sw2 : switch) (sws : bag switch),
    UniqSwIds sws ->
    Mem sw1 sws ->
    Mem  sw2 sws ->
    swId sw1 = swId sw2 ->
    sw1 === sw2.
  Proof with eauto.
    idtac "TODO(arjun): skipped infrastructure lemma swId_eq_Switch".
  Admitted.

  Hint Unfold UniqSwIds.

  Lemma simpl_step : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (allFMS1 : AllFMS (switches st1) (ofLinks st1)),
    step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 allFMS2,
      concreteStep
        (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1
                       allFMS1)
        obs
        (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2
                       allFMS2).
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
    exists (AllFMS_untouched1 allFMS1).
    trivial.
    (* Case 3. *)
    simpl in *.
    eexists.
    unfold FlowTablesSafe.
    intros.
    simpl in H0. destruct H0 as [HIn | HIn].
    2: solve [ unfold FlowTablesSafe in tblsOk1; eapply tblsOk1; simpl; eauto ].
    inversion HIn. subst. clear HIn.
    unfold AllFMS in allFMS1.
    apply Bag.mem_prop in allFMS1.
    destruct allFMS1 as [lnk0 [HLnkIn [HEq HFMS]]].
    inversion HFMS; subst.
    inversion H9; subst.
    apply Bag.mem_prop in H11. inversion H11.
    clear HEq HFMS HLnkIn.
    apply Bag.unify_union_singleton in H12.
    destruct H12 as [HEq | HContra].
    inversion HEq. subst...
    apply H11 in HContra.
    inversion HContra.
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    eexists.
    unfold AllFMS.
    intros.
    simpl in H0.
    destruct H0.
    2: solve [ unfold AllFMS in allFMS1; apply allFMS1; right; auto ].
    unfold AllFMS in allFMS1.
    apply Bag.mem_prop in allFMS1.
    destruct allFMS1 as [lnk0 [HLnkIn [HEq HFMS]]].
    simpl in HEq.
    destruct sw.
    inversion H0; subst. clear H0.
    exists lnk0.
    simpl. split... split...
    destruct lnk0; subst.
    eapply MkFMS.
    apply NoFlowModsInBuffer.
    intros.
    inversion HFMS; subst.
    idtac "TODO(arjun): finish flowmod safety case".
    (* Obvious now -- need to the other half of Bag.unify_union_singleton.
       Also, if SafeWire ends with a nobarrier at switchEp, we can always
       construct one that requires a barrier. *)
    admit.
    admit.
    auto.
    (* Case 4. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    idtac "TODO(arjun): FMS for SendPacketOut -- trivial case.".
    admit.
    (* Case 5. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists (LinkTopoOK_inv pks0 (pk::pks0) linksTopoOk1).
    exists (LinksHaveSrc_inv pks0 (pk::pks0) (LinksHaveSrc_untouched haveSrc1)).
    exists (LinksHaveDst_inv pks0 (pk::pks0) (LinksHaveDst_untouched haveDst1)).
    exists (UniqSwIds_pres uniqSwIds1).
    exists (AllFMS_untouched1 allFMS1).
    trivial.
    (* Case 6. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists (LinkTopoOK_inv (pks0 ++ [pk]) pks0 linksTopoOk1).
    exists 
      (LinksHaveSrc_inv (pks0 ++ [pk]) pks0 (LinksHaveSrc_untouched haveSrc1)).
    exists 
      (LinksHaveDst_inv (pks0 ++ [pk]) pks0 (LinksHaveDst_untouched haveDst1)).
    exists (UniqSwIds_pres uniqSwIds1).
    exists (AllFMS_untouched1 allFMS1).
    trivial.
    (* Case 7. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    exists allFMS1.
    trivial.
    (* Case 8. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    idtac "TODO(arjun): FMS for ControllerRecv -- trivial case.".
    admit.
    (* Case 9. *)
    exists tblsOk1.
    exists linksTopoOk1.
    exists haveSrc1.
    exists haveDst1.
    exists uniqSwIds1.
    eexists.
    unfold AllFMS.
    simpl in *.
    intros.
    unfold AllFMS in allFMS1. apply allFMS1 in H1.
    destruct H1 as [lnk [HIn [HLnkIdEq HFMS]]].
    apply in_app_iff in HIn; simpl in HIn.
    destruct HIn as [HIn | [HIn | HIn]].
    solve [ exists lnk; auto with datatypes ].
    2: solve [ exists lnk; auto with datatypes ].
    exists (OpenFlowLink swId0 fromSwitch0 (msg :: fromCtrl)).
    split.
    solve [ auto with datatypes ].
    subst. simpl in *.
    split. solve [ trivial ].
    (* hard part here *)
    clear HLnkIdEq.
    inversion HFMS; subst.
    inversion H4; subst.
    eapply MkFMS...
    destruct H6 as [ctrlEp0 HSafeWire].
    destruct (ControllerFMS _ _ _ _ HSafeWire H) as [ctrlEp1 HSafeWire1].
    solve [ exists ctrlEp1; trivial ].
    eapply MkFMS...
    destruct H6 as [ctrlEp0 HSafeWire].
    destruct (ControllerFMS _ _ _ _ HSafeWire H) as [ctrlEp1 HSafeWire1].
    solve [ exists ctrlEp1; trivial ].
    exact H.
    (* Case 10. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    idtac "TODO(arjun): FMS for SwitchSend -- trivial case.".
    admit.
    (* Case 11. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    idtac "TODO(arjun): FMS for BarrierRecv -- important case".
    admit.
    (* Case 12. *)
    exists (FlowTablesSafe_untouched tblsOk1).
    exists linksTopoOk1.
    exists (LinksHaveSrc_untouched haveSrc1).
    exists (LinksHaveDst_untouched haveDst1).
    exists (UniqSwIds_pres uniqSwIds1).
    idtac "TODO(arjun): FMS for NotBarrierRecv -- important case".
    admit.
  Qed.

  Lemma simpl_multistep : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (allFMS1 : AllFMS (switches st1) (ofLinks st1)),
    multistep step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 allFMS2,
      multistep concreteStep
                (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                               uniqSwIds1 allFMS1)
                obs
                (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                               uniqSwIds2 allFMS2).
  Proof with eauto with datatypes.
    intros.
    induction H.
    (* zero steps. *)
    solve [ eauto 8 ].
    (* tau step *)
    destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                         uniqSwIds1 allFMS1 H)
             as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 
                [allFMS2 step]]]]]].
    destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                          uniqSwIds2 allFMS2)
             as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3
                [uniqSwIds3 [allFMS3 stepN]]]]]].
    solve [ eauto 8 ].
    destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                         uniqSwIds1 allFMS1 H)
             as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 
                [allFMS2 step]]]]]].
    destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                          uniqSwIds2 allFMS2)
             as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3
                [uniqSwIds3 [allFMS3 stepN]]]]]].
    solve [ eauto 8 ].
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
    idtac "Proving relate_step_simpl_tau (Case 2 of 11)...".
    destruct st1. destruct st2. subst. unfold relate. simpl.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 3. PacketOut case. *)
    idtac "Proving relate_step_simpl_tau (Case 3 of 11)...".
    destruct st1. destruct st2. subst. unfold relate. simpl.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 4. SendDataLink case. *)
    idtac "Proving relate_step_simpl_tau (Case 4 of 11)...".
    destruct st1. destruct st2. subst. unfold relate. simpl.
    autorewrite with bag using simpl.
    destruct dst0.
    rewrite -> Bag.from_list_cons.
    idtac "TODO(arjun): trivial use of linkTopo consistency needed.".
    admit.
    (* 5. RecvDataLink case. *)
    idtac "Proving relate_step_simpl_tau (Case 5 of 11)...".
    idtac "TODO(arjun): trivial use of linkTopo consistency needed.".
    admit.
    (* 6. Controller steps *)
    idtac "Proving relate_step_simpl_tau (Case 6 of 11)...".
    unfold relate.
    simpl.
    rewrite -> (ControllerRemembersPackets H2).
    apply reflexivity.
    (* 7. Controller receives. *)
    idtac "Proving relate_step_simpl_tau (Case 7 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    rewrite -> (ControllerRecvRemembersPackets H2).
    bag_perm 100.
    (* 8. Controller sends *)
    idtac "Proving relate_step_simpl_tau (Case 8 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    rewrite -> (ControllerSendForgetsPackets H2).
    bag_perm 100.
    (* 9. Switch sends to controller *)
    idtac "Proving relate_step_simpl_tau (Case 9 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 10. Switch receives a barrier. *)
    idtac "Proving relate_step_simpl_tau (Case 10 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> Bag.unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 11. Switch receives a non-barrier. *)
    idtac "Proving relate_step_simpl_tau (Case 11 of 11)...".
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
      apply Bag.mem_union.
      left.
      simpl.
      apply reflexivity.
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
    simpl.
    autorewrite with bag using simpl.
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
    (uniqSwIds1 : UniqSwIds (switches devs1))
    (allFMS1 : AllFMS (switches devs1) (ofLinks devs1)),
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
               (ConcreteState devs1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1
                              uniqSwIds1 allFMS1)
               [(sw,pt,pk)]
               t.
  Proof with eauto.
    intros.
    destruct (simpl_multistep tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                              uniqSwIds1 allFMS1 H)
             as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 
                [uniqSwIds2 [allFMS2 Hmultistep]]]]]].
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