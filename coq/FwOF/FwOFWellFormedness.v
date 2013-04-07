Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.
Require FwOF.FwOFWellFormednessLemmas.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module Make (Import RelationDefinitions : RELATION_DEFINITIONS) <: RELATION.

  Module RelationDefinitions := RelationDefinitions.
  Import AtomsAndController.
  Import Machine.
  Import Atoms.
  Module Import Lemmas := FwOF.FwOFWellFormednessLemmas.Make (RelationDefinitions).

  Hint Resolve OfLinksHaveSrc_pres1 OfLinksHaveSrc_pres2 OfLinksHaveSrc_pres3.
  Hint Unfold UniqSwIds.
  Hint Resolve P_entails_FlowTablesSafe step_preserves_P.
  Hint Resolve DevicesFromTopo_pres0 DevicesFromTopo_pres1 SwitchesHaveOpenFlowLinks_pres0
       SwitchesHaveOpenFlowLinks_pres1.
  Hint Resolve LinksHaveSrc_untouched.
  Hint Resolve LinksHaveDst_untouched.
  Hint Resolve FlowTablesSafe_untouched.
  Hint Resolve UniqSwIds_pres.
  Hint Resolve FlowTablesSafe_PacketOut.
  Hint Resolve AllDiff_preservation.
  Hint Resolve NoBarriersInCtrlm_preservation.

  Ltac sauto := solve[eauto with datatypes].

  Lemma simpl_step : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (P0 : P (switches st1) (ofLinks st1) (ctrl st1))
    (uniqOfLinkIds1 : AllDiff of_to (ofLinks st1))
    (ofLinksHaveSw1 : OFLinksHaveSw (switches st1) (ofLinks st1))
    (devsFromTopo1 : DevicesFromTopo st1)
    (swsHaveOFLinks1 : SwitchesHaveOpenFlowLinks (switches st1) (ofLinks st1))
    (noBarriersInCtrlm1 : NoBarriersInCtrlm (switches st1)),
    step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 P1
           uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2
           noBarriersInCtrlm2,
      concreteStep
        (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1
                       P0 uniqOfLinkIds1 ofLinksHaveSw1 devsFromTopo1
                       swsHaveOFLinks1 noBarriersInCtrlm1)
        obs
        (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2
                       P1 uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2
                       swsHaveOFLinks2 noBarriersInCtrlm2).
  Proof with simpl;eauto with datatypes.
    intros.
    unfold concreteStep.
    simpl.
    { inversion H; subst; simpl in *.
      (* Case 1. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 2: processed a buffered FlowMod *)
      + eexists.
        unfold FlowTablesSafe.
        intros.
        apply Bag.in_union in H0. simpl in H0.
        { destruct H0 as [[HIn | HContra] | HIn]; 
          [idtac | solve[inversion HContra] | idtac].
          (* interesting case, where the flow table has been modified *)
          - inversion HIn; subst; clear HIn.
            assert (FlowModSafe swId1 tbl0 (({|FlowMod fm|}) <+> ctrlm1)) as J.
            { unfold FlowTablesSafe in tblsOk1.
              eapply tblsOk1.
              apply Bag.in_union. left. simpl... }
            inversion J; subst.
            * assert (NotFlowMod (FlowMod fm)) as Hcontra.
              { apply H0. apply Bag.in_union; simpl... }
              inversion Hcontra.
            * assert (FlowMod fm = FlowMod f /\ ctrlm1 = ctrlm0) as HEq.
              { eapply Bag.singleton_union_disjoint.
                apply Bag.union_from_ordered in H0...
                intros.
                assert (NotFlowMod (FlowMod fm)) as X...
                inversion X. }
              destruct HEq as [HEq HEq0].
              inversion HEq; subst...
              apply NoFlowModsInBuffer...
          - unfold FlowTablesSafe in tblsOk1.
            eapply tblsOk1.
            apply Bag.in_union... } 
        eexists...
        eexists...
        eexists...
        eexists...
        eexists...
        eexists...
        eexists...
        eexists. sauto.
        eexists. sauto.
        eexists.
        { unfold NoBarriersInCtrlm in *.
          intros.
          apply Bag.in_union in H0; simpl in H0.
          destruct H0 as [[H0|H0]|H0].
          + refine (noBarriersInCtrlm1 (Switch swId0 pts0 tbl0 inp0 outp0 (({|FlowMod fm|}) <+> ctrlm0) switchm0) _ _ _).
            - apply Bag.in_union. simpl...
            - apply Bag.in_union.
              rewrite <- H0 in H1.
              simpl in H1.
              right...
          + inversion H0.
          + refine (noBarriersInCtrlm1 sw _ _ _)...
            apply Bag.in_union... }
        sauto.
      (* Case 4. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. 
        { unfold NoBarriersInCtrlm in *.
          intros.
          apply Bag.in_union in H0; simpl in H0.
          destruct H0 as [[H0|H0]|H0].
          + refine (noBarriersInCtrlm1 (Switch swId0 pts0 tbl0 inp0 outp0 (({|PacketOut pt pk|}) <+> ctrlm0) switchm0) _ _ _).
            - apply Bag.in_union. simpl...
            - apply Bag.in_union.
              rewrite <- H0 in H1.
              simpl in H1.
              right...
          + inversion H0.
          + refine (noBarriersInCtrlm1 sw _ _ _)...
            apply Bag.in_union... }
        sauto.
      (* Case 5. *)
      + eexists. sauto.
        exists (LinkTopoOK_inv pks0 (pk::pks0) linksTopoOk1).
        exists (LinksHaveSrc_inv pks0 (pk::pks0) (LinksHaveSrc_untouched haveSrc1)).
        exists (LinksHaveDst_inv pks0 (pk::pks0) (LinksHaveDst_untouched haveDst1)).
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 6. *)
      + eexists. sauto.
        exists (LinkTopoOK_inv (pks0 ++ [pk]) pks0 linksTopoOk1).
        exists 
          (LinksHaveSrc_inv (pks0 ++ [pk]) pks0 (LinksHaveSrc_untouched haveSrc1)).
        exists 
          (LinksHaveDst_inv (pks0 ++ [pk]) pks0 (LinksHaveDst_untouched haveDst1)).
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 7. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 8. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. 
        { eapply AllDiff_preservation.
          exact uniqOfLinkIds1.
          do 2 rewrite -> map_app... }
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 9. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists.
        { eapply AllDiff_preservation.
          exact uniqOfLinkIds1.
          do 2 rewrite -> map_app... }
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 10. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists.
        { eapply AllDiff_preservation.
          exact uniqOfLinkIds1.
          do 2 rewrite -> map_app... }
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      (* Case 11. *)
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists.
        { eapply AllDiff_preservation.
          exact uniqOfLinkIds1.
          do 2 rewrite -> map_app... }
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        sauto.
      + eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists.
        { eapply AllDiff_preservation.
          exact uniqOfLinkIds1.
          do 2 rewrite -> map_app... }
        eexists. sauto.
        eexists. sauto.
        eexists. sauto.
        eexists. 
        { unfold NoBarriersInCtrlm in *.
          intros.
          apply Bag.in_union in H1; simpl in H1.
          destruct H1 as [[H1|H1]|H1].
          + rewrite <- H1 in H2.
            simpl in H2.
            apply Bag.in_union in H2. simpl in H2.
            destruct H2 as [[H2|H2]|H2].
            * subst. exact H0.
            * inversion H2.
            * refine (noBarriersInCtrlm1 (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) _ _ _).
              apply Bag.in_union. simpl...
              simpl... 
          + inversion H1.
          + refine (noBarriersInCtrlm1 sw _ _ _)...
            apply Bag.in_union... }
        sauto.
    (* epic fail of a proof script *)
    Grab Existential Variables. 
    eauto. eauto. eauto. eauto. eauto. eauto. eauto. 
    eauto. eauto. eauto. eauto. eauto. eauto. eauto. 
    eauto. eauto. eauto. eauto. eauto. eauto. eauto. 
    eauto. eauto. eauto. eauto. eauto. eauto. eauto. 
    eauto. eauto. eauto. eauto. eauto. eauto. eauto. 
    eauto. eauto. 
    }
  Qed.

  Lemma simpl_multistep' : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (P1 : P (switches st1) (ofLinks st1) (ctrl st1))
    (uniqOfLinkIds1 : AllDiff of_to (ofLinks st1))
    (ofLinksHaveSw1 : OFLinksHaveSw (switches st1) (ofLinks st1))
    (devsFromTopo1 : DevicesFromTopo st1)
    (swsHaveOFLinks1 : SwitchesHaveOpenFlowLinks (switches st1) (ofLinks st1))
    (noBarriersInCtrlm1 : NoBarriersInCtrlm (switches st1)),
    multistep step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 P2
           uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2
           noBarriersInCtrlm2,
      multistep concreteStep
                (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                               uniqSwIds1 P1 uniqOfLinkIds1
                               ofLinksHaveSw1 devsFromTopo1 swsHaveOFLinks1
                               noBarriersInCtrlm1)
                obs
                (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                               uniqSwIds2 P2 uniqOfLinkIds2
                               ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2
                               noBarriersInCtrlm2).
  Proof with eauto with datatypes.
    intros.
    induction H.
    (* zero steps. *)
    + solve [ eauto 13 ].
    (* tau step *)
    + destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                           uniqSwIds1 P1 uniqOfLinkIds1 ofLinksHaveSw1
                           devsFromTopo1 swsHaveOFLinks1
                           noBarriersInCtrlm1
                           H)
        as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 
           [P2 [uniqOfLinkIds2 [ofLinksHaveSw2 
           [devsFromTopo2 [swsHaveOFLinks2 
           [noBarriersInCtrlm2 step]]]]]]]]]]].
      destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                            uniqSwIds2 P2 uniqOfLinkIds2 ofLinksHaveSw2
                            devsFromTopo2 swsHaveOFLinks2 noBarriersInCtrlm2)
        as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3
                [uniqSwIds3 [PN [uniqOfLinkIdsN [ofLinksHaveSwN 
           [devsFromTopoN [swsHaveOFLinksN [noBarriersInCtrlmN stepN]]]]]]]]]]].
      solve [ eauto 13 ].
    (* The script below is an identical copy of the case above. *)
    + destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                           uniqSwIds1 P1 uniqOfLinkIds1 ofLinksHaveSw1
                           devsFromTopo1 swsHaveOFLinks1
                           noBarriersInCtrlm1
                           H)
        as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 
           [P2 [uniqOfLinkIds2 [ofLinksHaveSw2 
           [devsFromTopo2 [swsHaveOFLinks2 
           [noBarriersInCtrlm2 step]]]]]]]]]]].
      destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                            uniqSwIds2 P2 uniqOfLinkIds2 ofLinksHaveSw2
                            devsFromTopo2 swsHaveOFLinks2 noBarriersInCtrlm2)
        as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3
                [uniqSwIds3 [PN [uniqOfLinkIdsN [ofLinksHaveSwN 
           [devsFromTopoN [swsHaveOFLinksN [noBarriersInCtrlmN stepN]]]]]]]]]]].
      solve [ eauto 13 ].
  Qed.

  Lemma simpl_multistep : forall (st1 : concreteState) (devs2 : state) obs,
    multistep step (devices st1) obs devs2 ->
    exists (st2 : concreteState),
      devices st2 = devs2 /\
      multistep concreteStep st1 obs st2.
  Proof with simpl;auto.
    intros.
    destruct st1.
    destruct (simpl_multistep' concreteState_flowTableSafety0
      concreteState_consistentDataLinks0 linksHaveSrc0 linksHaveDst0
      uniqSwIds0 ctrlP0 uniqOfLinkIds0 ofLinksHaveSw0 devicesFromTopo0
      swsHaveOFLinks0 noBarriersInCtrlm0 H) as [v0 [v1 [v2 [v3 [v4 [v5 [v6 [v7 [v8 [v9 [v10 Hstep]]]]]]]]]]].
    exists (ConcreteState devs2 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)...
  Qed.

  Lemma relate_step_simpl_tau : forall st1 st2,
    concreteStep st1 None st2 ->
    relate (devices st1) = relate (devices st2).
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
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
    assert (topo (src (DataLink (swId0, pt) pks0 (s,p))) =
            Some (dst (DataLink (swId0, pt) pks0 (s,p)))) as Jtopo.
    {
      unfold ConsistentDataLinks in concreteState_consistentDataLinks0.
      apply concreteState_consistentDataLinks0.
        simpl in H1.
        rewrite <- H1.
        simpl... }
    simpl in Jtopo.
    rewrite -> Jtopo.
    bag_perm 100.
    (* 5. RecvDataLink case. *)
    idtac "Proving relate_step_simpl_tau (Case 5 of 11)...".
    destruct st1. destruct st2. subst. unfold relate. simpl.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 6. Controller steps *)
    idtac "Proving relate_step_simpl_tau (Case 6 of 11)...".
    unfold relate.
    simpl.
    rewrite -> (ControllerRemembersPackets H2).
    reflexivity.
    (* 7. Controller receives. *)
    idtac "Proving relate_step_simpl_tau (Case 7 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> unions_app.
    autorewrite with bag using simpl.
    rewrite -> (ControllerRecvRemembersPackets H2).
    bag_perm 100.
    (* 8. Controller sends *)
    idtac "Proving relate_step_simpl_tau (Case 8 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> unions_app.
    autorewrite with bag using simpl.
    rewrite -> (ControllerSendForgetsPackets H2).
    bag_perm 100.
    (* 9. Switch sends to controller *)
    idtac "Proving relate_step_simpl_tau (Case 9 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 10. Switch receives a barrier. *)
    idtac "Proving relate_step_simpl_tau (Case 10 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
    (* 11. Switch receives a non-barrier. *)
    idtac "Proving relate_step_simpl_tau (Case 11 of 11)...".
    unfold relate.
    simpl.
    repeat rewrite -> map_app.
    simpl.
    repeat rewrite -> unions_app.
    autorewrite with bag using simpl.
    bag_perm 100.
  Qed.

  Lemma relate_multistep_simpl_tau : forall st1 st2,
    multistep concreteStep st1 nil st2 ->
    relate (devices st1) = relate (devices st2).
  Proof with eauto.
    intros.
    remember nil.
    induction H...
    + apply relate_step_simpl_tau in H.
      rewrite -> H...
    + inversion Heql.
  Qed.

  Lemma relate_step_simpl_obs : forall  sw pt pk lps st1 st2,
    relate (devices st1) = ({| (sw,pt,pk) |} <+> lps) ->
    concreteStep st1 (Some (sw,pt,pk)) st2 ->
    relate (devices st2) = 
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).
  Proof with eauto with datatypes.
    intros.
    inversion H0.
    destruct st1.
    destruct st2.
    destruct devices0.
    destruct devices1.
    subst.
    simpl in *.
    inversion H1; subst; clear H1.
    inversion H6; subst; clear H6.
    assert (FlowTableSafe sw tbl0) as Z.
    { assert (FlowModSafe sw tbl0 ctrlm0) as Z.
      { unfold FlowTablesSafe in concreteState_flowTableSafety0.
        eapply concreteState_flowTableSafety0...
        apply Bag.in_union; simpl... }
      unfold FlowTableSafe in Z.
      inversion Z... }
    remember (Z pt pk outp' pksToCtrl H3) as Y eqn:X. clear X Z.
    rewrite <- Y. clear Y.

    unfold relate in *.
    simpl in *.
    autorewrite with bag using simpl.
    apply (Bag.pop_union_r _ ({|(sw,pt,pk)|})).
    repeat rewrite -> Bag.union_assoc.
    rewrite -> (Bag.union_comm _ lps).
    rewrite <- H.
    simpl.
    autorewrite with bag using simpl.
    bag_perm 100.
  Qed.

  Lemma relate_multistep_simpl_obs : forall  sw pt pk lps st1 st2,
    relate (devices st1) = ({| (sw,pt,pk) |} <+> lps) ->
    multistep concreteStep st1 [(sw,pt,pk)] st2 ->
    relate (devices st2) = 
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).
  Proof with eauto.
    intros.
    remember [(sw,pt,pk)] as obs.
    induction H0; subst.
    inversion Heqobs.
    apply IHmultistep...
    apply relate_step_simpl_tau in H0.
    symmetry in H0.
    rewrite -> H0...
    destruct obs; inversion Heqobs.
    subst.
    clear Heqobs.
    apply relate_multistep_simpl_tau in H1.
    apply relate_step_simpl_obs with (lps := lps) in H0 .
    rewrite <- H0.
    symmetry...
    trivial.
  Qed.

  Lemma simpl_weak_sim : forall st1 devs2 sw pt pk lps,
    multistep step (devices st1) [(sw,pt,pk)] devs2 ->
    relate (devices st1) = ({| (sw,pt,pk) |} <+> lps) ->
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps) ->
   exists st2 : concreteState,
     inverse_relation 
       bisim_relation
       (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps)
       st2 /\
     multistep concreteStep st1 [(sw,pt,pk)] st2.
  Proof with eauto.
    intros.
    destruct (simpl_multistep st1 H) as [st2 [Heq Hmultistep]]. 
    assert (relate (devices st1) = ({| (sw,pt,pk) |} <+> lps)) as Hrel.
      subst. simpl...
    exists st2.
    split.
    unfold inverse_relation.
    unfold bisim_relation.
    symmetry...
    exact (relate_multistep_simpl_obs Hrel Hmultistep).
    trivial.
  Qed.

End Make.
