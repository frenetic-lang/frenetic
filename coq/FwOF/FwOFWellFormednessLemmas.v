Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module Make (Import RelationDefinitions : RELATION_DEFINITIONS).

  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Lemma LinksHaveSrc_untouched : forall 
    {swId tbl pts sws links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveSrc 
      ({| Switch swId pts tbl inp outp ctrlm switchm |} <+> sws)  links ->
    LinksHaveSrc 
      ({| Switch swId pts tbl' inp' outp' ctrlm' switchm' |} <+> sws)
      links.
  Proof with auto.
    intros.
    unfold LinksHaveSrc in *.
    intros.
    apply H in H0. clear H.
    unfold LinkHasSrc in *.
    destruct H0 as [sw [HMem [HEq HIn]]].
    simpl in HMem.
    rewrite -> Bag.in_union in HMem.
    destruct HMem.
    + destruct sw.
      simpl in *.
      destruct H.
      inversion H.
      subst.
      eexists.
      split. 
      apply Bag.in_union. left. simpl. left. reflexivity.
      simpl...
      inversion H.
    + exists sw.
      split...
      simpl...
      apply Bag.in_union...
  Qed.

  Lemma LinksHaveDst_untouched : forall 
    {swId tbl pts sws links
    inp outp ctrlm switchm tbl' inp' outp' ctrlm' switchm' },
    LinksHaveDst
      ({| Switch swId pts tbl inp outp ctrlm switchm |} <+>  sws)  links ->
    LinksHaveDst 
      ({| Switch swId pts tbl' inp' outp' ctrlm' switchm' |} <+> sws)
      links.
  Proof with auto.

    intros.
    unfold LinksHaveDst in *.
    intros.
    apply H in H0. clear H.
    unfold LinkHasDst in *.
    destruct H0 as [sw [HMem [HEq HIn]]].
    simpl in HMem.
    rewrite -> Bag.in_union in HMem.
    destruct HMem.
    + destruct sw.
      simpl in *.
      destruct H.
      inversion H.
      subst.
      eexists.
      split. 
      apply Bag.in_union. left. simpl. left. reflexivity.
      simpl...
      inversion H.
    + exists sw.
      split...
      simpl...
      apply Bag.in_union...
  Qed.

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


  Lemma FlowTablesSafe_untouched : forall {sws swId pts tbl inp inp'
    outp outp' ctrlm switchm switchm' },
    FlowTablesSafe
      ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws) ->
    FlowTablesSafe 
      ({|Switch swId pts tbl inp' outp' ctrlm switchm'|} <+> sws).
  Proof with eauto.
    intros.
    unfold FlowTablesSafe in *.
    intros.
    simpl in H0.
    apply Bag.in_union in H0; simpl in H0.
    destruct H0 as [[H0 | H0] | H0].
    + inversion H0; subst; clear H0.
      eapply H...
      apply Bag.in_union; simpl...
    + inversion H0.
    + eapply H.
      apply Bag.in_union...
  Qed.

  Lemma FlowModSafe_PacketOut : forall swId tbl pt pk ctrlm,
    FlowModSafe swId tbl (({|PacketOut pt pk|}) <+> ctrlm) ->
    FlowModSafe swId tbl ctrlm.
  Proof with eauto.
    intros.
    inversion H; subst.
    + apply NoFlowModsInBuffer...
      intros. apply H0. apply Bag.in_union...
    + remember (Bag2Lemmas.union_from_ordered _ _ _ _ _ H0) as J0 eqn:X; clear X.
      clear H0.
      assert (In (FlowMod f) (to_list ctrlm0)) as X.
      { assert (In (FlowMod f) (to_list (({|PacketOut pt pk|}) <+> ctrlm0))).
        { rewrite <- J0.
          rewrite -> Bag.in_union; simpl... }
        rewrite -> Bag.in_union in H0.
        simpl in H0; destruct H0 as [[H0|H0]|H0]; solve [auto;inversion H0]. }
      apply Bag.in_split with (Order:=TotalOrder_fromController) in X.
      destruct X as [rest Heq].
      subst.
      eapply OneFlowModsInBuffer...
      intros.
      rewrite <- Bag.union_assoc in J0.
      rewrite -> (Bag.union_comm _ ({|PacketOut pt pk|})) in J0.
      rewrite -> Bag.union_assoc in J0.
      apply Bag.pop_union_l in J0.
      subst.
      apply H1...
      apply Bag.in_union...
  Qed.

  Lemma FlowTablesSafe_PacketOut : forall sws swId pts tbl inp inp'
    outp outp' ctrlm switchm switchm' pt pk,
    FlowTablesSafe
      ({|Switch swId pts tbl inp outp (({|PacketOut pt pk|}) <+> ctrlm) switchm|} <+> sws) ->
    FlowTablesSafe 
      ({|Switch swId pts tbl inp' outp' ctrlm switchm'|} <+> sws).
  Proof with eauto.
    intros.
    unfold FlowTablesSafe in *.
    intros.
    simpl in H0.
    apply Bag.in_union in H0; simpl in H0.
    destruct H0 as [[H0 | H0] | H0].
    + inversion H0; subst; clear H0.
      assert (FlowModSafe swId1 tbl1 (({|PacketOut pt pk|} <+> ctrlm1))) as X.
      { eapply H...
        apply Bag.in_union; simpl... }
      eapply FlowModSafe_PacketOut...
    + inversion H0.
    + eapply H. apply Bag.in_union. right...
  Qed.

  Lemma LinksHaveSrc_inv : forall {sws links links0 src dst} pks pks',
    LinksHaveSrc sws (links ++ (DataLink src pks dst) :: links0) ->
    LinksHaveSrc sws (links ++ (DataLink src pks' dst) :: links0).
  Proof with auto with datatypes.
    intros.
    unfold LinksHaveSrc in *.
    intros.
    apply in_app_iff in H0.
    simpl in H0.
    destruct H0 as [H0 | [H0 | H0]]; subst...
    destruct (H (DataLink src0 pks0 dst0))...
    destruct H0 as [HMem [HEq HIn]].
    simpl in *.
    unfold LinkHasSrc.
    exists x...
  Qed.

  Lemma LinksHaveDst_inv : forall {sws links links0 src dst} pks pks',
    LinksHaveDst sws (links ++ (DataLink src pks dst) :: links0) ->
    LinksHaveDst sws (links ++ (DataLink src pks' dst) :: links0).
  Proof with auto with datatypes.
    intros.
    unfold LinksHaveDst in *.
    intros.
    apply in_app_iff in H0.
    simpl in H0.
    destruct H0 as [H0 | [H0 | H0]]; subst...
    destruct (H (DataLink src0 pks0 dst0))...
    destruct H0 as [HMem [HEq HIn]].
    simpl in *.
    unfold LinkHasDst.
    exists x...
  Qed.

  Lemma UniqSwIds_pres : forall {sws swId pts tbl inp outp ctrlm switchm
    pts' tbl' inp' outp' ctrlm' switchm'},
    UniqSwIds ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws) ->
    UniqSwIds ({|Switch swId pts' tbl' inp' outp' ctrlm' switchm'|} <+> sws).
  Proof with auto with datatypes.
    intros.
    unfold UniqSwIds in *.
    apply Bag.AllDiff_preservation with (x := Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)...
  Qed.

  Lemma OfLinksHaveSrc_pres1 : forall { sws swId pts1 tbl1 inp1 outp1 ctrlm1 
    switchm1 pts2 tbl2 inp2 outp2 ctrlm2 switchm2 switchmLst1 ctrlmLst1
    switchmLst2 ctrlmLst2 ofLinks1 ofLinks2 },
    OFLinksHaveSw
      ({|Switch swId pts1 tbl1 inp1 outp1 ctrlm1 switchm1|} <+> sws)
      (ofLinks1 ++ OpenFlowLink swId switchmLst1 ctrlmLst1 :: ofLinks2) ->
    OFLinksHaveSw
      ({|Switch swId pts2 tbl2 inp2 outp2 ctrlm2 switchm2|} <+> sws)
      (ofLinks1 ++ OpenFlowLink swId switchmLst2 ctrlmLst2 :: ofLinks2).
  Proof with auto with datatypes.
    intros.
    unfold OFLinksHaveSw in *.
    intros.
    destruct ofLink.
    unfold ofLinkHasSw in *.
    apply in_app_iff in H0; simpl in H0; destruct H0 as [H0 | [H0 | H0]]; subst.
    + destruct H with (ofLink := OpenFlowLink of_to0 of_switchm0 of_ctrlm0)
        as [sw [HMem HEq]]...
      assert ({ of_to0 = swId0 } + { of_to0 <> swId0 }) as J by (apply TotalOrder.eqdec).
      destruct J; subst.
      - exists (Switch swId0 pts2 tbl2 inp2 outp2 ctrlm2 switchm2).
        split...
        apply Bag.in_union.
        left.
        simpl...
      - exists sw.
        destruct sw.
        simpl in HEq.
        subst.
        split...
        apply Bag.in_union.
        right.
        apply Bag.in_union in HMem.
        destruct HMem.
        * simpl in H1.
          destruct H1.
          inversion H1.
          subst.
          contradiction n...
          inversion H1.
        * trivial.
    + inversion H0; subst; clear H0.
      exists (Switch of_to0 pts2 tbl2 inp2 outp2 ctrlm2 switchm2).
      split...
      apply Bag.in_union.
      left.
      simpl...
    + destruct H with (ofLink := OpenFlowLink of_to0 of_switchm0 of_ctrlm0) 
        as [sw [HMem HEq]]...
      assert ({ of_to0 = swId0 } + { of_to0 <> swId0 }) as J by (apply TotalOrder.eqdec).
      destruct J; subst.
      - exists (Switch swId0 pts2 tbl2 inp2 outp2 ctrlm2 switchm2).
        split... apply Bag.in_union. left. simpl...
      - exists sw.
        destruct sw.
        simpl in HEq.
        subst.
        split...
        apply Bag.in_union.
        right.
        apply Bag.in_union in HMem.
        destruct HMem...
        simpl in H1.
        destruct H1.
        * inversion H1.
          subst.
          contradiction n...
        * inversion H1.
  Qed.

  Lemma OfLinksHaveSrc_pres2 : forall { sws swId pts1 tbl1 inp1 outp1 ctrlm1 
    switchm1 pts2 tbl2 inp2 outp2 ctrlm2 switchm2 ofLinks },
    OFLinksHaveSw
      ({|Switch swId pts1 tbl1 inp1 outp1 ctrlm1 switchm1|} <+> sws)
      ofLinks ->
    OFLinksHaveSw
      ({|Switch swId pts2 tbl2 inp2 outp2 ctrlm2 switchm2|} <+> sws)
      ofLinks.
  Proof with auto with datatypes.
    intros.
    unfold OFLinksHaveSw in *.
    intros.
    destruct ofLink.
    apply H in H0.
    clear H.
    unfold ofLinkHasSw in *.
    destruct H0 as [sw [HMem HEq]].
    simpl in HEq.
    destruct sw.
    assert ({ of_to0 = swId0 } + { of_to0 <> swId0 }) as J by (apply TotalOrder.eqdec).
    destruct J; subst.
    simpl in *. subst.
    exists (Switch swId0 pts2 tbl2 inp2 outp2 ctrlm2 switchm2).
    split... 
    + apply Bag.in_union.
      left.
      simpl...
    + exists (Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0).
      apply Bag.in_union in HMem.
      destruct HMem.
      - simpl in H.
        destruct H.
        * inversion H.
          subst.
          simpl in n.
          contradiction n...
        * inversion H.
      - split...
        apply Bag.in_union...
  Qed.

  Lemma OfLinksHaveSrc_pres3 : forall { sws swId switchmLst1 ctrlmLst1
    switchmLst2 ctrlmLst2 ofLinks1 ofLinks2 },
    OFLinksHaveSw sws
      (ofLinks1 ++ OpenFlowLink swId switchmLst1 ctrlmLst1 :: ofLinks2) ->
    OFLinksHaveSw sws
      (ofLinks1 ++ OpenFlowLink swId switchmLst2 ctrlmLst2 :: ofLinks2).
  Proof with auto with datatypes.
    intros.
    unfold OFLinksHaveSw in *.
    intros.
    destruct ofLink.
    apply in_app_iff in H0; simpl in H0; destruct H0 as [H0 | [H0 | H0]]; subst...
    inversion H0; subst; clear H0.
    unfold ofLinkHasSw in *.
    destruct (H (OpenFlowLink of_to0 switchmLst1 ctrlmLst1)) as [sw [HMem HEq]]...
    exists sw...
  Qed.

  Hint Resolve OfLinksHaveSrc_pres1 OfLinksHaveSrc_pres2 OfLinksHaveSrc_pres3.

  Hint Unfold UniqSwIds.
  Hint Resolve P_entails_FlowTablesSafe.

  Section DevicesFromTopo.
    
    Hint Unfold DevicesFromTopo.

    Lemma DevicesFromTopo_pres0 : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0
      switchm0 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 sws links0 ofLinks0
      ctrl0,
      DevicesFromTopo
        (State ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws)
               links0
               ofLinks0
               ctrl0) ->
      DevicesFromTopo
        (State ({|Switch swId0 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|} <+> sws)
               links0
               ofLinks0
               ctrl0).
    Proof with simpl; eauto.
      intros.
      unfold DevicesFromTopo in *.
      intros.
      apply H in H0.
      clear H.
      destruct H0 as [sw0 [sw1 [lnk [HMemSw0 [HMemSw1 [HInLnk [HSw0Eq [HSw1Eq
                     [HLnkSrcEq HLnkDstEq]]]]]]]]].
      destruct lnk.
      simpl in *.
      destruct sw0.
      destruct sw1.
      simpl in *.
      subst.
      apply Bag.in_union in HMemSw0.
      apply Bag.in_union in HMemSw1.
      destruct HMemSw0; destruct HMemSw1.
      + eexists. eexists. eexists.
        simpl in H; simpl in H0.
        destruct H0; destruct H...
        inversion H; inversion H0...
        subst...
        subst...
        split...
        apply Bag.in_union.
        left...
        split...
        apply  Bag.in_union. left...
        split...
        inversion H.
        inversion H0.
        inversion H.
      + do 3 eexists.
        simpl in H.
        destruct H. 2: solve[inversion H].
        inversion H. subst.
        repeat split.
        apply Bag.in_union. left. simpl. left. reflexivity.
        apply Bag.in_union. right. exact H0.
        exact HInLnk.
        trivial.
        trivial.
        trivial.
        trivial.
      + do 3 eexists.
        simpl in H0.
        destruct H0. 2: solve[inversion H0].
        inversion H0; subst; clear H0.
        repeat split...
        apply Bag.in_union. right. exact H.
        apply Bag.in_union. left. simpl. left. reflexivity.
        trivial.
        trivial.
      + do 3 eexists.
        repeat split...
        apply Bag.in_union. right. exact H.
        apply Bag.in_union. right. exact H0.
        trivial.
        trivial.
    Qed.

    Lemma DevicesFromTopo_pres1 : forall sws0 links0 links1 src
      dst pks0 pks1 ofLinks0 ctrl0,
      DevicesFromTopo
        (State sws0
               (links0 ++ DataLink src pks0 dst :: links1)
               ofLinks0
               ctrl0) ->
      DevicesFromTopo
        (State sws0
               (links0 ++ DataLink src pks1 dst :: links1)
               ofLinks0
               ctrl0).
    Proof with simpl; eauto with datatypes.
      intros.
      unfold DevicesFromTopo in *.
      intros.
      apply H in H0.
      clear H.
      destruct H0 as [sw0 [sw1 [lnk [HMemSw0 [HMemSw1 [HInLnk [HSw0Eq [HSw1Eq
                     [HLnkSrcEq HLnkDstEq]]]]]]]]].
      destruct lnk.
      simpl in *.
      destruct sw0.
      destruct sw1.
      simpl in *.
      subst.
      rewrite -> in_app_iff in HInLnk. simpl in HInLnk.
      destruct HInLnk as [HInLnk | [HInLnk | HInLnk]].
      + repeat eexists.
        exact HMemSw0.
        exact HMemSw1.
        instantiate (1 := DataLink (swId1,pt1) pks2 (swId0,pt0)).
        auto with datatypes.
        simpl...
        simpl...
        simpl...
        simpl...
      + repeat eexists.
        exact HMemSw0.
        exact HMemSw1.
        instantiate (1 := DataLink (swId1,pt1) pks1 (swId0,pt0)).
        inversion HInLnk. subst.
        auto with datatypes.
        simpl...
        simpl...
        simpl...
        simpl...
      + repeat eexists.
        exact HMemSw0.
        exact HMemSw1.
        instantiate (1 := DataLink (swId1,pt1) pks2 (swId0,pt0)).
        auto with datatypes.
        simpl...
        simpl...
        simpl...
        simpl...
    Qed.
        
  End DevicesFromTopo.

  Section SwitchesHaveOpenFlowLinks.
    
    Hint Unfold SwitchesHaveOpenFlowLinks.

    Lemma SwitchesHaveOpenFlowLinks_pres0 : forall swId0 pts0 tbl0 inp0 outp0 
      ctrlm0 switchm0 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 sws ofLinks0,
      SwitchesHaveOpenFlowLinks
        ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws)
        ofLinks0 ->
      SwitchesHaveOpenFlowLinks
        ({|Switch swId0 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|} <+> sws)
               ofLinks0.
    Proof with simpl; eauto.
      intros.
      unfold SwitchesHaveOpenFlowLinks in *.
      intros.
      simpl in *.
      apply Bag.in_union in H0.
      simpl in H0.
      destruct H0 as [[H0 | H0] | H0].
      + destruct sw.
        inversion H0; subst; clear H0.
        edestruct H as [lnk [HIn HEq]]...
        apply Bag.in_union. simpl. left. left. reflexivity.
        destruct lnk.
        simpl in *.
        subst.
        eexists...
      + inversion H0.
      + edestruct H as [lnk [HIn HEq]]...
        apply Bag.in_union. right...
    Qed.

    Lemma SwitchesHaveOpenFlowLinks_pres1 : forall sws0 ofLinks0
      ofLinks1 swId switchm0 ctrlm0 switchm1 ctrlm1,
      SwitchesHaveOpenFlowLinks
        sws0
        (ofLinks0 ++ OpenFlowLink swId switchm0 ctrlm0 :: ofLinks1) ->
      SwitchesHaveOpenFlowLinks
        sws0
        (ofLinks0 ++ OpenFlowLink swId switchm1 ctrlm1 :: ofLinks1).
      Proof with eauto with datatypes.
        unfold SwitchesHaveOpenFlowLinks.
        intros.
        simpl in *.
        apply H in H0.
        clear H.
        destruct H0 as [ofLink [HIn HIdEq]].
        apply in_app_iff in HIn.
        simpl in HIn.
        destruct HIn as [HIn | [HIn | HIn]].
        + eexists...
        + exists (OpenFlowLink swId0 switchm1 ctrlm1).
          split...
          subst.
          simpl in *...
        + eexists...
      Qed.
        
  End SwitchesHaveOpenFlowLinks.

  Section NoBarriersInCtrlm.

  Lemma NoBarriersInCtrlm_preservation : forall swId0 pts0 tbl0 inp0 outp0
      ctrlm0 switchm0 tbl1 inp1 outp1 switchm1 sws,
    NoBarriersInCtrlm ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws) ->
    NoBarriersInCtrlm ({|Switch swId0 pts0 tbl1 inp1 outp1 ctrlm0 switchm1|} <+> sws).
  Proof with eauto with datatypes.
    unfold NoBarriersInCtrlm.
    intros.
    apply Bag.in_union in H0; simpl in H0.
    destruct H0 as [[H0|H0]|H0].
    + subst.
      refine (H (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) _ m _)...
      apply Bag.in_union. simpl...
    + inversion H0.
    + refine (H sw _ m _)...
      apply Bag.in_union...
  Qed.

  End NoBarriersInCtrlm.

End Make.
