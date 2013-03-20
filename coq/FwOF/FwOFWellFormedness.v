Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module Make (AtomsAndController_ : ATOMS_AND_CONTROLLER) <: RELATION.

  Module AtomsAndController := AtomsAndController_.
  Import AtomsAndController_.
  Import Machine.
  Import Atoms.

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition FlowTablesSafe (sws : bag switch_le) : Prop :=
    forall swId pts tbl inp outp ctrlm switchm,
      In (Switch swId pts tbl inp outp ctrlm switchm) (to_list sws) ->
      FlowTableSafe swId tbl.

  Definition ConsistentDataLinks (links : list dataLink) : Prop :=
    forall (lnk : dataLink),
      In lnk links ->
      topo (src lnk) = Some (dst lnk).

  Definition LinkHasSrc (sws : bag switch_le) (link : dataLink) : Prop :=
    exists switch,
      In switch (to_list sws) /\
      fst (src link) = swId switch /\
      In (snd (src link)) (pts switch).

  Definition LinkHasDst (sws : bag switch_le) (link : dataLink) : Prop :=
    exists switch,
      In switch (to_list sws) /\
      fst (dst link) = swId switch /\
      In (snd (dst link)) (pts switch).

  Definition LinksHaveSrc (sws : bag switch_le) (links : list dataLink) :=
    forall link, In link links -> LinkHasSrc sws link.

  Definition LinksHaveDst (sws : bag switch_le) (links : list dataLink) :=
    forall link, In link links -> LinkHasDst sws link.

  Definition UniqSwIds (sws : bag switch_le) := AllDiff swId (to_list sws).

  Definition ofLinkHasSw (sws : bag switch_le) (ofLink : openFlowLink) :=
    exists sw,
      In sw (to_list sws) /\
      of_to ofLink = swId sw.

  Definition OFLinksHaveSw (sws : bag switch_le) (ofLinks : list openFlowLink) :=
    forall ofLink, In ofLink ofLinks -> ofLinkHasSw sws ofLink.

  Definition DevicesFromTopo (devs : state) :=
    forall swId0 swId1 pt0 pt1,
      Some (swId0,pt0) = topo (swId1,pt1) ->
      exists sw0 sw1 lnk,
        In sw0 (to_list (switches devs)) /\
        In sw1 (to_list (switches devs)) /\
        In lnk (links devs) /\
        swId sw0 = swId0 /\
        swId sw1 = swId1 /\
        src lnk = (swId1,pt1) /\
        dst lnk = (swId0, pt0).

  Definition SwitchesHaveOpenFlowLinks (devs : state) :=
    forall sw,
      In sw (to_list (switches devs)) ->
      exists ofLink,
        In ofLink (ofLinks devs) /\
        swId sw = of_to ofLink.

  Record concreteState := ConcreteState {
    devices : state;
    concreteState_flowTableSafety : FlowTablesSafe (switches devices);
    concreteState_consistentDataLinks : ConsistentDataLinks (links devices);
    linksHaveSrc : LinksHaveSrc (switches devices) (links devices);
    linksHaveDst : LinksHaveDst (switches devices) (links devices);
    uniqSwIds : UniqSwIds (switches devices);
    allFMS : AllFMS (switches devices) (ofLinks devices);
    ctrlP : P (switches devices) (ofLinks devices) (ctrl devices);
    uniqOfLinkIds : AllDiff of_to (ofLinks devices);
    ofLinksHaveSw : OFLinksHaveSw (switches devices) (ofLinks devices);
    devicesFromTopo : DevicesFromTopo devices;
    swsHaveOFLinks : SwitchesHaveOpenFlowLinks devices
  }.

  Implicit Arguments ConcreteState [].

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (devices st) obs (devices st0).

  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).

  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        from_list (map (affixSwitch swId) (to_list inp)) <+>
        unions (map (transfer swId) (to_list outp)) <+>
        unions (map (select_packet_out swId) (to_list ctrlm)) <+>
        unions (map (select_packet_in swId) (to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        from_list (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        unions (map (select_packet_out sw) ctrlm) <+>
        unions (map (select_packet_in sw) switchm)
    end.

  Definition relate (st : state) : abst_state :=
    unions (map relate_switch (to_list (switches st))) <+>
    unions (map relate_dataLink (links st)) <+>
    unions (map relate_openFlowLink (ofLinks st)) <+>
    relate_controller (ctrl st).

  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast = (relate (devices st)).

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
    outp outp' ctrlm ctrlm' switchm switchm' },
    FlowTablesSafe
      ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws) ->
    FlowTablesSafe 
      ({|Switch swId pts tbl inp' outp' ctrlm' switchm'|} <+> sws).
  Proof with eauto.
    intros.
    unfold FlowTablesSafe in *.
    intros.
    simpl in H0.
    apply Bag.in_union in H0.
    destruct H0.
    + inversion H0.
      inversion H1.
      subst.
      eapply H. simpl. apply Bag.in_union. left. simpl. left...
      simpl in H1; inversion H1.
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
    unfold union in H.
    unfold to_list in H.
    simpl in H.
    destruct sws.
    rename to_list into swsLst.
    unfold union.
    unfold to_list.
    simpl.
    (* might need induction (shouldn't since swId is the same *)
  Admitted.

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

  Section FMS.

    Hint Constructors FMS SafeWire SwitchEP.

    Hint Resolve FMS_untouched.

    Lemma SwitchEP_equiv : forall swId pts tbl inp0 inp1 outp0 outp1
      ctrlm0 switchm0 switchm1 ep,
      SwitchEP (Switch swId pts tbl inp0 outp0 ctrlm0 switchm0) ep ->
      SwitchEP (Switch swId pts tbl inp1 outp1 ctrlm0 switchm1) ep.
    Proof with eauto.
      intros.
      inversion H; subst.
      apply NoFlowModsInBuffer...
      eapply OneFlowModInBuffer...
    Qed.

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
      apply Bag.in_union in H0.
      destruct H0 as [H0 | H0].
      + destruct sw.
        simpl in H0.
        destruct H0 as [H0 | H0].
        2: solve[inversion H0].
        inversion H0.
        subst. 
        clear H0.
        assert (In (Switch swId1 pts1 tbl1 inp0 outp0 ctrlm1 switchm0)
                  (to_list ({|(Switch swId1 pts1 tbl1 inp0 outp0 ctrlm1 switchm0)|} <+>
                   sws))) as J.
        { apply Bag.in_union... }
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
      +  assert (In sw
                   (to_list ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws)))
             as X...
         apply Bag.in_union...
    Qed.

  End FMS.

  Hint Unfold UniqSwIds.
  Hint Resolve step_preserves_P.

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
      ctrlm0 switchm0 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 sws links0 ofLinks0
      ctrl0,
      SwitchesHaveOpenFlowLinks
        (State ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|} <+> sws)
               links0
               ofLinks0
               ctrl0) ->
      SwitchesHaveOpenFlowLinks
        (State ({|Switch swId0 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|} <+> sws)
               links0
               ofLinks0
               ctrl0).
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
        apply H.
        apply Bag.in_union.
        (*
      + assert ( (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)  === (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) \/ Mem (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) sws).
        left. apply reflexivity.
        apply H in H1. clear H.
        destruct H1 as [ofLink [HIn HSwEq]].
        destruct ofLink.
        simpl in *.
        subst.
        inversion H0; subst.
        eexists...
      + destruct (H sw (or_intror H0)) as [ofLink [HIn HSwEq]].
        destruct ofLink.
        simpl in *.
        subst.
        destruct sw.
        simpl in *.
        subst.
        eexists... *)
    Admitted.

    Lemma SwitchesHaveOpenFlowLinks_pres1 : forall sws0 links0 ofLinks0
      ofLinks1 swId switchm0 ctrlm0 switchm1 ctrlm1
      ctrl0,
      SwitchesHaveOpenFlowLinks
        (State sws0 links0
               (ofLinks0 ++ OpenFlowLink swId switchm0 ctrlm0 :: ofLinks1)
               ctrl0) ->
      SwitchesHaveOpenFlowLinks
        (State sws0 links0
               (ofLinks0 ++ OpenFlowLink swId switchm1 ctrlm1 :: ofLinks1)
               ctrl0).
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

  Hint Resolve DevicesFromTopo_pres0 DevicesFromTopo_pres1 SwitchesHaveOpenFlowLinks_pres0
       SwitchesHaveOpenFlowLinks_pres1.

  Lemma simpl_step : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (allFMS1 : AllFMS (switches st1) (ofLinks st1))
    (P0 : P (switches st1) (ofLinks st1) (ctrl st1))
    (uniqOfLinkIds1 : AllDiff of_to (ofLinks st1))
    (ofLinksHaveSw1 : OFLinksHaveSw (switches st1) (ofLinks st1))
    (devsFromTopo1 : DevicesFromTopo st1)
    (swsHaveOFLinks1 : SwitchesHaveOpenFlowLinks st1),
    step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 allFMS2 P1
           uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2,
      concreteStep
        (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 uniqSwIds1
                       allFMS1 P0 uniqOfLinkIds1 ofLinksHaveSw1 devsFromTopo1
                       swsHaveOFLinks1)
        obs
        (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2
                       allFMS2 P1 uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2
                       swsHaveOFLinks2).
  Proof with eauto with datatypes.
    intros.
    unfold concreteStep.
    simpl.
    { inversion H; subst.
      (* Case 1. *)
      + exists (FlowTablesSafe_untouched tblsOk1).
        exists linksTopoOk1.
        exists (LinksHaveSrc_untouched haveSrc1).
        exists (LinksHaveDst_untouched haveDst1).
        exists (UniqSwIds_pres uniqSwIds1).
        exists (AllFMS_untouched1 _ _ _ allFMS1).
        eexists. simpl...
        eexists. simpl...
        exists (OfLinksHaveSrc_pres2 ofLinksHaveSw1).
        eexists...
      (* Case 2: processed a buffered FlowMod *)
      + simpl in *.
        eexists.
        unfold FlowTablesSafe.
        intros.
        apply Bag.in_union in H0. simpl in H0.
        { destruct H0 as [[HIn | HContra] | HIn]; 
          [idtac | solve[inversion HContra] | idtac].
          (* interesting case, where the flow table has been modified *)
          - inversion HIn; subst; clear HIn.
            unfold AllFMS in allFMS1.
            edestruct allFMS1 as [lnk0 [HLnkIn [HEq HFMS]]].
            { apply Bag.in_union. left. simpl. left. reflexivity. }
            destruct lnk0; simpl in *; subst.
            inversion HFMS; subst.
            inversion H2; subst.
            * assert (NotFlowMod (FlowMod fm)) as Hcontra.
              { apply H9. apply Bag.in_union. left. simpl... }
              inversion Hcontra.
            * assert (In (FlowMod fm) (to_list (({|FlowMod f|}) <+> ctrlm2))) 
                as X.
              { rewrite <- H11. apply Bag.in_union. left. simpl... }
              apply Bag.in_union in X. simpl in X.
              { destruct X as [[X | X] | X].
                + inversion X; subst...
                + inversion X.
                + apply H9 in X. inversion X. }
          - unfold FlowTablesSafe in tblsOk1.
            eapply tblsOk1.
            apply Bag.in_union. right. exact HIn. }
        eexists...
        exists (LinksHaveSrc_untouched haveSrc1).
        exists (LinksHaveDst_untouched haveDst1).
        exists (UniqSwIds_pres uniqSwIds1).
        eexists.
        { unfold AllFMS.
          intros.
        apply Bag.in_union in H0; simpl in H0.
        { destruct H0 as [[HIn | HContra] | HIn]; 
          [idtac | solve[inversion HContra] | idtac].
          (* interesting case, where the flow table has been modified *)
          - subst.
            unfold AllFMS in allFMS1.
            edestruct allFMS1 as [lnk0 [HLnkIn [HEq HFMS]]].
            { apply Bag.in_union. left. simpl. left. reflexivity. }
            exists lnk0.
            destruct lnk0; simpl in *; subst.
            repeat split...
            inversion HFMS; subst.
            inversion H2; subst.
            * assert (NotFlowMod (FlowMod fm)) as Hcontra.
              { apply H9. apply Bag.in_union. left. simpl... }
              inversion Hcontra.
            * assert (In (FlowMod fm) (to_list (({|FlowMod f|}) <+> ctrlm2))) 
                as X.
              { rewrite <- H11. apply Bag.in_union. left. simpl... }
              apply Bag.in_union in X. simpl in X.
              { destruct X as [[X | X] | X].
                + inversion X; subst.
                  rewrite <- Bag.pop_union_l in H11.
                  destruct H10 as [ctrlEp Hsafewire].
                  eapply MkFMS. 
                  eapply NoFlowModsInBuffer.
                  intros...
                  apply H9.
                  rewrite <- H11...
                  instantiate (1:= (Endpoint_NoBarrier (modify_flow_table fm tbl0)))...
                  exists ctrlEp...
                + inversion X.
                + apply H9 in X. inversion X. }
          - subst.
            unfold AllFMS in allFMS1.
            edestruct allFMS1 as [lnk0 [HLnkIn [HEq HFMS]]].
            { apply Bag.in_union. right. exact HIn. }
            exists lnk0... } }
        eexists. simpl...
        eexists. simpl...
        exists (OfLinksHaveSrc_pres2 ofLinksHaveSw1).
        eexists...
      (* Case 4. *)
      + exists (FlowTablesSafe_untouched tblsOk1).
        exists linksTopoOk1.
        exists (LinksHaveSrc_untouched haveSrc1).
        exists (LinksHaveDst_untouched haveDst1).
        exists (UniqSwIds_pres uniqSwIds1).
        simpl in *.
        eexists.
        unfold AllFMS.
        intros.
        destruct sw.
        simpl in *.
        { apply Bag.in_union in H1. simpl in H1.
          { destruct H1 as [[H1 | HContra] | H1]; 
            [idtac | solve[inversion HContra] | idtac].
          + inversion H1.
            subst.
            unfold AllFMS in allFMS1.
            edestruct allFMS1 as [lnk0 [HLnkIn [HEq HFMS]]].
            apply Bag.in_union. left. simpl. left. reflexivity.
            simpl in HEq.
            subst.
            exists lnk0.
            split...
            split...
            destruct lnk0.
            simpl.
            simpl in *.
            apply FMS_dequeue_pktOut...
          + unfold AllFMS in allFMS1.
            simpl in allFMS1.
            apply (allFMS1 (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 
                                   switchm1))...
            apply Bag.in_union.
            right... } }
        eexists...
        eexists...
        eexists...
      (* Case 5. *)
      + exists (FlowTablesSafe_untouched tblsOk1).
        exists (LinkTopoOK_inv pks0 (pk::pks0) linksTopoOk1).
        exists (LinksHaveSrc_inv pks0 (pk::pks0) (LinksHaveSrc_untouched haveSrc1)).
        exists (LinksHaveDst_inv pks0 (pk::pks0) (LinksHaveDst_untouched haveDst1)).
        exists (UniqSwIds_pres uniqSwIds1).
        exists (AllFMS_untouched1 _ _ _ allFMS1).
        eexists. eauto.
        eexists. eauto.
        eexists. simpl. eauto.
        eexists...
      (* Case 6. *)
      + exists (FlowTablesSafe_untouched tblsOk1).
        exists (LinkTopoOK_inv (pks0 ++ [pk]) pks0 linksTopoOk1).
        exists 
          (LinksHaveSrc_inv (pks0 ++ [pk]) pks0 (LinksHaveSrc_untouched haveSrc1)).
        exists 
          (LinksHaveDst_inv (pks0 ++ [pk]) pks0 (LinksHaveDst_untouched haveDst1)).
        exists (UniqSwIds_pres uniqSwIds1).
        exists (AllFMS_untouched1 _ _ _ allFMS1).
        eexists...
        exists. simpl...
        exists. simpl...
        eauto.
        eexists...
      (* Case 7. *)
      + exists tblsOk1.
        exists linksTopoOk1.
        exists haveSrc1.
        exists haveDst1.
        exists uniqSwIds1.
        exists allFMS1.
        eexists...
      (* Case 8. *)
      + simpl in *.
        exists tblsOk1.
        exists linksTopoOk1.
        exists haveSrc1.
        exists haveDst1.
        exists uniqSwIds1.
        eexists.
        unfold AllFMS in *. intros. destruct sw. subst.
        assert ({swId0 = swId1 } + { swId0 <> swId1 }) as HIdEq.
        { apply TotalOrder.eqdec. }
        { destruct HIdEq; subst.
          + exists (OpenFlowLink swId1 fromSwitch0 fromCtrl).
            split...
            split...
            destruct (allFMS1 (Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)) as [lnk [HLnkIn [HId HFMS]]].
            { auto. }
            destruct lnk.
            simpl in *.
            subst.
            assert (OpenFlowLink swId1 of_switchm0 of_ctrlm0 =
                    OpenFlowLink swId1 (fromSwitch0 ++ [msg]) fromCtrl) as HOfEq.
            { eapply AllDiff_uniq... }
            inversion HOfEq; subst; clear HOfEq.
            apply FMS_untouched with (inp0 := inp0) (outp0 := outp0) (switchm0 := switchm0)
                                                    (switchmLst0 := fromSwitch0 ++ [msg]).
            exact HFMS.
          + destruct (allFMS1 (Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)) as [lnk [HLnkIn [HId HFMS]]].
            { simpl. exact H1. }
            exists lnk.
            apply in_app_iff in HLnkIn.
            simpl in HLnkIn.
            { destruct HLnkIn as [HLnkIn | [HLnkIn | HLnkIn]].
              + split...
              + destruct lnk. inversion HLnkIn. contradiction n. subst...
              + split... } }
        exists...
        exists. eapply AllDiff_preservation. exact uniqOfLinkIds1.
          do 2 rewrite -> map_app...
        exists (OfLinksHaveSrc_pres3 ofLinksHaveSw1).
        eauto.
      (* Case 9. *)
      + simpl in *.
        exists tblsOk1.
        exists linksTopoOk1.
        exists haveSrc1.
        exists haveDst1.
        exists uniqSwIds1.
        eexists.
        unfold AllFMS.
        simpl in *.
        intros.
        unfold AllFMS in allFMS1.
        destruct (allFMS1 _ H1) as [lnk [HIn [HLnkIdEq HFMS]]].
        apply in_app_iff in HIn; simpl in HIn.
        { destruct HIn as [HIn | [HIn | HIn]].
        * exists lnk; auto with datatypes.
        * exists (OpenFlowLink swId0 fromSwitch0 (msg :: fromCtrl)).
          split...
          split; subst...
          simpl in *.
          destruct sw.
          subst.
          inversion HFMS; subst.
          inversion H4; subst.
          simpl in *.
          - eapply MkFMS...
            destruct H12 as [ctrlEp0 HSafeWire].
            destruct (ControllerFMS fromCtrl _ _ _ P0 H0 H H1 H4) as [ctrlEp1 HSafeWire1]...
          - eapply MkFMS...
            destruct H12 as [ctrlEp0 HSafeWire].
            destruct (ControllerFMS fromCtrl _ _ _ P0 H0 H H1 H4) as [ctrlEp1 HSafeWire1]...
        * exists lnk; auto with datatypes. }
        exists...
        exists. eapply AllDiff_preservation. exact uniqOfLinkIds1.
          simpl. do 2 rewrite -> map_app...
        exists. simpl. eauto.
        eauto.
      (* Case 10. *)
      + simpl in *.
        exists (FlowTablesSafe_untouched tblsOk1).
        exists linksTopoOk1.
        exists (LinksHaveSrc_untouched haveSrc1).
        exists (LinksHaveDst_untouched haveDst1).
        exists (UniqSwIds_pres uniqSwIds1).
        eexists.
        unfold AllFMS in *.
        intros.
        destruct sw.
        subst.
        assert ({swId0 = swId1 } + { swId0 <> swId1 }) as HIdEq.
        { apply TotalOrder.eqdec. }
        { destruct HIdEq; subst.
          + unfold UniqSwIds in uniqSwIds1. (* Not what we want, since it has {|msg|} *)
            assert (AllDiff swId (to_list 
                                    (({|Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+> sws))) as uniqSwIds2.
            { eapply AllDiff_preservation.
              exact uniqSwIds1.
              idtac "TODO(arjun): stupid new problem here.".
              admit. }
            exists (OpenFlowLink swId1 (msg :: fromSwitch0) fromCtrl).
            split...
            split...
            destruct (allFMS1 (Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 ({|msg|} <+> switchm0))) as [lnk J].
            { simpl. apply Bag.in_union. left. simpl. left. trivial. }
            destruct J as [HLnkIn [HId HFMS]].
            simpl in HId.
            destruct lnk.
            subst.
            simpl in *.
            assert (OpenFlowLink of_to0 of_switchm0 of_ctrlm0 =
                    OpenFlowLink of_to0 fromSwitch0 fromCtrl) as HEqOf.
            { eapply AllDiff_uniq... }
            inversion HEqOf; subst; clear HEqOf.
            apply FMS_untouched 
              with (inp0 := inp0) (outp0 := outp0) 
                   (switchm0 := ({|msg|} <+> switchm0))
                   (switchmLst0 := fromSwitch0).
            apply Bag.in_union in H0.
            simpl in H0.
            inversion HFMS; subst.
            { destruct H0 as [[H0 | H0] | H0]; 
              [idtac | solve[inversion H0] | idtac].
              + inversion H0; subst.
                eapply MkFMS...
              + idtac "TODO(arjun): stupid new problem here.".
                admit. } 
          + apply Bag.in_union in H0. simpl in H0.
            destruct H0 as [[H0 | H0] | H0].
            inversion H0; subst.
            contradiction n...
            inversion H0.
            destruct (allFMS1 (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1
                                      switchm1)) as [lnk [J [J0 J1]]]...
            { apply Bag.in_union. right... }
            exists lnk...
            split...
            apply in_app_iff in J. simpl in J.
            destruct J as [J | [J | J]]...
            simpl in J0.
            destruct lnk.
            simpl in J0.
            subst.
            inversion J; subst.
            contradiction n... }
        eexists...
        eexists. eapply AllDiff_preservation. exact uniqOfLinkIds1.
          simpl. do 2 rewrite -> map_app...
        eexists. simpl. eauto.
        eauto.
        eauto.
        eexists...
      (* Case 11. *)
      + simpl in *.
        exists (FlowTablesSafe_untouched tblsOk1).
        exists linksTopoOk1.
        exists (LinksHaveSrc_untouched haveSrc1).
        exists (LinksHaveDst_untouched haveDst1).
        exists (UniqSwIds_pres uniqSwIds1).
        eexists.
        unfold AllFMS in *. intros. destruct sw. subst.
        assert ({swId0 = swId1 } + { swId0 <> swId1 }) as HIdEq.
        { apply TotalOrder.eqdec. }
        { destruct HIdEq; subst.
          + exists (OpenFlowLink swId1 fromSwitch0 fromCtrl).
            split...
            split...
            assert (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm0 switchm1 = Switch swId1 pts0 tbl0 inp0 outp0 ({||}) ({|BarrierReply xid|} <+> switchm0)).
            { unfold UniqSwIds in uniqSwIds1.
              assert (AllDiff swId (to_list ({|Switch swId1 pts0 tbl0 inp0 outp0 ({||}) ({|BarrierReply xid|} <+> switchm0)|} <+> sws))).
              eapply AllDiff_preservation.
              exact uniqSwIds1.
              idtac "TODO(arjun): stupid problem with AllDif and bags".
              admit.
              admit.
              (* simpl...
              eapply AllDiff_uniq.
              exact H1.
              rewrite <- Bag.mem_in_to_list in H0.
              exact H0.
              simpl...
              reflexivity. *) }
            inversion H1. subst. clear H1.
            destruct (allFMS1 (Switch swId1 pts0 tbl0 inp0 outp0 ({||}) switchm0)) as [lnk [HLnkIn [HId HFMS]]].
            { apply Bag.in_union. left. simpl. left. trivial. }
            destruct lnk.
            simpl in *.
            subst.
            assert (OpenFlowLink swId1 of_switchm0 of_ctrlm0 =
                    OpenFlowLink swId1 fromSwitch0 (fromCtrl ++ [BarrierRequest xid])) as HEqOf.
            { eapply AllDiff_uniq... }
            inversion HEqOf; subst; clear HEqOf.
            inversion HFMS; subst.
            destruct H11 as [ctrlEp HSafeWire].
            destruct (SafeWire_dequeue_BarrierRequest _ _ HSafeWire) as [switchEp2 [HSafeWire2 HEpEq]].
            inversion H3; subst.
            * apply MkFMS with (switchEp := switchEp2)...
              apply NoFlowModsInBuffer. intros. simpl in H1. inversion H1.
              auto.
            *  inversion H11.
               destruct ctrlm1.
               rename to_list into lst.
               unfold to_list in H2.
               simpl in H2. 
               rewrite <- OrderedLists.union_singleton_l in H2...
               symmetry in H2.
               apply OrderedLists.insert_nonempty in H2...
               inversion H2.
          + apply Bag.in_union in H0.
            destruct H0.
            simpl in H0.
            destruct H0.
            inversion H0; subst. contradiction n... inversion H0.
            destruct (allFMS1 (Switch swId1 pts1 tbl1 inp1 outp1 
                                      ctrlm0 switchm1)) 
              as [lnk [HLnkIn [HId HFMS]]].
            { apply Bag.in_union. right... }
            exists lnk.
            apply in_app_iff in HLnkIn.
            simpl in HLnkIn.
            { destruct HLnkIn as [HLnkIn | [HLnkIn | HLnkIn]].
              + split...
              + destruct lnk. inversion HLnkIn. contradiction n. subst...
              + split... } }
        exists...
        exists. eapply AllDiff_preservation. exact uniqOfLinkIds1.
          simpl. do 2 rewrite -> map_app...
        exists. simpl. eauto.
        eauto.
        eexists...
      (* Case 12. *)
      + simpl in *.
        exists (FlowTablesSafe_untouched tblsOk1).
        exists linksTopoOk1.
        exists (LinksHaveSrc_untouched haveSrc1).
        exists (LinksHaveDst_untouched haveDst1).
        exists (UniqSwIds_pres uniqSwIds1).
        eexists.
        unfold AllFMS in *. intros. destruct sw. subst.
        assert ({swId0 = swId1 } + { swId0 <> swId1 }) as HIdEq.
        { apply TotalOrder.eqdec. }
        { destruct HIdEq; subst.
          + exists (OpenFlowLink swId1 fromSwitch0 fromCtrl).
            split...
            split...
            assert (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 = Switch swId1 pts0 tbl0 inp0 outp0 ({|msg|}<+>ctrlm0) switchm0).
            { unfold UniqSwIds in uniqSwIds1.
              assert (AllDiff swId (to_list ({|Switch swId1 pts0 tbl0 inp0 outp0 ({|msg|}<+>ctrlm0) switchm0|} <+> sws))).
              eapply AllDiff_preservation.
              exact uniqSwIds1.
              idtac "TODO(arjun): stupid problem with AllDif and bags".
              admit.
              admit.
(*
              simpl...
              eapply AllDiff_uniq.
              exact H2.
              rewrite <- Bag.mem_in_to_list in H1.
              exact H1.
              simpl...
              reflexivity. *) }
            inversion H2. subst. clear H2.
            destruct (allFMS1 (Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)) as [lnk [HLnkIn [HId HFMS]]].
            { apply Bag.in_union. left. simpl... }
            destruct lnk.
            simpl in *.
            subst.
            assert (OpenFlowLink swId1 of_switchm0 of_ctrlm0 =
                    OpenFlowLink swId1 fromSwitch0 (fromCtrl ++ [msg])) as HEqOf.
            { eapply AllDiff_uniq... }
            inversion HEqOf; subst; clear HEqOf.
            apply FMS_pop...
          + apply Bag.in_union in H1.
            destruct H1.
            simpl in H1. destruct H1. inversion H1; subst; contradiction n...
            inversion H1.
            destruct (allFMS1 (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 
                                      switchm1)) as [lnk [HLnkIn [HId HFMS]]].
            { apply Bag.in_union. right... }
            exists lnk.
            apply in_app_iff in HLnkIn.
            simpl in HLnkIn.
            { destruct HLnkIn as [HLnkIn | [HLnkIn | HLnkIn]].
              + split...
              + destruct lnk. inversion HLnkIn. contradiction n. subst...
              + split... } }
        eexists...
        eexists. eapply AllDiff_preservation. exact uniqOfLinkIds1.
          simpl. do 2 rewrite -> map_app...
        eexists. simpl. eauto.
        eauto.
        eexists...
    } 
    (* epic fail of a proof script *)
    Grab Existential Variables. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto.
    eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto. eauto.
  Qed.

  Lemma simpl_multistep' : forall (st1 st2 : state) obs
    (tblsOk1 : FlowTablesSafe (switches st1))
    (linksTopoOk1 : ConsistentDataLinks (links st1))
    (haveSrc1 : LinksHaveSrc (switches st1) (links st1))
    (haveDst1 : LinksHaveDst (switches st1) (links st1))
    (uniqSwIds1 : UniqSwIds (switches st1))
    (allFMS1 : AllFMS (switches st1) (ofLinks st1))
    (P1 : P (switches st1) (ofLinks st1) (ctrl st1))
    (uniqOfLinkIds1 : AllDiff of_to (ofLinks st1))
    (ofLinksHaveSw1 : OFLinksHaveSw (switches st1) (ofLinks st1))
    (devsFromTopo1 : DevicesFromTopo st1)
    (swsHaveOFLinks1 : SwitchesHaveOpenFlowLinks st1),
    multistep step st1 obs st2 ->
    exists tblsOk2 linksTopoOk2 haveSrc2 haveDst2 uniqSwIds2 allFMS2 P2
           uniqOfLinkIds2 ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2,
      multistep concreteStep
                (ConcreteState st1 tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                               uniqSwIds1 allFMS1 P1 uniqOfLinkIds1
                               ofLinksHaveSw1 devsFromTopo1 swsHaveOFLinks1)
                obs
                (ConcreteState st2 tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                               uniqSwIds2 allFMS2 P2 uniqOfLinkIds2
                               ofLinksHaveSw2 devsFromTopo2 swsHaveOFLinks2).
  Proof with eauto with datatypes.
    intros.
    induction H.
    (* zero steps. *)
    + solve [ eauto 12 ].
    (* tau step *)
    + destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                           uniqSwIds1 allFMS1 P1 uniqOfLinkIds1 ofLinksHaveSw1
                           devsFromTopo1 swsHaveOFLinks1 H)
        as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 
                [allFMS2 [P2 [uniqOfLinkIds2 [ofLinksHaveSw2 
           [devsFromTopo2 [swsHaveOFLinks2 step]]]]]]]]]]].
      destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                            uniqSwIds2 allFMS2 P2 uniqOfLinkIds2 ofLinksHaveSw2
                            devsFromTopo2 swsHaveOFLinks2)
        as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3
                [uniqSwIds3 [allFMS3 [PN [uniqOfLinkIdsN [ofLinksHaveSwN 
           [devsFromTopoN [swsHaveOFLinksN stepN]]]]]]]]]]].
      solve [ eauto 13 ].
    (* The script below is an identical copy of the case above. *)
    + destruct (simpl_step tblsOk1 linksTopoOk1 haveSrc1 haveDst1 
                           uniqSwIds1 allFMS1 P1 uniqOfLinkIds1 ofLinksHaveSw1
                           devsFromTopo1 swsHaveOFLinks1 H)
        as [tblsOk2 [linksTopoOk2 [haveSrc2 [haveDst2 [uniqSwIds2 
                [allFMS2 [P2 [uniqOfLinkIds2 [ofLinksHaveSw2 
           [devsFromTopo2 [swsHaveOFLinks2 step]]]]]]]]]]].
      destruct (IHmultistep tblsOk2 linksTopoOk2 haveSrc2 haveDst2 
                            uniqSwIds2 allFMS2 P2 uniqOfLinkIds2 ofLinksHaveSw2
                            devsFromTopo2 swsHaveOFLinks2)
        as [tblsOk3 [linksTopoOk3 [haveSrc3 [haveDst3
                [uniqSwIds3 [allFMS3 [PN [uniqOfLinkIdsN [ofLinksHaveSwN 
           [devsFromTopoN [swsHaveOFLinksN stepN]]]]]]]]]]].
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
      uniqSwIds0 allFMS0 ctrlP0 uniqOfLinkIds0 ofLinksHaveSw0 devicesFromTopo0
      swsHaveOFLinks0 H) as [v0 [v1 [v2 [v3 [v4 [v5 [v6 [v7 [v8 [v9 [v10 Hstep]]]]]]]]]]].
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
    inversion H1. subst. clear H1.
    inversion H6. subst. clear H6.

    assert (FlowTableSafe sw tbl0) as Z.
      unfold FlowTablesSafe in concreteState_flowTableSafety0.
      eapply concreteState_flowTableSafety0...
      apply Bag.in_union.
      left.
      simpl...
    unfold FlowTableSafe in Z.
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
