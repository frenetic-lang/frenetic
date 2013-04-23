Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.
Require Import Bag.TotalOrder.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module Make (Import Relation : RELATION).

  Import RelationDefinitions.
  Import AtomsAndController.
  Import Machine.
  Import Atoms.

  Lemma DrainWire : forall sws (swId : switchId) pts tbl inp outp 
    ctrlm switchm links src pks0 pks swId pt links0 ofLinks ctrl,
     multistep step
      (State 
        ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws)
        (links ++ (DataLink src (pks0 ++ pks) (swId,pt)) :: links0)
        ofLinks ctrl)
      nil
      (State 
        ({|Switch swId pts tbl (from_list (map (fun pk => (pt,pk)) pks) <+> inp) outp ctrlm switchm|} <+> sws)
        (links ++ (DataLink src pks0 (swId,pt)) :: links0)
        ofLinks ctrl).
   Proof with auto.
     intros.
     generalize dependent inp0. 
     generalize dependent pks1.
     induction pks1 using rev_ind.
     + intros.
       simpl in *.
       rewrite -> app_nil_r.
       rewrite -> Bag.from_list_nil_is_empty.
       rewrite -> Bag.union_empty_l.
       apply multistep_nil.
     (* inductive case *)
     + intros. 
       rewrite -> (app_assoc pks0 pks1).
       rewrite -> map_app.
       eapply multistep_tau.
       apply RecvDataLink.
       eapply multistep_app with
       (s2 := State ({|Switch swId1 pts0 tbl0 (from_list (map (fun pk => (pt,pk)) pks1) <+> (({|(pt,x)|}) <+> inp0)) outp0 ctrlm0 switchm0|} <+> sws)
                    (links0 ++ DataLink src0 pks0 (swId1,pt) :: links1)
                    ofLinks0
                    ctrl0).
       apply (IHpks1 ( ({| (pt, x) |}) <+> inp0)).
       autorewrite with bag using simpl.
       apply multistep_nil.
       simpl...
   Qed.

  Lemma ObserveFromOutp : forall pktOuts pktIns pk pt0 pt1
    swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
    swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1
    sws pks links0 links1 ofLinks0 ctrl0,
    (pktOuts, pktIns) = process_packet tbl1 pt1 pk ->
    Some (swId1,pt1) = topo (swId0,pt0) ->
    multistep step
      (State 
        (({|Switch swId0 pts0 tbl0 inp0 ({|(pt0,pk)|} <+> outp0) 
                  ctrlm0 switchm0|}) <+>
         ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) pks (swId1,pt1)) :: links1)
        ofLinks0 ctrl0)
      [(swId1,pt1,pk)]
      (State 
        (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
         ({|Switch swId1 pts1 tbl1
                  (from_list (map (fun pk => (pt1,pk)) pks) <+> inp1) 
                  (from_list pktOuts <+> outp1)
                  ctrlm1
                  (from_list (map (PacketIn pt1) pktIns) <+> switchm1)|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) nil (swId1,pt1)) :: links1)
        ofLinks0 ctrl0).
  Proof with simpl;eauto with datatypes.
    intros.
    eapply multistep_tau.
    apply SendDataLink.
    rewrite <- Bag.union_assoc.
    rewrite -> (Bag.union_comm _ ({|Switch swId0 pts0 tbl0 inp0 outp0 
                                           ctrlm0 switchm0|})).
    rewrite -> Bag.union_assoc.
    eapply multistep_app with (obs2 := [(swId1,pt1,pk)]).
    apply (DrainWire 
      (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+> sws)
      swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 
      links0 (swId0,pt0) [pk] pks0 swId1 pt1 links1 ofLinks0 ctrl0).
    assert ([pk] = nil ++ [pk]) as X... rewrite -> X. clear X.
    eapply multistep_tau.
    apply RecvDataLink.
    eapply multistep_obs.
    apply PktProcess.
    instantiate (1 := pktIns).
    instantiate (1 := pktOuts).
    symmetry...
    match goal with
    | [ |- multistep step _ nil ?Y ] => remember Y as S2
    end.
    subst.
    assert (forall (b1 b2 b3 : bag switch_le), b1 <+> b2 <+> b3 = b2 <+> b1 <+> b3).
    { intros.  bag_perm 100. }
    rewrite -> H1.
    apply multistep_nil.
    trivial.
  Qed.

  Lemma ObserveFromOutp_same_switch : forall pktOuts pktIns pk pt0 pt1
    swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
    sws pks links0 links1 ofLinks0 ctrl0,
    (pktOuts, pktIns) = process_packet tbl0 pt1 pk ->
    Some (swId0,pt1) = topo (swId0,pt0) ->
    multistep step
      (State 
        (({|Switch swId0 pts0 tbl0 inp0 ({|(pt0,pk)|} <+> outp0) 
                  ctrlm0 switchm0|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) pks (swId0,pt1)) :: links1)
        ofLinks0 ctrl0)
      [(swId0,pt1,pk)]
      (State 
        (({|Switch swId0 pts0 tbl0 
                  (from_list (map (fun pk => (pt1,pk)) pks) <+> inp0)
                  (from_list pktOuts <+> outp0)
                  ctrlm0
                  (from_list (map (PacketIn pt1) pktIns) <+> switchm0)|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) nil (swId0,pt1)) :: links1)
        ofLinks0 ctrl0).
  Proof with simpl;eauto with datatypes.
    intros.
    eapply multistep_tau.
    apply SendDataLink.
    eapply multistep_app with (obs2 := [(swId0,pt1,pk)]).
    apply (DrainWire 
             sws
             swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 
             links0 (swId0,pt0) [pk] pks0 swId0 pt1 links1 ofLinks0 ctrl0).
    rewrite <- (app_nil_l [pk]).
    eapply multistep_tau.
    apply RecvDataLink.
    eapply multistep_obs.
    eapply PktProcess...
    apply multistep_nil.
    trivial.
  Qed.

  Lemma DrainFromControllerBag : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0
    switchm0  sws0 links0 ofLinks0 ctrl0,
    (forall x, In x (to_list ctrlm0) -> NotBarrierRequest x) ->
    exists tbl1 outp1,
      multistep step
        (State 
           (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
            sws0)
           links0 ofLinks0 ctrl0)
        nil
        (State 
           (({|Switch swId0 pts0 tbl1 inp0 outp1 ({||}) switchm0|}) <+>
            sws0)
           links0 ofLinks0 ctrl0).
  Proof with simpl;eauto with datatypes.
    intros swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 sws0 links0 ofLinks0
           ctrl0 HNotBarrier.
    destruct ctrlm0.
    rename to_list into ctrlm0.
    generalize dependent tbl0.
    generalize dependent outp0.
    (* Note that by doing inductio on the unique list that represents this set,
       we are fixing an order. Not that it matters. *)
    induction ctrlm0; intros.
    + exists tbl0.
      exists outp0.
      subst...
      assert (Bag nil order = empty). apply Bag.ordered_irr. simpl...
      rewrite -> H.
      apply multistep_nil.
    (* Inductive case *)
    + destruct a.
      - inversion order; subst.
        assert (forall x, In x (to_list (Bag ctrlm0 H2)) -> NotBarrierRequest x) as Y.
        { intros. apply HNotBarrier. simpl. right... }
        destruct (IHctrlm0 H2 Y ({|(p, p0)|} <+> outp0) tbl0)
          as [tbl1 [outp1 Hstep]].
        exists tbl1. exists outp1.
        assert (Bag (PacketOut p p0 :: ctrlm0) order = from_list  (PacketOut p p0 :: ctrlm0)).
        { apply Bag.ordered_irr... unfold to_list. 
          simpl. rewrite -> OrderedLists.insert_eq_head...
          f_equal. symmetry. apply OrderedLists.from_list_id...
          apply OrderedLists.from_list_order. intros. apply H1.
          rewrite -> OrderedLists.in_from_list_iff... }
        rewrite -> Bag.from_list_cons in H.
        eapply multistep_tau.
        rewrite -> H.
        apply SendPacketOut.
        assert (Bag ctrlm0 H2 = from_list ctrlm0). 
        { apply Bag.ordered_irr...
          unfold to_list.
          symmetry.
          apply OrderedLists.from_list_id... }
        rewrite -> H0 in *.
        apply Hstep.
      - assert (NotBarrierRequest (BarrierRequest n)) as contra.
        { apply HNotBarrier... }
        inversion contra.
      - inversion order; subst.
        assert (forall x, In x (to_list (Bag ctrlm0 H2)) -> NotBarrierRequest x) as Y.
        { intros. apply HNotBarrier. simpl. right... }
        destruct (IHctrlm0 H2 Y outp0 (modify_flow_table f tbl0)) as [tbl1 [outp1 Hstep]].
        exists tbl1.
        exists outp1.
        assert (Bag (FlowMod f :: ctrlm0) order = (({|FlowMod f|}) <+> (Bag ctrlm0 H2))).
        { apply Bag.ordered_irr. 
          unfold to_list. simpl. 
          symmetry. apply OrderedLists.union_cons... }
        rewrite -> H.
        eapply multistep_tau.
        apply ModifyFlowTable.
        apply Hstep.
  Qed.
    
  (* In ObserveFromController, flow table and flow mod safety are irrelevant,
     since [PacketOut] messages are not processed by flow tables. *)
  Lemma DrainFromController : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
    sws0 links0 ofLinks0 lstSwitchm0 lstCtrlm0 lstCtrlm1 ofLinks1 ctrl0,
    (forall x, In x (to_list ctrlm0) -> NotBarrierRequest x) ->                                
    exists tbl0' ctrlm0' outp0' switchm1,
      multistep step
        (State 
           (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
            sws0)
           links0
           (ofLinks0 ++ 
            (OpenFlowLink swId0 lstSwitchm0 (lstCtrlm0 ++ lstCtrlm1)) ::
            ofLinks1)
           ctrl0)
        nil
        (State 
           (({|Switch swId0 pts0 tbl0' inp0 outp0' ctrlm0' switchm1|}) <+>
            sws0)
           links0
           (ofLinks0 ++ (OpenFlowLink swId0 lstSwitchm0 lstCtrlm0) ::
            ofLinks1)
           ctrl0).
  Proof with simpl;eauto with datatypes.
    intros  swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 sws0 links0 ofLinks0 
            lstSwitchm0 lstCtrlm0 lstCtrlm1 ofLinks1 ctrl0 HNotBarrier.
    generalize dependent tbl0.
    generalize dependent ctrlm0.
    generalize dependent outp0.
    generalize dependent switchm0.
    induction lstCtrlm1 using rev_ind; intros.
    exists tbl0.
    exists ctrlm0.
    exists outp0.
    exists switchm0.
    rewrite -> app_nil_r.
    apply multistep_nil.
    (* Inductive case *)
    destruct x.
    (* PacketOut *)
    +  assert (forall x, In x (to_list ({|PacketOut p p0|} <+> ctrlm0)) -> NotBarrierRequest x) as Y.
       { intros.
         apply Bag.in_union in H; simpl in H.
         destruct H...
         destruct H. subst; apply PacketOut_NotBarrierRequest. inversion H. }
       destruct (IHlstCtrlm1 switchm0 outp0 (({|PacketOut p p0|}) <+> ctrlm0) Y tbl0)
         as [tbl1 [ctrlm1 [outp1 [switchm1 Hstep]]]].
       exists tbl1.
       exists ctrlm1.
       exists outp1.
       exists switchm1.
       eapply multistep_tau.
       rewrite -> app_assoc.
       apply RecvFromController.
       apply PacketOut_NotBarrierRequest.
       exact Hstep.
    (* BarrierRequest *)
    + destruct
        (DrainFromControllerBag swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 sws0
                                  links0
           (ofLinks0 ++ 
            (OpenFlowLink swId0 lstSwitchm0
                          (lstCtrlm0 ++ lstCtrlm1 ++ [BarrierRequest n])) ::
            ofLinks1)
           ctrl0)
      as [tbl1 [outp1 Hdrain]]...
      assert (forall x, In x (to_list (@empty fromController fromController_le)) -> NotBarrierRequest x) as Y.
      { intros. simpl in H. inversion H. }
      destruct (IHlstCtrlm1 (({|BarrierReply n|}) <+> switchm0) outp1 empty Y tbl1)
        as [tbl2 [ctrlm2 [outp2 [switchm2 Hstep2]]]].
      exists tbl2.
      exists ctrlm2.
      exists outp2.
      exists switchm2.
      eapply multistep_app.
      apply Hdrain.
      eapply multistep_tau.
      rewrite -> app_assoc.
      apply RecvBarrier.
      apply Hstep2.
      trivial.
    (* FlowMod case *)
   +  assert (forall x, In x (to_list ({|FlowMod f|} <+> ctrlm0)) -> NotBarrierRequest x) as Y.
       { intros.
         apply Bag.in_union in H; simpl in H.
         destruct H...
         destruct H. subst; apply FlowMod_NotBarrierRequest. inversion H. }
      destruct (IHlstCtrlm1 switchm0 outp0  (({|FlowMod f|}) <+> ctrlm0) Y tbl0)
        as [tbl1 [ctrlm1 [outp1 [switchm1 Hstep]]]].
       exists tbl1.
       exists ctrlm1.
       exists outp1.
       exists switchm1.
       eapply multistep_tau.
       rewrite -> app_assoc.
       apply RecvFromController.
       apply FlowMod_NotBarrierRequest.
       exact Hstep.
  Qed.
      
  (* In ObserveFromController, flow table and flow mod safety are irrelevant,
     since [PacketOut] messages are not processed by flow tables. *)
  Lemma ObserveFromController : forall 
    (srcSw dstSw : switchId)
    dstPt pk pktOuts pktIns srcPt
    pts0 tbl0 inp0 outp0 ctrlm0 switchm0
    pts1 tbl1 inp1 outp1 ctrlm1 switchm1
    sws0 links0 pks0 links1
    ofLinks0 lstSwitchm0 lstCtrlm0 lstCtrlm1 ofLinks1
    ctrl0,
    (pktOuts, pktIns) = process_packet tbl1 dstPt pk ->
    Some (dstSw,dstPt) = topo (srcSw, srcPt) ->
    (forall x, In x (to_list ctrlm0) -> NotBarrierRequest x) ->                                
    exists tbl0' switchm0' ctrlm0' outp0',
      multistep step
        (State 
           (({|Switch srcSw pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
            ({|Switch dstSw pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
            sws0)
           (links0 ++ (DataLink (srcSw,srcPt) pks0 (dstSw,dstPt)) :: links1)
           (ofLinks0 ++ 
            (OpenFlowLink srcSw lstSwitchm0 
                          (lstCtrlm0 ++ (PacketOut srcPt pk)  :: lstCtrlm1)) ::
            ofLinks1)
           ctrl0)
        [(dstSw,dstPt,pk)]
        (State 
           (({|Switch srcSw pts0 tbl0' inp0 outp0' 
                      ctrlm0' switchm0'|}) <+>
            ({|Switch dstSw pts1 tbl1 
                      (from_list (map (fun pk => (dstPt,pk)) pks0) <+> inp1)
                      (from_list pktOuts <+> outp1)
                      ctrlm1
                      (from_list (map (PacketIn dstPt) pktIns) <+> 
                       switchm1)|}) <+>
            sws0)
           (links0 ++ (DataLink (srcSw,srcPt) nil (dstSw,dstPt)) :: links1)
           (ofLinks0 ++ 
            (OpenFlowLink srcSw lstSwitchm0 lstCtrlm0) ::
            ofLinks1)
           ctrl0).
  Proof with simpl;eauto with datatypes.
    intros.
    destruct (DrainFromController srcSw pts0 tbl0 inp0 outp0 ctrlm0 switchm0
                (({|Switch dstSw pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
                 sws0)
                (links0 ++ (DataLink (srcSw,srcPt) pks0 (dstSw,dstPt)) :: links1)
                ofLinks0
                lstSwitchm0
                (lstCtrlm0 ++ [PacketOut srcPt pk])
                lstCtrlm1
                ofLinks1
                ctrl0) as [tbl01 [ctrlm01 [outp01 [switchm01 Hdrain]]]]...
    exists tbl01.
    exists switchm01.
    exists ctrlm01.
    exists outp01.
    eapply multistep_app.
    assert (lstCtrlm0 ++ PacketOut srcPt pk :: lstCtrlm1 = 
            (lstCtrlm0 ++ [PacketOut srcPt pk]) ++ lstCtrlm1) as X.
      rewrite <- app_assoc...
     rewrite -> X. clear X.
    exact Hdrain.
    clear Hdrain.
    eapply multistep_tau.
    apply RecvFromController.
    apply PacketOut_NotBarrierRequest.
    eapply multistep_tau.
    apply SendPacketOut.
    eapply ObserveFromOutp...
    trivial.
  Qed.
  
  Lemma EasyObservePacketOut : forall sw pt srcSw p switches0 links0 ofLinks01
    of_switchm0 lstCtrlm0 pk lstCtrlm1 ofLinks02 ctrl0
    (linksHaveSrc0 : LinksHaveSrc switches0 links0)
    (linksHaveDst0 : LinksHaveDst switches0 links0)
    (devicesFromTopo0 : DevicesFromTopo (State switches0 links0
               (ofLinks01 ++ 
                (OpenFlowLink srcSw of_switchm0 
                              (lstCtrlm0 ++ PacketOut p pk :: lstCtrlm1)) ::
                ofLinks02)
               ctrl0))
    (Htopo : Some (sw,pt) = topo (srcSw,p)),
    NoBarriersInCtrlm switches0 ->
    exists state1,
      multistep
        step
        (State switches0 links0
               (ofLinks01 ++ 
                (OpenFlowLink srcSw of_switchm0 
                              (lstCtrlm0 ++ PacketOut p pk :: lstCtrlm1)) ::
                ofLinks02)
               ctrl0)
        [(sw,pt,pk)]
        state1.
  Proof with simpl;eauto with datatypes.
    intros.
    rename H into HNoBarriers.
    assert (exists pks, In (DataLink (srcSw,p) pks (sw,pt)) links0) as X.
    { 
      unfold DevicesFromTopo in devicesFromTopo0.
      apply devicesFromTopo0 in Htopo.
      destruct Htopo as [sw0 [sw1 [lnk [_ [_ [Hlnk [_ [_ [HIdEq0 HIdEq1]]]]]]]]].
      simpl in Hlnk.
      destruct lnk.
      subst.
      simpl in *.
      rewrite -> HIdEq0 in Hlnk.
      rewrite -> HIdEq1 in Hlnk.
      exists pks0... }
    destruct X as [pks Hlink].  apply in_split in Hlink.
    destruct Hlink as [links01 [links02 Hlink]]. subst.

    assert (LinkHasSrc switches0 (DataLink (srcSw,p) pks (sw,pt))) as X.
      apply linksHaveSrc0...
    unfold LinkHasSrc in X.
    simpl in X.
    destruct X as [switch0 [HMemSw0 [HSwId0Eq HPtsIn0]]].

    assert (LinkHasDst switches0 (DataLink (srcSw,p) pks (sw,pt))) as X.
      apply linksHaveDst0...
    unfold LinkHasDst in X.
    simpl in X.
    destruct X as [switch1 [HMemSw1 [HSwId1Eq HPtsIn1]]].

    destruct switch0.
    destruct switch1.
    subst.
    simpl in *.

    remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
    destruct X as [pktOuts pktIns].
    apply Bag.in_split with (Order := TotalOrder_switch) in HMemSw0.
    destruct HMemSw0 as [sws0 HMemSw0].
    subst.
    apply Bag.in_split with (Order := TotalOrder_switch) in HMemSw1.
    destruct HMemSw1 as [sws1 HMemSw0].
    subst.
    
    destruct (TotalOrder.eqdec swId0 swId1) as [HEq | HNeq].
    + subst.
      destruct (DrainFromController swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 sws0
                                    (links01 ++ DataLink (swId1,p) pks (swId1,pt) :: links02)
                                    ofLinks01 of_switchm0 (lstCtrlm0 ++ [PacketOut p pk]) 
                                    lstCtrlm1 ofLinks02 ctrl0) as
          [tbl2 [ctrlm2 [outp2 [switchm2 Hstep1]]]].
      { unfold NoBarriersInCtrlm in HNoBarriers.
        remember (Switch swId1 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) as sw.
        assert (ctrlm0 = ctrlm sw) as HEq. 
        { subst... }
        rewrite -> HEq.
        refine (HNoBarriers _ _).
        apply Bag.in_union; simpl... }
      rewrite <- app_assoc in Hstep1.
      simpl in Hstep1.
      remember (process_packet tbl2 pt pk) as J.
      destruct J.
      symmetry in HeqJ.
      eexists.
      eapply multistep_app.
      exact Hstep1.
      eapply multistep_tau.
      apply RecvFromController.
      { apply PacketOut_NotBarrierRequest. }
      eapply multistep_tau.
      apply SendPacketOut.
      eapply multistep_tau.
      apply SendDataLink.
      eapply multistep_app.
      { assert (pk :: pks = [pk] ++ pks) as J. auto.
        rewrite -> J.
        apply DrainWire... }
      eapply multistep_tau.
      { assert ([pk] = nil ++ [pk]) as J. 
        { auto. }
        rewrite -> J.
        apply RecvDataLink. }
      eapply multistep_obs.
      eapply PktProcess...
      eapply multistep_nil.
      reflexivity.
      reflexivity.
    + assert (In (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) (to_list sws1)) as J.
      { assert (In (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) 
                   (to_list (({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+> sws1))).
        { rewrite <- HMemSw0. apply Bag.in_union; simpl... }
        apply Bag.in_union in H. simpl in H.
        destruct H as [[H|H]|H]...
        + inversion H. subst. contradiction HNeq...
        + inversion H. }
      apply Bag.in_split with (Order:=TotalOrder_switch) in J.
      destruct J as [otherSws Heq].
      rewrite -> Heq in HMemSw0.
      assert (In (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1) (to_list sws0)) as J.
      { assert (In (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1) 
                   (to_list (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+> sws0))).
        { rewrite -> HMemSw0. apply Bag.in_union; simpl... }
        apply Bag.in_union in H. simpl in H.
        destruct H as [[H|H]|H]...
        + inversion H. subst. contradiction HNeq...
        + inversion H. }
      apply Bag.in_split with (Order:=TotalOrder_switch) in J.
      destruct J as [otherSws0 Heq0].
      rewrite -> Heq0 in HMemSw0.
      move HMemSw0 after Heq0.
      do 2 rewrite <- Bag.union_assoc in HMemSw0.
      rewrite <- (Bag.union_comm _ ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|})) in HMemSw0.
      do 2 rewrite -> Bag.union_assoc in HMemSw0.
      apply Bag.pop_union_l in HMemSw0.
      apply Bag.pop_union_l in HMemSw0.
      subst.

    destruct (@ObserveFromController swId0 swId1 pt pk  pktOuts pktIns p
             pts0 tbl0 inp0 outp0 ctrlm0 switchm0
             pts1 tbl1 inp1 outp1 ctrlm1 switchm1
             otherSws
             links01 pks links02
             ofLinks01 of_switchm0 lstCtrlm0 lstCtrlm1 ofLinks02
             ctrl0 Hprocess Htopo) as
        [ tbl2 [switchm2 [ctrlm2 [outp2 Hstep]]]].
    { intros.
      unfold NoBarriersInCtrlm in HNoBarriers.
      eapply HNoBarriers.
      apply Bag.in_union. left. simpl. left. reflexivity.
      simpl... }
    eexists. exact Hstep.
  Qed.

  Lemma DrainToController : forall sws0 links0 ofLinks00 swId0 switchm0
    switchm1 ctrlm0 ofLinks01 ctrl0,
    exists sws1 links1 ofLinks10 ofLinks11 ctrl1,
      multistep step
        (State 
           sws0
           links0
           (ofLinks00 ++ 
            (OpenFlowLink swId0 (switchm0 ++ switchm1) ctrlm0) ::
            ofLinks01)
           ctrl0)
        nil
        (State
           sws1
           links1
           (ofLinks10 ++ 
            (OpenFlowLink swId0 switchm0 ctrlm0) ::
            ofLinks11)
           ctrl1).
  Proof with simpl;eauto with datatypes.
    intros.
    generalize dependent ctrl0.
    induction switchm1 using rev_ind; intros.
    exists sws0. exists links0. exists ofLinks00. exists ofLinks01.
    exists ctrl0. rewrite -> app_nil_r. apply multistep_nil.
    destruct (ControllerRecvLiveness sws0 links0 ofLinks00 swId0 
                                     (switchm0 ++ switchm1) x
                                     ctrlm0 ofLinks01 ctrl0)
             as [ctrl1 [Hstep1 _]].
    destruct (IHswitchm1 ctrl1) as
        [sws1 [links1 [ofLinks10 [ofLinks11 [ctrl2 Hstep2]]]]].
    exists sws1. exists links1. exists ofLinks10. exists ofLinks11.
    exists ctrl2. 
    eapply multistep_app.
      rewrite -> app_assoc.
      apply Hstep1.
      apply Hstep2.
    trivial.
  Qed.


  Instance PtPk_TotalOrder : TotalOrder (PairOrdering portId_le packet_le).
  Proof. apply TotalOrder_pair; eauto. exact TotalOrder_portId. exact TotalOrder_packet. Defined.

  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with simpl;eauto with datatypes.
    unfold weak_simulation.
    intros.
    unfold inverse_relation in H.
    unfold bisim_relation in H.
    unfold relate in H.
    inversion H0; subst.
    simpl.
    remember (devices t) as devices0.
    destruct devices0.
    simpl in *.
    (* The first challenge is to figure out what the cases are. We cannot
       proceed by inversion or induction. Instead, hypothesis H entails
       that (sw,pt,pk) is in one of the concrete components. Mem defines
       how this may be. *)
    assert (In (sw,pt,pk) (to_list (({|(sw,pt,pk)|}) <+> lps))) as J.
    { apply Bag.in_union... }
    rewrite -> H1 in J.
    repeat rewrite -> Bag.in_union in J.
    destruct J as [HMemSwitch | [ HMemLink | [HMemOFLink | HMemCtrl ]]].
    (* The packet in on a switch. *)
    apply Bag.in_unions in HMemSwitch.
    destruct HMemSwitch as [switch_abst [XIn XMem]].
    apply in_map_iff in XIn.
    destruct XIn as [switch [XRel XIn]].
    subst.
    destruct switch.
    simpl in XMem.
    repeat rewrite -> Bag.in_union in XMem.
    destruct XMem as [HMemInp | [HMemOutp | [HMemCtrlm | HMemSwitchm]]].

    (* At this point, we've discriminated all but one other major case.
       Packets on OpenFlow links may be on either side. *)

    (* ********************************************************************** *)
    (* Case 1 : Packet is in an input buffer                                  *)
    (* ********************************************************************** *)
    { apply Bag.in_to_from_list in HMemInp.
      apply in_map_iff in HMemInp.
      destruct HMemInp as [[pt0 pk0] [Haffix HMemInp]].
      simpl in Haffix. inversion Haffix. subst. clear Haffix.
      apply Bag.in_split with (Order := PtPk_TotalOrder) in HMemInp.
      destruct HMemInp as [inp HEqInps].
      subst.
      apply Bag.in_split with (Order := TotalOrder_switch) in XIn.
      destruct XIn as [sws0 XIn].
      remember (process_packet tbl0 pt pk) as ToCtrl eqn:Hprocess. 
      destruct ToCtrl as (outp',inPkts).
      eapply simpl_weak_sim.
      rewrite <- Heqdevices0.
      rewrite -> XIn.
      apply multistep_obs with (a0 := State (({|Switch sw pts0 tbl0 inp (from_list outp' <+> outp0) ctrlm0
                                            (from_list (map (PacketIn pt) inPkts) <+> switchm0)  |}) <+> sws0) 
                                            links0 ofLinks0 ctrl0).
      apply PktProcess...
      eapply multistep_nil.
      rewrite <- Heqdevices0.
      unfold relate.
      simpl...
      rewrite -> H1... }

    (* ********************************************************************** *)
    (* Case 2 : Packet is in an output buffer                                 *)
    (* ********************************************************************** *)

    (* Proof sketch: We can assume that (topo sw pt) is defined, which implies
       that a data-link exists from this switch to some destination, dst.
       We reach the final state in two steps: SendDataLink followed by
       PktProcess.*)

    (* Train-wreck proof script: type classes are beyond my comprehension ATM. If you move
       this to the top, everything breaks. :( *)
  Instance SwPtPk_TotalOrder : TotalOrder (PairOrdering (PairOrdering switchId_le portId_le) packet_le).
  Proof. 
    apply TotalOrder_pair.
    apply TotalOrder_pair.
    exact TotalOrder_switchId. 
    exact TotalOrder_portId.
    exact TotalOrder_packet. 
  Defined.
 
    (* 1. Rewrite outp0 as a union of (pt,pk) and some other bag.  *)
    { apply Bag.in_unions in HMemOutp.
      destruct HMemOutp as [lps0 [HIn HMemOutp]].
      apply in_map_iff in HIn.
      destruct HIn as [[srcPt srcPk] [HTransfer HIn]].
      rename swId0 into srcSw.
      simpl in HTransfer.
      remember (topo (srcSw,srcPt)) as Htopo.
      destruct Htopo.
      + destruct p as [dstSw dstPt].
        subst.
        simpl in HMemOutp.
        destruct HMemOutp. 2: inversion H.
        symmetry in H. inversion H. subst. clear H.
        rename srcPk into pk.
        assert (exists pks, In (DataLink (srcSw,srcPt) pks (dstSw,dstPt)) links0) as X.
        {
          destruct t.
          unfold DevicesFromTopo in devicesFromTopo0.
          apply devicesFromTopo0 in HeqHtopo.
          destruct HeqHtopo as [sw0 [sw1 [lnk [_ [_ [Hlnk [_ [_ [HIdEq0 HIdEq1]]]]]]]]].
          simpl in Hlnk.
          destruct lnk.
          subst.
          simpl in *.
          rewrite -> HIdEq0 in Hlnk.
          rewrite -> HIdEq1 in Hlnk.
          exists pks0...
          rewrite <- Heqdevices0 in Hlnk.
          simpl in Hlnk... }
        destruct X as [pks Hlink].
        (* 2. Rewrite links0 as the union of the link in Hlink and the rest. *)
        apply in_split in Hlink.
        destruct Hlink as [links01 [links02 Hlink]].
        (* 3. Establish that the destination switch exists. *)
        assert 
          (LinkHasDst
             switches0
             (DataLink (srcSw,srcPt) pks (dstSw,dstPt))) as J0.
        { destruct t.
          simpl in Heqdevices0.
          rewrite <- Heqdevices0 in *.
          subst.
          apply linksHaveDst0... }
        unfold LinkHasDst in J0.
        destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
        destruct switch2.
        simpl in *.
        symmetry in HSw2IdEq.
        subst.
        apply Bag.in_split with (Order := TotalOrder_switch) in XIn.
        destruct XIn as [sws XXX].
        subst.
        apply Bag.in_union with (Order:=TotalOrder_switch) in HSw2In.
        destruct HSw2In.
        - simpl in H.
          destruct H; inversion H.
          subst; clear H.
          apply Bag.in_split with (Order:=PtPk_TotalOrder) in HIn.
          destruct HIn as [outp0' HEq0].
          subst.
          remember (process_packet tbl1 dstPt pk) as X eqn:Hprocess.
          destruct X as [outp1' pktIns].
          eapply simpl_weak_sim.
          rewrite <- Heqdevices0...
          eapply ObserveFromOutp_same_switch...
          rewrite <- Heqdevices0...
          apply AbstractStep.
        - { apply Bag.in_split with (Order := TotalOrder_switch) in H.
            destruct H as [sws0 XXX].
            subst.
            apply Bag.in_split with (Order:=PtPk_TotalOrder) in HIn.
            destruct HIn as [outp0' HEq0].
            subst.
            rename outp0' into outp0.
            remember (process_packet tbl1 dstPt pk) as X eqn:Hprocess.
            destruct X as [outp1' pktIns].
            eapply simpl_weak_sim.
            rewrite <- Heqdevices0.
            eapply ObserveFromOutp...
            rewrite <- Heqdevices0.
            unfold relate.
            rewrite -> H1.
            autorewrite with bag using simpl.
            reflexivity.
            apply AbstractStep. }
      + subst. 
        unfold to_list in HMemOutp.
        simpl in HMemOutp.
        inversion HMemOutp. }
    
    (* ********************************************************************** *)
    (* Case 3 : Packet is in a PacketOut message                              *)
    (* ********************************************************************** *)
    { apply Bag.in_unions in HMemCtrlm.
      destruct HMemCtrlm as [ctrlmBag [HIn HMemCtrlm]].
      apply in_map_iff in HIn.
      destruct HIn as [ctrlm [Htopo HInMsg]].
      subst.
      destruct ctrlm. 
      2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* not a barrier *)
      2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* not a flowmod *)
      simpl in HMemCtrlm.
      remember (topo (swId0,p)) as Htopo.
      destruct Htopo.
      (* packet does not go poof *)
      2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ].
      destruct p1.
      simpl in HMemCtrlm.
      destruct HMemCtrlm. 2: solve [inversion H].
      inversion H. subst. clear H.
      apply Bag.in_split with (Order:=TotalOrder_fromController) in HInMsg.
      destruct HInMsg as [ctrlm0' XX].
      subst. rename ctrlm0' into ctrlm0.
      assert (exists pks, In (DataLink (swId0,p) pks (sw,pt)) links0) as X.
      { 
        destruct t.
        unfold DevicesFromTopo in devicesFromTopo0.
        apply devicesFromTopo0 in HeqHtopo.
        destruct HeqHtopo as [sw0 [sw1 [lnk [_ [_ [Hlnk [_ [_ [HIdEq0 HIdEq1]]]]]]]]].
        simpl in Hlnk.
        destruct lnk.
        subst.
        simpl in *.
        rewrite -> HIdEq0 in Hlnk.
        rewrite -> HIdEq1 in Hlnk.
        rewrite <- Heqdevices0 in *.
        simpl in *.
        exists pks0... }
      destruct X as [pks Hlink].
      apply in_split in Hlink.
      destruct Hlink as [links01 [links02 Hlink]].
      subst.
      assert 
        (LinkHasDst
           switches0
           (DataLink (swId0,p) pks (sw,pt))) as J0.
      { destruct t.
        simpl in *.
        rewrite <- Heqdevices0 in *.
        apply linksHaveDst0... }
      unfold LinkHasDst in J0.
      destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
      destruct switch2.
      simpl in *.
      subst.
      apply Bag.in_split with (Order:=TotalOrder_switch) in XIn.
      destruct XIn as [sws0 HEq1].
      subst.
      apply Bag.in_union in HSw2In.
      destruct HSw2In.
      + simpl in H.
        destruct H; inversion H; subst; clear H.
        remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
        destruct X as [outp1' pktIns].
        eapply simpl_weak_sim.
        rewrite <- Heqdevices0.
        eapply multistep_tau.
        apply SendPacketOut.
        eapply ObserveFromOutp_same_switch...
        rewrite <- Heqdevices0...
        apply AbstractStep.
      + apply Bag.in_split with (Order:=TotalOrder_switch) in H.
        destruct H as [sws XXX].
        subst.
        remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
        destruct X as [outp1' pktIns].
        eapply simpl_weak_sim.
        rewrite <- Heqdevices0.
        eapply multistep_tau.
        apply SendPacketOut.
        eapply ObserveFromOutp...
        rewrite <- Heqdevices0...
        apply AbstractStep... }

    (* ********************************************************************** *)
    (* Case 4 : Packet is in a PacketIn message                               *)
    (* ********************************************************************** *)

    { apply Bag.in_unions in HMemSwitchm.
      destruct HMemSwitchm as [lps0 [HIn HMem]].
      apply in_map_iff in HIn.
      destruct HIn as [switchm [HEq HIn]].
      subst.
      destruct switchm.
      2: simpl in HMem; inversion HMem. (* not a barrier *)
      simpl in HMem.
      apply Bag.in_unions_map in HMem.
      destruct HMem as [[srcPt srcPk] [HInAbst HInTransfer]].
      simpl in HInTransfer.
      remember (topo (swId0, srcPt)) as Htopo.
      destruct Htopo.
      + destruct p1.
        simpl in HInTransfer.
        destruct HInTransfer.
        2: inversion H.
        inversion H; subst; clear H.
        assert (exists switchm0l ctrlm0l, 
                  In (OpenFlowLink swId0 switchm0l ctrlm0l) ofLinks0) as X.
        { destruct t.
          unfold SwitchesHaveOpenFlowLinks in swsHaveOFLinks0.
          simpl in swsHaveOFLinks0.
          simpl in Heqdevices0.
          assert (switches0 = switches devices0). 
          { rewrite <- Heqdevices0... }
          rewrite <- H in swsHaveOFLinks0.
          apply swsHaveOFLinks0 in XIn.
          destruct XIn as [ofLink [HOFLinkIn HIdEq]].
          clear H.
          destruct ofLink.
          simpl in HIdEq...
          subst... }
        destruct X as [switchm0l [ctrlm0l HOfLink]].
        apply in_split in HOfLink.
        destruct HOfLink as [ofLinks00 [ofLinks01 HOFLink]].
        subst.
        apply Bag.in_split with (Order:=TotalOrder_switch) in XIn.
        destruct XIn as [sws XX].
        apply Bag.in_split with (Order:=TotalOrder_fromSwitch) in HIn.
        destruct HIn as [switchm0' XX'].
        subst.
        rename switchm0' into switchm0.

        destruct (DrainToController
                    (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+> sws)
                    links0 ofLinks00 swId0 [PacketIn p p0] switchm0l ctrlm0l
                    ofLinks01 ctrl0)
          as [sws1 [links1 [ofLinks10 [ofLinks11 [ctrl1 Hstep2]]]]].
        
        destruct (ControllerRecvLiveness sws1 links1 ofLinks10 swId0 nil
                                         (PacketIn p p0)
                                         ctrlm0l ofLinks11 ctrl1)
          as [ctrl2 [Hstep3 [lps' HInCtrl]]].
        simpl in HInCtrl.
        apply in_split in HInAbst.
        destruct HInAbst as [pre [post HInAbst]].
        rewrite -> HInAbst in HInCtrl.
        rewrite -> map_app in HInCtrl.
        rewrite -> Bag.unions_app in HInCtrl.
        simpl in HInCtrl.
        rewrite -> Bag.unions_cons in HInCtrl.
        rewrite <- HeqHtopo in HInCtrl.
        assert (In (sw,pt,pk) (to_list (relate_controller ctrl2))) as HIn.
        { rewrite <- HInCtrl.
          apply Bag.in_union.
          left.
          apply Bag.in_union.
          right.
          apply Bag.in_union.
          left.
          simpl... }
        destruct (ControllerLiveness
                    sw pt pk ctrl2 sws1 links1
                    (ofLinks10 ++ (OpenFlowLink swId0 nil ctrlm0l) :: ofLinks11)
                    HIn)
          as [ofLinks20 [ofLinks21 [ctrl3 [swTo [ptTo [switchmLst [ctrlmLst
             [Hstep4 HPktEq]]]]]]]].
        simpl in HPktEq.
        remember (topo (swTo,ptTo)) as X.
        destruct X.
        destruct p1.
        inversion HPktEq. subst. clear HPktEq.
        assert (multistep step (devices t) nil (State sws1 links1 (ofLinks20 ++ (OpenFlowLink swTo switchmLst (PacketOut ptTo pk :: ctrlmLst)) :: ofLinks21) ctrl3)).
        {  eapply multistep_tau.
          rewrite <- Heqdevices0.
          eapply SendToController.
          eapply multistep_app.
          simpl in Hstep2.
          eapply Hstep2.
          eapply multistep_app.
          eapply Hstep3.
          eapply Hstep4.
          instantiate (1 := nil)...
          reflexivity. }
        remember H as H' eqn:X; clear X.
        apply simpl_multistep in H'.
        destruct H' as [st1 [Hdevices1 HPreStep]].
        destruct (@EasyObservePacketOut sw pt swTo ptTo sws1 links1 ofLinks20
                                        switchmLst nil pk ctrlmLst
                                        ofLinks21 ctrl3) as [stateN stepN]...
        { destruct st1. simpl in *. subst. simpl in *... }
        { destruct st1. simpl in *. subst. simpl in *... }
        { destruct st1. simpl in *. subst. simpl in *... }
        { destruct st1. simpl in *. subst. simpl in *... }
        eapply simpl_weak_sim.
        eapply multistep_tau.
        rewrite <- Heqdevices0.
        eapply SendToController.
        eapply multistep_app.
        simpl in Hstep2.
        eapply Hstep2.
        eapply multistep_app.
        eapply Hstep3.
        eapply multistep_app.
        eapply Hstep4.
        exact stepN.
        instantiate (1 := [(sw,pt,pk)])...
        instantiate (1 := [(sw,pt,pk)])...
        reflexivity.
        rewrite <- Heqdevices0.
        rewrite -> H1.
        unfold relate.
        autorewrite with bag using simpl.
        reflexivity.
        apply AbstractStep... 
        inversion HPktEq. 
      + simpl in HInTransfer.
        inversion HInTransfer. }
  
    (* ********************************************************************** *)
    (* Case 5 : Packet is on a data link                                      *)
    (* ********************************************************************** *)
    { apply Bag.in_unions_map in HMemLink.
      destruct HMemLink as [link [HIn HMemLink]].
      destruct link.
      simpl in HMemLink.
      destruct dst0 as [sw0 pt0].
      apply Bag.in_to_from_list in HMemLink.
      rewrite -> in_map_iff in HMemLink.
      destruct HMemLink as [pk0 [HEq HMemLink]].
      inversion HEq. subst. clear HEq.
      apply in_split in HMemLink.
      destruct HMemLink as [pks01 [pks02 HMemLink]].
      subst.

      assert
        (LinkHasDst 
           switches0 (DataLink src0 (pks01 ++ pk :: pks02) (sw,pt))) as J0.
      { destruct t.
        simpl in *.
        rewrite <- Heqdevices0 in *.
        apply linksHaveDst0... }
      unfold LinkHasDst in J0.
      destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
      destruct switch2.
      simpl in *.
      subst.
      apply Bag.in_split with (Order:=TotalOrder_switch) in HSw2In.
      destruct HSw2In as [sws HSw2In].
      remember (process_packet tbl0 pt pk) as X eqn:Hprocess.
      destruct X as [pktOuts pktIns].
      apply in_split in HIn.
      destruct HIn as [links01 [links02 HIn]].
      subst.

      eapply simpl_weak_sim.
      rewrite <- Heqdevices0.
      eapply multistep_app with (obs2 := [(swId0,pt,pk)]).
      assert ((pks01 ++ [pk]) ++ pks02 = pks01 ++ pk :: pks02) as X.
      { rewrite <- app_assoc... }
      rewrite <- X.
      apply (DrainWire sws swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
                       links01 src0 (pks01 ++ [pk]) pks02 swId0 pt 
                       links02 ofLinks0 ctrl0).
      eapply multistep_tau.
      eapply RecvDataLink.
      eapply multistep_obs.
      eapply PktProcess...
      eapply multistep_nil.
      reflexivity.
      rewrite <- Heqdevices0.
      rewrite -> H1.
      reflexivity.
      apply AbstractStep. }

    (* ********************************************************************** *)
    (* Cases 6 and 7 : Packet is on an OpenFlow link                          *)
    (* ********************************************************************** *)
    apply Bag.in_unions_map in HMemOFLink.
    destruct HMemOFLink as [link0 [HIn HMem]].
    destruct link0.
    simpl in HMem.
    apply Bag.in_union in HMem.
    destruct HMem as [HMemCtrlm | HMemSwitchm].

    (* ************************************************************************)
    (* Case 6 : Packet is in a PacketOut message from the controller          *)
    (* ************************************************************************)
    { apply in_split in HIn.
      destruct HIn as [ofLinks01 [ofLinks02 HIn]]. subst.
      apply Bag.in_unions_map in HMemCtrlm. 
      destruct HMemCtrlm as [msg [MemCtrlm HPk]].
      destruct msg.
      2: solve [ simpl in HPk; inversion HPk ]. (* not a barrier *)
      2: solve [ simpl in HPk; inversion HPk ]. (* not a flowmod *)
      simpl in HPk.
      remember (topo (of_to0,p)) as Htopo.
      rename of_to0 into srcSw.
      destruct Htopo.
      (* packet does not go poof *)
      2: solve [ simpl in HPk; inversion HPk ].
      destruct p1.
      simpl in HPk.
      destruct HPk.
      2: solve[inversion H].
      inversion H. subst. clear H.

      apply in_split in MemCtrlm.
      destruct MemCtrlm as [lstCtrlm0 [lstCtrlm1 HMemCtrlm]].
      subst.
      destruct (@EasyObservePacketOut sw pt srcSw p switches0 links0 ofLinks01
                                      of_switchm0 lstCtrlm0 pk lstCtrlm1
                                      ofLinks02 ctrl0) as [stateN stepN]...
    { destruct t. simpl in *. rewrite <- Heqdevices0 in *. auto. }
    { destruct t. simpl in *. rewrite <- Heqdevices0 in *. auto. }
    { destruct t. simpl in *. rewrite <- Heqdevices0 in *. auto. }
    { destruct t. simpl in *. rewrite <- Heqdevices0 in *. auto. }
    apply simpl_weak_sim with (devs2 := stateN)...
    rewrite <- Heqdevices0...
    rewrite -> H1.
    unfold relate.
    simpl.
    rewrite <- Heqdevices0...
    apply AbstractStep. }

    (* ************************************************************************)
    (* Case 7 : Packet is in a PacketIn message from a switch                 *)
    (* ************************************************************************)

    { apply in_split in HIn.
      destruct HIn as [ofLinks00 [ofLinks01 HIn]]. subst.
      apply Bag.in_unions_map in HMemSwitchm.
      destruct HMemSwitchm as [msg [HmsgIn HPk]].
      apply in_split in HmsgIn.
      destruct HmsgIn as [switchm0 [switchm1 HmsgIn]]. subst.
      destruct msg.
      2: solve [ simpl in HPk; inversion HPk ]. (* not a barrier *)
      destruct (DrainToController switches0 links0 ofLinks00 of_to0 
                                  (switchm0 ++ [PacketIn p p0]) switchm1 of_ctrlm0
                                  ofLinks01 ctrl0)
      as [sws1 [links1 [ofLinks10 [ofLinks11 [ctrl1 Hstep1]]]]].
      match goal with
        | [ H : multistep step ?s1 nil ?s2 |- _ ] =>
        remember s1 as S1; remember s2 as S2
    end.
    assert (switchm0 ++ PacketIn p p0 :: switchm1 =
            (switchm0 ++ [PacketIn p p0]) ++ switchm1) as X.
    rewrite <- app_assoc...
    rewrite -> X in *. clear X.
    rewrite <- HeqS1 in Heqdevices0.
    assert (multistep step (devices t) nil S2) as X...
    { rewrite <- Heqdevices0... }
    apply simpl_multistep in X.
    destruct X as [st2 [Heqdevices2 HconcreteStep1]].
    destruct (ControllerRecvLiveness sws1 links1 ofLinks10 of_to0 switchm0
                                     (PacketIn p p0)
                                     of_ctrlm0 ofLinks11 ctrl1)
             as [ctrl2 [Hstep2 [lps' HInCtrl]]].
    simpl in HInCtrl.
    simpl in HPk.
    assert (In (sw,pt,pk) (to_list (relate_controller ctrl2))) as HMem2.
    { rewrite <- HInCtrl.
      rewrite -> Bag.in_union. left.
      simpl.
      exact HPk. }
    destruct (ControllerLiveness
             sw pt pk ctrl2 sws1 links1
             (ofLinks10 ++ (OpenFlowLink of_to0 switchm0 of_ctrlm0) :: ofLinks11)
             HMem2)
      as [ofLinks20 [ofLinks21 [ctrl3 [swTo [ptTo [switchmLst [ctrlmLst
          [Hstep3 HPktEq]]]]]]]].
    simpl in HPktEq.
    remember (topo (swTo, ptTo)) as X eqn:Htopo.
    destruct X.
    destruct p1. inversion HPktEq. subst. clear HPktEq.
    destruct (@EasyObservePacketOut sw pt swTo ptTo sws1 links1 ofLinks20
                                   switchmLst nil pk ctrlmLst
                                   ofLinks21 ctrl3) as [stateN stepN]...
    { destruct st2. simpl in *. subst. simpl in *. auto. }
    { destruct st2. simpl in *. subst. simpl in *. auto. }
    { destruct st2. simpl in *. subst. simpl in *. auto. }
    { destruct st2. simpl in *. subst. simpl in *. auto. }
    apply simpl_weak_sim with (devs2 := stateN).
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep1. clear Hstep1.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep2. clear Hstep2.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep3.
    apply stepN.
    reflexivity.
    reflexivity.
    reflexivity.
    rewrite -> H1.
    unfold relate.
    rewrite -> HeqS1.
    autorewrite with bag using simpl.
    reflexivity.
    apply AbstractStep.
    simpl in HPktEq.
    inversion HPktEq. }

    (* ************************************************************************)
    (* Case 8 : Packet is at the controller                                   *)
    (* ************************************************************************)
    { simpl in *.
      destruct 
        (ControllerLiveness sw pt pk ctrl0 switches0 links0 ofLinks0 HMemCtrl)
        as [ofLinks10 [ofLinks11 [ctrl1 [swTo [ptTo [switchmLst
              [ctrlmLst [Hstep Hrel]]]]]]]].
      simpl in Hrel.
      remember (topo (swTo,ptTo)) as X eqn:Htopo.
      destruct X.
      destruct p.
      inversion Hrel. subst. clear Hrel.
      rename swTo into srcSw. rename ptTo into p.
      destruct (@EasyObservePacketOut sw pt srcSw p switches0 links0 ofLinks10
                                      switchmLst nil pk ctrlmLst
                                      ofLinks11 ctrl1) as [stateN stepN]...
        { destruct t. simpl in *. subst. simpl in *. auto. }
        { destruct t. simpl in *. subst. simpl in *. auto. }
        { destruct t. simpl in *. subst. simpl in *. auto. }
        { destruct t. simpl in *. subst. simpl in *. auto. }
        apply simpl_weak_sim with (devs2 := stateN)...
        rewrite <- Heqdevices0.
        eapply multistep_app...
        rewrite -> H1.
        rewrite <- Heqdevices0.
        unfold relate.
        simpl.
        autorewrite with bag using simpl.
        trivial.
        apply AbstractStep.
        inversion Hrel. }
Qed.

End Make.
