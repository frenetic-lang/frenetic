Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require Import Bag.Bag.
Require Import FwOF.FwOFSignatures.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (Import Relation : RELATION).

  Lemma DrainWire : forall sws (swId : switchId) pts tbl inp outp 
    ctrlm switchm links src pks0 pks swId pt links0 ofLinks ctrl,
     multistep step
      (State 
        ({|Switch swId pts tbl inp outp ctrlm switchm|} <+> sws)
        (links ++ (DataLink src (pks0 ++ pks) (swId,pt)) :: links0)
        ofLinks ctrl)
      nil
      (State 
        ({|Switch swId pts tbl (FromList (map (fun pk => (pt,pk)) pks) <+> inp) outp ctrlm switchm|} <+> sws)
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
       apply multistep_tau with 
       (a0 := State ({|Switch swId1 pts0 tbl0 (FromList nil <+> inp0) outp0 ctrlm0 switchm0|} <+> sws)
                    (links0 ++ DataLink src0 pks0 (swId1,pt) :: links1)
                    ofLinks0
                    ctrl0).
       apply StepEquivState.
       apply StateEquiv.
       apply Bag.pop_union_r.
       apply Bag.equiv_singleton.
       apply SwitchEquiv; try solve [apply reflexivity].
       rewrite -> Bag.FromList_nil_is_Empty.
       rewrite -> Bag.union_empty_l.
       apply reflexivity.
       apply multistep_nil.
     (* inductive case *)
     + intros. 
       eapply multistep_tau.
       rewrite -> (app_assoc pks0 pks1).
       apply RecvDataLink.
       rewrite -> map_app.
       eapply multistep_app with
       (s2 := State ({|Switch swId1 pts0 tbl0 (FromList (map (fun pk => (pt,pk)) pks1) <+> (({|(pt,x)|}) <+> inp0)) outp0 ctrlm0 switchm0|} <+> sws)
                    (links0 ++ DataLink src0 pks0 (swId1,pt) :: links1)
                    ofLinks0
                    ctrl0).
       apply (IHpks1 ( ({| (pt, x) |}) <+> inp0)).
       apply multistep_tau with
        (a0 := State ({|Switch swId1 pts0 tbl0 (FromList (map (fun pk => (pt,pk)) pks1 ++ map (fun pk => (pt,pk)) [x]) <+> inp0) outp0 ctrlm0 switchm0|} <+> sws)
                    (links0 ++ DataLink src0 pks0 (swId1,pt) :: links1)
                    ofLinks0
                    ctrl0).
       apply StepEquivState.
       apply StateEquiv.
       apply Bag.pop_union_r.
       apply Bag.equiv_singleton.
       apply SwitchEquiv; try solve [apply reflexivity].
       simpl.
       rewrite -> Bag.FromList_app.
       rewrite -> Bag.from_list_singleton.
       rewrite -> Bag.union_assoc.
       apply reflexivity.
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
                  (FromList (map (fun pk => (pt1,pk)) pks) <+> inp1) 
                  (FromList pktOuts <+> outp1)
                  ctrlm1
                  (FromList (map (PacketIn pt1) pktIns) <+> switchm1)|}) <+>
         sws)
        (links0 ++ (DataLink (swId0,pt0) nil (swId1,pt1)) :: links1)
        ofLinks0 ctrl0).
  Proof with simpl;eauto with datatypes.
    intros.

    eapply multistep_tau.
    apply SendDataLink.
    apply multistep_tau with
    (a0 := State
             (({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
              ({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+>
              sws)
             (links0 ++ (DataLink (swId0,pt0) ([pk]++pks0) (swId1,pt1))::links1)
             ofLinks0
             ctrl0).
      apply StepEquivState.
      apply StateEquiv.
      rewrite <- Bag.union_assoc.
      rewrite -> (Bag.union_comm _ ({|Switch swId0 pts0 tbl0 inp0 outp0 
                                             ctrlm0 switchm0|})).
      rewrite -> Bag.union_assoc.
      apply Bag.pop_union; apply reflexivity.

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
    eapply multistep_tau with (a0 := S2).
    subst.
    apply StepEquivState.
    apply StateEquiv.
    bag_perm 100.
    apply multistep_nil.
    trivial.
  Qed.

  (** Remark is for another lemma, not ObserveFromController!

     This lemma relies on the following property: we can pick a
      packet located anywhere (e.g., a packet at the controller or in
      a PacketIn/PacketOut message) and construct a trace that
      observes *only* that packet. This is not obvious, because
      barriers may force other messages to be processed first.
      However, barriers cannot force a switch to emit a packet.

      Consider [PacketOut pt pk] sent to switch [sw1], followed by a 
      [BarrierRequest n]. Even if the controller waits for [BarrierReply n],
      that does not force pk to be transfered to its destination.

      Now, consider a controller that waits for a [PacketIn pk] at the
      destination, thereby forcing a different observation. We have to
      rule out such controllers with our liveness property. *)


  Definition lifted_modify_flow_table (msg : fromController) (ft : flowTable) :=
    match msg with
      | FlowMod mod => modify_flow_table mod ft
      |  _ => ft
    end.


  Lemma DrainFromControllerBag : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0
    switchm0  sws0 links0 ofLinks0 ctrl0,
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
    intros.
    remember (Bag.to_list ctrlm0) as lst.
    generalize dependent ctrlm0.
    generalize dependent tbl0.
    generalize dependent outp0.
    induction lst; intros.
    + exists tbl0.
      exists outp0.
      apply multistep_tau with
        (a0 := State ({|Switch swId0 pts0 tbl0 inp0 outp0 Empty switchm0|} <+> sws0)
                     links0
                     ofLinks0
                     ctrl0).
      apply StepEquivState.
      apply StateEquiv.
      apply Bag.pop_union_r.
      apply Bag.equiv_singleton.
      apply SwitchEquiv; try solve [apply reflexivity].
      apply Bag.to_list_nil...
      apply multistep_nil.
    (* Inductive case *)
    + assert (ctrlm0 === ({|a|}) <+> (FromList lst)) as X.
      { apply Bag.to_list_equiv. rewrite <- Heqlst. simpl... }
    destruct a.
    (* PacketOut case *)
    destruct (IHlst ({|(p,p0)|} <+> outp0) tbl0 (FromList lst))
      as [tbl1 [outp1 Hstep]].
    trivial.
    exists tbl1. exists outp1.
    eapply multistep_tau with
      (a0 := (State 
           (({|Switch swId0 pts0 tbl0 inp0 outp0
                      (({|PacketOut p p0|}) <+> FromList lst) switchm0|}) <+>
            sws0)
           links0 ofLinks0 ctrl0)).
      apply StepEquivState.
      apply StateEquiv.
      apply Bag.pop_union_r.
      apply Bag.equiv_singleton.
      apply SwitchEquiv; try solve [apply reflexivity | auto].
    eapply multistep_tau.
      apply SendPacketOut.
      idtac "TODO(arjun): p in pts0 in ctrlm link". admit.
      exact Hstep.
    (* BarrierRequest case, on the switch buffer, which cannot happen *)
    idtac "TODO(arjun): must disallow barriers in the ctrlm on switch!".
    (* TODO(arjun): could add a bogus transition ... *)
    admit.
    (* FlowMod case *)
    destruct (IHlst outp0 (modify_flow_table f tbl0) (FromList lst))
      as [tbl1 [outp1 Hstep]].
    trivial.
    exists tbl1. exists outp1.
    eapply multistep_tau with
      (a0 := (State 
           (({|Switch swId0 pts0 tbl0 inp0 outp0
                      (({|FlowMod f|}) <+> FromList lst) switchm0|}) <+>
            sws0)
           links0 ofLinks0 ctrl0)).
      apply StepEquivState.
      apply StateEquiv.
      apply Bag.pop_union_r.
      apply Bag.equiv_singleton.
      apply SwitchEquiv; try solve [apply reflexivity | auto].
    eapply multistep_tau.
      apply ModifyFlowTable.
      exact Hstep.
  Qed.
    
  (* In ObserveFromController, flow table and flow mod safety are irrelevant,
     since [PacketOut] messages are not processed by flow tables. *)
  Lemma DrainFromController : forall swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
    sws0 links0 ofLinks0 lstSwitchm0 lstCtrlm0 lstCtrlm1 ofLinks1 ctrl0,
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
    intros.
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
    destruct (IHlstCtrlm1 switchm0 outp0 (({|PacketOut p p0|}) <+> ctrlm0) tbl0)
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
    destruct
      (DrainFromControllerBag swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 sws0
                                  links0
           (ofLinks0 ++ 
            (OpenFlowLink swId0 lstSwitchm0
                          (lstCtrlm0 ++ lstCtrlm1 ++ [BarrierRequest n])) ::
            ofLinks1)
           ctrl0)
      as [tbl1 [outp1 Hdrain]].
    destruct (IHlstCtrlm1 (({|BarrierReply n|}) <+> switchm0) outp1 Empty tbl1)
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
    destruct (IHlstCtrlm1 switchm0 outp0  (({|FlowMod f|}) <+> ctrlm0) tbl0)
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
                      (FromList (map (fun pk => (dstPt,pk)) pks0) <+> inp1)
                      (FromList pktOuts <+> outp1)
                      ctrlm1
                      (FromList (map (PacketIn dstPt) pktIns) <+> 
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
                ctrl0) as [tbl01 [ctrlm01 [outp01 [switchm01 Hdrain]]]].
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
    admit. (* TODO(arjun): pt in pts *)
    eapply ObserveFromOutp...
    trivial.
  Qed.
   
  Hint Resolve switch_equiv_is_Equivalence.

  Instance Equivalence_eq `(A : Type) : Equivalence (@eq A).
  Proof with auto.
    split.
    unfold Reflexive...
    unfold Symmetric...
    unfold Transitive. intros. subst...
  Qed.
 
  Instance ptPkt_eqdec : EqDec (portId * packet) eq := 
    prod_eqdec EqDec_portId EqDec_packet.

  Instance swPtPk_eqdec : EqDec (switchId * portId * packet) eq :=
    prod_eqdec (prod_eqdec EqDec_switchId EqDec_portId) EqDec_packet.

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

    (* Goal is to rewrite switches0 as switch0 <+> switch1 <+> ?? *)
    apply Bag.mem_split with (ED := switch_eqdec) in HMemSw0.
    destruct HMemSw0 as [sws0 HMemSw0].
    eapply Bag.mem_equiv with (ED := switch_eqdec) in HMemSw1.
    2: exact HMemSw0.
    destruct HMemSw1 as [switch1 [HMemSw1 HSw1Eq]].
    apply Bag.mem_union in HMemSw1.
    destruct HMemSw1.
      idtac "TODO(arjun): skipping src=dst switches case.". admit.
    apply Bag.mem_split with (ED := switch_eqdec) in H.
    destruct H as [sws1 HSw2Eq].
    rewrite -> HSw2Eq in HMemSw0.
    apply symmetry in HSw1Eq.
    rewrite -> (Bag.equiv_singleton _ _ _ HSw1Eq) in HMemSw0.
    clear sws0 switch1 HSw2Eq HSw1Eq.

    destruct (@ObserveFromController swId0 swId1 pt pk  pktOuts pktIns p
             pts0 tbl0 inp0 outp0 ctrlm0 switchm0
             pts1 tbl1 inp1 outp1 ctrlm1 switchm1
             sws1 links01 pks links02
             ofLinks01 of_switchm0 lstCtrlm0 lstCtrlm1 ofLinks02
             ctrl0 Hprocess Htopo) as
        [ tbl2 [switchm2 [ctrlm2 [outp2 Hstep]]]].
    exists
      (State (({|Switch swId0 pts0 tbl2 inp0 outp2 ctrlm2 switchm2|}) <+> 
                ({|Switch swId1 pts1 tbl1 
                          (FromList (map (fun pk : packet => (pt, pk)) pks) <+>
                                  inp1)
                          (FromList pktOuts <+> outp1)
                          ctrlm1
                          (FromList (map (PacketIn pt) pktIns)<+>switchm1)|}) <+>
                          sws1)
               (links01 ++ (DataLink (swId0,p) nil (swId1,pt)) :: links02)
               (ofLinks01 ++ (OpenFlowLink swId0 of_switchm0 lstCtrlm0) ::
                ofLinks02)
               ctrl0).
    apply multistep_tau with
      (a0 :=
         State (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0|}) <+> 
                ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
                sws1)
               (links01 ++ (DataLink (swId0,p) pks (swId1,pt)) :: links02)
               (ofLinks01 ++
                (OpenFlowLink swId0 of_switchm0
                              (lstCtrlm0 ++ PacketOut p pk :: lstCtrlm1)) ::
                ofLinks02)
               ctrl0).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> HMemSw0.
    apply reflexivity.
    apply Hstep.
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
    

  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with simpl;eauto with datatypes.
    unfold weak_simulation.
    intros.
    unfold inverse_relation in H.
    unfold bisim_relation in H.
    unfold relate in H.
    destruct t.
    simpl in *.
    inversion H0; subst.
    (* Idiotic case, where the two abstract states are equivalent. *)
    admit.
    (* Real cases here. *)
    simpl.
    destruct devices0.
    (* The first challenge is to figure out what the cases are. We cannot
       proceed by inversion or induction. Instead, hypothesis H entails
       that (sw,pt,pk) is in one of the concrete components. Mem defines
       how this may be. *)
    assert (Mem (sw,pt,pk) (({|(sw,pt,pk)|}) <+> lps)) as J.
      apply Bag.mem_union. left. simpl. apply reflexivity.
    (* By J, (sw,pt,pk) is in the left-hand side of H. By Mem_equiv,
       (sw,pt,pk) is also on the right-hand side too. *)
    destruct (Bag.Mem_equiv (sw,pt,pk) H J) as 
      [HMemSwitch | [ HMemLink | [HMemOFLink | HMemCtrl ]]].
    (* The packet in on a switch. *)
    apply Bag.Mem_unions in HMemSwitch.
    destruct HMemSwitch as [switch_abst [ Xin Xmem ]].
    simpl in Xin.
    simpl in H.
    apply Bag.in_map_mem  with 
      (E := switch_equiv_is_Equivalence) in Xin.
    destruct Xin as [switch [Xrel Xin]].
    subst.
    apply Bag.mem_split with (ED := switch_eqdec) in Xrel.
    destruct Xrel as [sws Xrel].
    destruct switch.
    simpl in Xmem.
    destruct Xmem as [HMemInp | [HMemOutp | [HMemCtrlm | HMemSwitchm]]].

    (* At this point, we've discriminated all but one other major case.
       Packets on OpenFlow links may be on either side. *)

    (* ********************************************************************** *)
    (* Case 1 : Packet is in an input buffer                                  *)
    (* ********************************************************************** *)

    rewrite -> in_map_iff in HMemInp.
    destruct HMemInp as [[pt0 pk0] [Haffix HMemInp]].
    simpl in Haffix.
    inversion Haffix.
    subst.
    apply Bag.mem_in_to_list with 
      (R := eq) (E := @Equivalence_eq (portId * packet))
      in HMemInp.
    clear Haffix.
    eapply Bag.mem_split with (ED := ptPkt_eqdec) in HMemInp.
    destruct HMemInp as [inp HEqInp].

    remember (process_packet tbl0 pt pk) as ToCtrl eqn:Hprocess. 
    destruct ToCtrl as (outp',inPkts).

    eapply simpl_weak_sim...
    apply multistep_tau with
    (a0 := (State ({|Switch sw pts0 tbl0 (({|(pt,pk)|}) <+> inp) outp0
                                 ctrlm0 switchm0|} <+> sws)
                  links0
                  ofLinks0
                  ctrl0)).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> Xrel.
    do 2 rewrite -> (Bag.union_comm _ _ sws).
    apply Bag.pop_union_l.
    apply Bag.equiv_singleton.
    apply SwitchEquiv; try solve [ eauto | apply reflexivity ].
    eapply multistep_obs.
    apply PktProcess. 
    instantiate (1 := inPkts).
    instantiate (1 := outp').
    symmetry. exact Hprocess.
    eapply multistep_nil.
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.

    (* ********************************************************************** *)
    (* Case 2 : Packet is in an output buffer                                 *)
    (* ********************************************************************** *)

    (* Proof sketch: We can assume that (topo sw pt) is defined, which implies
       that a data-link exists from this switch to some destination, dst.
       We reach the final state in two steps: SendDataLink followed by
       PktProcess.*)
 
    (* 1. Rewrite outp0 as a union of (pt,pk) and some other bag.  *)
    apply Bag.mem_unions_map in HMemOutp.
    destruct HMemOutp as [[pt0 pk0] [HIn HMemOutp]]. 
    eapply Bag.mem_split with (ED := swPtPk_eqdec) in HMemOutp.
    destruct HMemOutp as [HEmpty HSingleton].
    simpl in HSingleton.
    remember (topo (swId0,pt0)) as Htopo.
    destruct Htopo.
    destruct p.
    apply Bag.singleton_equiv in HSingleton.
    destruct HSingleton as [HSingleton HEmpty'].
    clear HEmpty HEmpty'.
    inversion HSingleton. subst. clear HSingleton.
    apply Bag.mem_in_to_list 
      with 
        (R := eq) (E := @Equivalence_eq (portId * packet)) in HIn.
    apply Bag.mem_split with (ED := ptPkt_eqdec) in HIn.
    destruct HIn as [outp' HIn].

    assert (exists pks, In (DataLink (swId0,pt0) pks (sw,pt)) links0) as X.
    {
      unfold DevicesFromTopo in devicesFromTopo0.
      apply devicesFromTopo0 in HeqHtopo.
      destruct HeqHtopo as [sw0 [sw1 [lnk [_ [_ [Hlnk [_ [_ [HIdEq0 HIdEq1]]]]]]]]].
      simpl in Hlnk.
      destruct lnk.
      subst.
      simpl in *.
      rewrite -> HIdEq0 in Hlnk.
      rewrite -> HIdEq1 in Hlnk.
      exists pks0... }
    destruct X as [pks Hlink].

    (* 2. Rewrite links0 as the union of the link in Hlink and the rest. *)
    apply in_split in Hlink.
    destruct Hlink as [links01 [links02 Hlink]].
    subst.

    (* 3. Establish that the destination switch exists. *)
    assert 
      (LinkHasDst
         switches0
         (DataLink (swId0,pt0) pks (sw,pt))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
    destruct X as [outp1' pktIns].

    eapply Bag.mem_equiv with (ED := switch_eqdec) in HSw2In.
    2: exact Xrel.
    destruct HSw2In as [switch1 [HMem2 Hswitch2]].
    apply Bag.mem_union in HMem2.
    destruct HMem2. 

    idtac "TODO(arjun): src and dst switches are the same".
    admit.

    apply Bag.mem_split with (ED := switch_eqdec) in H1.
    destruct H1 as [sws0 H1].
    rewrite -> H1 in Xrel.

    apply simpl_weak_sim with
      (devs2 := 
        State ({| Switch swId0 pts0 tbl0 inp0 outp' ctrlm0 switchm0 |} <+>
               ({| Switch swId1 pts1 tbl1 
                          ((FromList (map (fun pk => (pt,pk)) pks)) <+> inp1) 
                          (FromList outp1' <+> outp1)
                        ctrlm1 (FromList (map (PacketIn pt) pktIns) <+> switchm1)|}) <+>
               sws0)
              (links01 ++ (DataLink (swId0,pt0) nil (swId1,pt)) :: links02)
              ofLinks0
              ctrl0).
    (* First step rewrites outp0 to outp' <+> (pt0,pk) *)
    apply multistep_tau with
    (a0 := State (({|Switch swId0 pts0 tbl0 inp0 (({|(pt0,pk)|}) <+> outp')
                            ctrlm0 switchm0|}) <+>
                  ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
                  sws0)
                 (links01 ++ (DataLink (swId0,pt0) pks (swId1,pt)) :: links02)
                 ofLinks0
                 ctrl0).
      apply StepEquivState.
      apply StateEquiv.
      rewrite -> Xrel.
      apply Bag.pop_union.
      apply Bag.equiv_singleton.
      apply SwitchEquiv; try solve [ apply reflexivity ].
      exact HIn.
      apply Bag.pop_union.
      apply Bag.equiv_singleton.
      apply symmetry...
      apply reflexivity.

    eapply ObserveFromOutp... (* #winning *)
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.
    trivial.
    assert (Mem (sw,pt,pk) Empty) as Hcontra.
    { eapply Bag.Mem_equiv.
      apply symmetry.
      exact HSingleton.
      simpl.
      left...
      apply reflexivity. }
    simpl in Hcontra.
    inversion Hcontra.

    (* ********************************************************************** *)
    (* Case 3 : Packet is in a PacketOut message                              *)
    (* ********************************************************************** *)
    apply Bag.mem_unions_map in HMemCtrlm.
    destruct HMemCtrlm as [msg [HIn HMemCtrlm]].
    destruct msg.
    2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* not a barrier *)
    2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ]. (* not a flowmod *)
    simpl in HMemCtrlm.
    remember (topo (swId0,p)) as Htopo.
    destruct Htopo.
    (* packet does not go poof *)
    2: solve [ simpl in HMemCtrlm; inversion HMemCtrlm ].
    destruct p1.
    simpl in HMemCtrlm.
    unfold Equivalence.equiv in HMemCtrlm.
    symmetry in HMemCtrlm. (* Prefer the names on the left *)
    inversion HMemCtrlm.
    subst.
    clear HMemCtrlm.
    subst.

    apply Bag.mem_in_to_list with 
      (R:=eq) (E:=@Equivalence_eq fromController) in HIn.
    apply Bag.mem_split with (ED:=fromController_eqdec) in HIn.
    destruct HIn as [ctrlm0' HIn].

    assert (exists pks, In (DataLink (swId0,p) pks (sw,pt)) links0) as X.
    { 
      unfold DevicesFromTopo in devicesFromTopo0.
      apply devicesFromTopo0 in HeqHtopo.
      destruct HeqHtopo as [sw0 [sw1 [lnk [_ [_ [Hlnk [_ [_ [HIdEq0 HIdEq1]]]]]]]]].
      simpl in Hlnk.
      destruct lnk.
      subst.
      simpl in *.
      rewrite -> HIdEq0 in Hlnk.
      rewrite -> HIdEq1 in Hlnk.
      exists pks0... }
    destruct X as [pks Hlink].
    apply in_split in Hlink.
    destruct Hlink as [links01 [links02 Hlink]].
    subst.

    assert 
      (LinkHasDst
         switches0
         (DataLink (swId0,p) pks (sw,pt))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    remember (process_packet tbl1 pt pk) as X eqn:Hprocess.
    destruct X as [outp1' pktIns].

    eapply Bag.mem_equiv with (ED := switch_eqdec) in HSw2In.
    2: exact Xrel.
    destruct HSw2In as [switch1 [HMem2 Hswitch2]].
    apply Bag.mem_union in HMem2.
    destruct HMem2. 

    idtac "TODO(arjun): src and dst switches are the same".
    admit.

    apply Bag.mem_split with (ED := switch_eqdec) in H1.
    destruct H1 as [sws0 H1].
    rewrite -> H1 in Xrel.

    apply simpl_weak_sim with
      (devs2 := 
        State ({| Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0' switchm0 |} <+>
               ({| Switch swId1 pts1 tbl1 
                          ((FromList (map (fun pk => (pt,pk)) pks)) <+> inp1) 
                          (FromList outp1' <+> outp1)
                        ctrlm1 (FromList (map (PacketIn pt) pktIns) <+> switchm1)|}) <+>
               sws0)
              (links01 ++ (DataLink (swId0,p) nil (swId1,pt)) :: links02)
              ofLinks0
              ctrl0).
    apply multistep_tau with
      (a0 :=
         (State (({|Switch swId0 pts0 tbl0 inp0 outp0 
                           (({|PacketOut p pk|}) <+> ctrlm0')
                           switchm0|}) <+>
                 ({|Switch swId1 pts1 tbl1 inp1 outp1 ctrlm1 switchm1|}) <+>
                 sws0)
                (links01 ++ (DataLink (swId0,p) pks (swId1,pt)) :: links02)
                ofLinks0
                ctrl0)).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> Xrel.
    apply Bag.pop_union.
    apply Bag.equiv_singleton.
    apply SwitchEquiv; try solve [ apply reflexivity | auto ].
    apply Bag.pop_union.
    apply Bag.equiv_singleton.
    apply symmetry...
    apply reflexivity.

    eapply multistep_tau.
    apply SendPacketOut.
    idtac "TODO(arjun): port exists".
    admit.
    eapply ObserveFromOutp... (* #winning *)
    
    unfold relate.
    simpl.
    rewrite -> H.
    apply reflexivity.
    trivial.

    (* ********************************************************************** *)
    (* Case 4 : Packet is in a PacketIn message                               *)
    (* ********************************************************************** *)

    apply Bag.mem_unions_map in HMemSwitchm.
    destruct HMemSwitchm as [switchm [HIn HMem]].
    destruct switchm.
    2: simpl in HMem; inversion HMem. (* not a barrier *)
    simpl in HMem.
    apply Bag.mem_in_to_list with 
      (R:=eq) (E:=@Equivalence_eq fromSwitch) in HIn.
    apply Bag.mem_split with (ED := fromSwitch_eqdec) in HIn.
    destruct HIn as [switchm1 HIn].

    assert (exists switchm0l ctrlm0l, 
              In (OpenFlowLink swId0 switchm0l ctrlm0l) ofLinks0) as X.
    { unfold SwitchesHaveOpenFlowLinks in swsHaveOFLinks0.
      simpl in swsHaveOFLinks0.
      assert (@Mem _ _  switch_Equivalence (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0) 
                  ({| (Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0)|} <+> sws)).
      { simpl. left. apply reflexivity. }
      apply Bag.mem_equiv with  (b2 := switches0) (ED := switch_eqdec) in H1.
      destruct H1 as [sw' [HMemSw HEq]].
      destruct sw'.
      inversion HEq.
      subst. clear HEq.
      apply swsHaveOFLinks0 in HMemSw.
      destruct HMemSw as [ofLink [HOFLinkIn HIdEq]].
      destruct ofLink.
      simpl in HIdEq.
      subst.
      eexists...
      apply symmetry... }

    destruct X as [switchm0l [ctrlm0l HOfLink]].
    apply in_split in HOfLink.
    destruct HOfLink as [ofLinks00 [ofLinks01 HOFLink]].
    subst.

    assert (step
              (State
                 switches0
                 links0
                 (ofLinks00 ++ 
                  (OpenFlowLink swId0 switchm0l ctrlm0l) :: ofLinks01)
                 ctrl0)
              None
              (State
                 (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 
                             (({|PacketIn p p0|}) <+> switchm1)|}) <+> sws)
                 links0
                 (ofLinks00 ++ 
                  (OpenFlowLink swId0 switchm0l ctrlm0l) :: ofLinks01)
                 ctrl0)) as Hstep1.
      apply StepEquivState.
      apply StateEquiv.
      rewrite -> Xrel.
      apply Bag.pop_union.
      apply Bag.equiv_singleton.
      apply SwitchEquiv; try solve [ apply reflexivity | auto ].
      apply reflexivity.

    destruct (DrainToController
                (({|Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm1|}) <+> sws)
                links0 ofLinks00 swId0 [PacketIn p p0] switchm0l ctrlm0l
                ofLinks01 ctrl0)
      as [sws1 [links1 [ofLinks10 [ofLinks11 [ctrl1 Hstep2]]]]].
    match goal with
      | [ H : multistep step ?s1 nil ?s2 |- _ ] =>
        remember s1 as S1; remember s2 as S2
    end.
    assert (FlowTablesSafe (switches S1)) as tblsOk0.
    { subst.
      unfold switches in *.
      eapply FlowTablesSafe_untouched.
      unfold FlowTablesSafe in *. 
      intros.
      eapply concreteState_flowTableSafety0...
      eapply Bag.Mem_equiv.
      apply symmetry.
      exact Xrel.
      exact H1. }
    assert (ConsistentDataLinks (links S1)) as linksTopoOk0. subst...
    assert (LinksHaveSrc (switches S1) (links S1)) as haveSrc0.
    { subst.
      unfold switches in *.
      simpl.
      eapply LinksHaveSrc_untouched.
      unfold LinksHaveSrc in *.
      intros.
      eapply LinkHasSrc_equiv.
      exact Xrel.
      eapply linksHaveSrc0... }
    assert (LinksHaveDst (switches S1) (links S1)) as haveDst0.
    { subst.
      unfold switches in *.
      simpl.
      eapply LinksHaveDst_untouched.
      unfold LinksHaveDst in *.
      intros.
      eapply LinkHasDst_equiv.
      exact Xrel.
      eapply linksHaveDst0... }
    assert (UniqSwIds (switches S1)) as uniqSwIds0'.
    { subst.
      unfold switches in *.
      idtac "TODO(arjun): UniqSwIds preserved under equivalence.".
      admit.
    }
    assert (AllFMS (switches S1) (ofLinks S1)) as allFMS0'.
    { subst.
      unfold switches in *.
      unfold ofLinks in *.
      idtac "TODO(arjun): AllFMS preserved under equivalence.".
      admit.
    }
    assert (P (switches S1) (ofLinks S1) (ctrl S1)) as ctrlP1'.
    { subst.
      unfold switches in *.
      unfold ofLinks in *.
      idtac "TODO(arjun): P preserved under equivalence; provided by controller".
      admit.
    }
    assert (AllDiff of_to (ofLinks S1)) as uniqOfLinkIds0'.
    { subst.
      unfold ofLinks in *.
      eapply AllDiff_preservation...
      do 2 rewrite -> map_app... }
    assert (OFLinksHaveSw (switches S1) (ofLinks S1)) as ofLinksHaveSw0'.
    { subst.
      unfold switches in *.
      unfold ofLinks in *.
      idtac "TODO(arjun): OFLinksHaveSw preserved under changes.".
      admit. }
    assert (DevicesFromTopo S1) as devicesFromTopo0'.
    { subst.
      idtac "TODO(arjun): DevicesFromTopo preserved under changes.".
      admit. }
    assert (SwitchesHaveOpenFlowLinks S1) as swsHaveOFLinks0'.
    { subst.
      idtac "TODO(arjun): SwitchesHaveOpenFlowLinks preserved under changes.".
      admit. }
    destruct (simpl_multistep tblsOk0 linksTopoOk0 haveSrc0 haveDst0 
                              uniqSwIds0' allFMS0' 
                              ctrlP1' uniqOfLinkIds0' ofLinksHaveSw0' 
                              devicesFromTopo0' 
                              swsHaveOFLinks0' Hstep2)
             as [tblsOk1 [linksTopoOk1 [haveSrc1 [haveDst1 
                  [uniqSwIds1 [allFMS1 [ctrlP1 [uniqOfLinkIds1 [ofLinksHaveSrc1
                  [devicesFromTopo1 [swsHaveOFLinks1 _]]]]]]]]]]].
    subst.
    simpl in *.
    destruct (ControllerRecvLiveness sws1 links1 ofLinks10 swId0 nil
                                     (PacketIn p p0)
                                     ctrlm0l ofLinks11 ctrl1)
             as [ctrl2 [Hstep3 [lps' HInCtrl]]].
    assert (exists y, Mem y (relate_controller ctrl2) /\ (sw,pt,pk) === y)
      as HMem2.
      simpl in HInCtrl.
      apply Bag.mem_split with (ED := swPtPk_eqdec) in HMem.
      destruct HMem as [lps1 HPk].
      rewrite -> HPk in HInCtrl.
      apply Bag.mem_equiv with (ED := swPtPk_eqdec) (b1 :=  (({|(sw, pt, pk)|}) <+> lps1) <+> lps')...
      simpl. left. left. apply reflexivity.
    destruct HMem2 as [y [HMem2 HEqy]].
    destruct y. destruct p1.
    unfold Equivalence.equiv in HEqy.
    symmetry in HEqy. inversion HEqy. subst. clear HEqy.
    destruct (ControllerLiveness
             sw pt pk ctrl2 sws1 links1
             (ofLinks10 ++ (OpenFlowLink swId0 nil ctrlm0l) :: ofLinks11)
             HMem2)
      as [ofLinks20 [ofLinks21 [ctrl3 [swTo [ptTo [switchmLst [ctrlmLst
          [Hstep4 HPktEq]]]]]]]].
    simpl in HPktEq.
    remember (topo (swTo, ptTo)) as X eqn:Htopo.
    destruct X.
    destruct p1. inversion HPktEq. subst. clear HPktEq.
    destruct (@EasyObservePacketOut sw pt swTo ptTo sws1 links1 ofLinks20
                                   switchmLst nil pk ctrlmLst
                                   ofLinks21 ctrl3) as [stateN stepN]...
    apply simpl_weak_sim with (devs2 := stateN).
    eapply multistep_tau.
    apply Hstep1.
    eapply multistep_tau.
    eapply SendToController.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    assert (PacketIn p p0 :: switchm0l = [PacketIn p p0] ++ switchm0l) as X.
      auto.
    rewrite -> X. clear X.
    apply Hstep2.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep3.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep4.
    apply stepN.
    reflexivity.
    reflexivity.
    reflexivity.
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.
    trivial.
    assert (Mem (sw,pt,pk) Empty) as Hcontra.
    { eapply Bag.Mem_equiv.
      apply symmetry.
      apply reflexivity.
      rewrite -> HPktEq.
      simpl.
      apply reflexivity. }
    inversion Hcontra.
  
    (* ********************************************************************** *)
    (* Case 5 : Packet is on a data link                                      *)
    (* ********************************************************************** *)
    apply Bag.mem_unions_map in HMemLink.
    destruct HMemLink as [link [HIn HMemLink]].
    simpl in HIn.
    destruct link.
    destruct dst0 as [sw0 pt0].
    simpl in HMemLink.
    rewrite -> in_map_iff in HMemLink.
    destruct HMemLink as [pk0 [HEq HMemLink]].
    inversion HEq. subst. clear HEq.
    apply in_split in HMemLink.
    destruct HMemLink as [pks01 [pks02 HMemLink]].
    subst.

    assert
      (LinkHasDst 
         switches0 (DataLink src0 (pks01 ++ pk :: pks02) (sw,pt))) as J0.
      apply linksHaveDst0...
    unfold LinkHasDst in J0.
    destruct J0 as [switch2 [HSw2In [HSw2IdEq HSw2PtsIn]]].
    destruct switch2.
    simpl in *.
    subst.
    apply Bag.mem_split with (ED:=switch_eqdec) in HSw2In.
    destruct HSw2In as [sws HSw2In].
    remember (process_packet tbl0 pt pk) as X eqn:Hprocess.
    destruct X as [pktOuts pktIns].

    apply in_split in HIn.
    destruct HIn as [links01 [links02 HIn]].
    subst.
    
    apply simpl_weak_sim with
      (devs2 := 
        State (({| Switch swId0 pts0 tbl0
                          ((FromList (map (fun pk => (pt,pk)) pks02)) <+> inp0) 
                          (FromList pktOuts <+> outp0)
                          ctrlm0
                          (FromList (map (PacketIn pt) pktIns) <+> switchm0)|})
                <+> sws)
              (links01 ++ (DataLink src0 pks01 (swId0,pt)) :: links02)
              ofLinks0
              ctrl0).
    apply multistep_tau with
      (a0 := 
        State (({| Switch swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0 |})
                <+> sws)
              (links01 ++ (DataLink src0 (pks01 ++ pk :: pks02)
                                    (swId0,pt)) :: links02)
              ofLinks0
              ctrl0).
    apply StepEquivState.
    apply StateEquiv.
    rewrite -> HSw2In.
    apply Bag.pop_union_l.
    apply reflexivity.
    assert ((pks01 ++ [pk]) ++ pks02 = pks01 ++ pk :: pks02) as X.
      rewrite <- app_assoc...
    rewrite <- X. clear X.
    eapply multistep_app with (obs2 := [(swId0,pt,pk)]).
    apply (DrainWire sws
      swId0 pts0 tbl0 inp0 outp0 ctrlm0 switchm0
      links01 src0 (pks01 ++ [pk]) pks02 swId0 pt links02 ofLinks0 ctrl0).
    eapply multistep_tau.
    apply RecvDataLink.
    eapply multistep_obs.
    apply PktProcess.
      symmetry. exact Hprocess.
    apply multistep_nil.
    trivial.
    rewrite -> H. apply reflexivity.
    trivial.

    (* ********************************************************************** *)
    (* Cases 6 and 7 : Packet is on an OpenFlow link                          *)
    (* ********************************************************************** *)
    simpl in HMemOFLink.
    simpl in *.
    apply Bag.mem_unions_map in HMemOFLink.
    destruct HMemOFLink as [link0 [HIn HMem]].
    destruct link0.
    simpl in HMem.
    destruct HMem as [HMemCtrlm | HMemSwitchm].

    (* ************************************************************************)
    (* Case 6 : Packet is in a PacketOut message from the controller          *)
    (* ************************************************************************)
    apply in_split in HIn.
    destruct HIn as [ofLinks01 [ofLinks02 HIn]]. subst.
    apply Bag.mem_unions_map in HMemCtrlm. 
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
    unfold Equivalence.equiv in HPk.
    symmetry in HPk. (* Prefer the names on the left *)
    inversion HPk. subst. clear HPk.
    subst.

    apply in_split in MemCtrlm.
    destruct MemCtrlm as [lstCtrlm0 [lstCtrlm1 HMemCtrlm]].
    subst.
    destruct (@EasyObservePacketOut sw pt srcSw p switches0 links0 ofLinks01
                                   of_switchm0 lstCtrlm0 pk lstCtrlm1
                                   ofLinks02 ctrl0) as [stateN stepN]...
    apply simpl_weak_sim with (devs2 := stateN)...
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.
    trivial.

    (* ************************************************************************)
    (* Case 7 : Packet is in a PacketIn message from a switch                 *)
    (* ************************************************************************)

    apply in_split in HIn.
    destruct HIn as [ofLinks00 [ofLinks01 HIn]]. subst.

    apply Bag.mem_unions_map in HMemSwitchm.
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
    assert (FlowTablesSafe (switches S1)) as tblsOk0. subst...
    assert (ConsistentDataLinks (links S1)) as linksTopoOk0. subst...
    assert (LinksHaveSrc (switches S1) (links S1)) as haveSrc0. subst...
    assert (LinksHaveDst (switches S1) (links S1)) as haveDst0. subst...
    assert (UniqSwIds (switches S1)) as uniqSwIds0'. subst...
    assert (AllFMS (switches S1) (ofLinks S1)) as allFMS0'. subst. admit...
    assert (P (switches S1) (ofLinks S1) (ctrl S1)) as ctrlP1'. admit.
    assert (AllDiff of_to (ofLinks S1)) as uniqOfLinkIds0'.
    { subst.
      unfold ofLinks in *.
      eapply AllDiff_preservation...
      do 2 rewrite -> map_app... }
    assert (OFLinksHaveSw (switches S1) (ofLinks S1)) as ofLinksHaveSw0'. admit.
    assert (DevicesFromTopo S1) as devicesFromTopo0'. admit.
    assert (SwitchesHaveOpenFlowLinks S1) as swsHaveOFLinks0'. admit.
    destruct (simpl_multistep tblsOk0 linksTopoOk0 haveSrc0 haveDst0 
                              uniqSwIds0' allFMS0' ctrlP1' uniqOfLinkIds0'
                              ofLinksHaveSw0' devicesFromTopo0' swsHaveOFLinks0' Hstep1)
             as [tblsOk1 [linksTopoOk1 [haveSrc1 [haveDst1 
                [uniqSwIds1 [allFMS1 [ctrlP1 [uniqOfLinkIds1 
                [ofLinksHaveSw1 [devicesFromTopo1 [swsHaveOFLinks1 _]]]]]]]]]]].
    subst.
    simpl in *.
    destruct (ControllerRecvLiveness sws1 links1 ofLinks10 of_to0 switchm0
                                     (PacketIn p p0)
                                     of_ctrlm0 ofLinks11 ctrl1)
             as [ctrl2 [Hstep2 [lps' HInCtrl]]]. 
    assert (exists y, Mem y (relate_controller ctrl2) /\ (sw,pt,pk) === y)
      as HMem2.
      simpl in HInCtrl.
      apply Bag.mem_split with (ED := swPtPk_eqdec) in HPk.
      destruct HPk as [lps1 HPk].
      rewrite -> HPk in HInCtrl.
      apply Bag.mem_equiv with (ED := swPtPk_eqdec) (b1 := (({|(sw, pt, pk)|}) <+> lps1) <+> lps')...
      simpl. left. left. apply reflexivity.
    destruct HMem2 as [y [HMem2 HEqy]].
    destruct y. destruct p1.
    unfold Equivalence.equiv in HEqy.
    symmetry in HEqy. inversion HEqy. subst. clear HEqy.
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
    apply simpl_weak_sim with (devs2 := stateN).
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    assert (switchm0 ++ PacketIn p p0 :: switchm1 =
             (switchm0 ++ [PacketIn p p0]) ++ switchm1) as X.
       rewrite <- app_assoc...
    rewrite -> X. clear X.
    apply Hstep1. clear Hstep1.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep2. clear Hstep2.
    eapply multistep_app with (obs2 := [(sw,pt,pk)]).
    apply Hstep3.
    apply stepN.
    reflexivity.
    reflexivity.
    reflexivity.
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.
    trivial.
    assert (Mem (sw,pt,pk) Empty) as Hcontra.
    { eapply Bag.Mem_equiv.
      apply reflexivity.
      rewrite -> HPktEq.
      simpl.
      apply reflexivity. }
    simpl in Hcontra.
    inversion Hcontra.

    (* ************************************************************************)
    (* Case 8 : Packet is at the controller                                   *)
    (* ************************************************************************)
    simpl in *.
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
    apply simpl_weak_sim with (devs2 := stateN)...
    eapply multistep_app...
    rewrite -> H.
    unfold relate.
    simpl.
    apply reflexivity.
    trivial.
    assert (Mem (sw,pt,pk) Empty) as Hcontra.
    { eapply Bag.Mem_equiv.
      apply symmetry.
      apply reflexivity.
      rewrite -> Hrel.
      simpl.
      apply reflexivity. }
    simpl in Hcontra.
    inversion Hcontra.
Qed.

End Make.