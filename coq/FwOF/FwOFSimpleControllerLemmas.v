Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.TotalOrder.
Require Import Bag.Bag2.
Require Import FwOF.FwOFSignatures.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.
Require FwOF.FwOFMachine.
Require FwOF.FwOFSimpleController.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module MakeController (NetAndPol : NETWORK_AND_POLICY) <: ATOMS_AND_CONTROLLER.
  Module Import Atoms := FwOF.FwOFSimpleController.Make (NetAndPol).
  Module Machine := FwOF.FwOFMachine.Make (Atoms).
  Import Machine.

  Require Import Bag.Bag2.

  Inductive NotPacketOut : fromController -> Prop :=
  | BarrierRequest_NotPacketOut : forall xid,
      NotPacketOut (BarrierRequest xid)
  | FlowMod_NotPacketOut : forall fm,
      NotPacketOut (FlowMod fm).

  Hint Constructors NotPacketOut.

  Inductive Invariant : bag switch_le ->  list openFlowLink -> controller -> Prop :=
  | MkP : forall sws ofLinks swsts pktOuts,
      (forall sw swId0 pendingMsgs switchmLst ctrlmLst,
         In (SwitchState swId0 pendingMsgs) swsts ->
         In sw (to_list sws) ->
         In (OpenFlowLink swId0 switchmLst ctrlmLst) ofLinks ->
         swId sw = swId0 ->
         (forall msg, In msg pendingMsgs -> NotPacketOut msg) /\
         exists ctrlEp switchEp,
           SwitchEP sw switchEp /\
           SafeWire swId0 ctrlEp (rev pendingMsgs ++ ctrlmLst) 
                                  switchEp) ->
      Invariant sws ofLinks (Atoms.State pktOuts swsts).

  Hint Constructors Invariant.

  Lemma controller_recv_pres_P : forall sws ofLinks0 ofLinks1
    ctrl0 ctrl1 swId msg switchm ctrlm,
    Recv ctrl0 swId msg ctrl1 ->
    Invariant sws
              (ofLinks0 ++ (OpenFlowLink swId (switchm ++ [msg]) ctrlm) :: ofLinks1)
              ctrl0 ->
    Invariant sws 
              (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
              ctrl1.
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
    + inversion H0; subst.
      eapply MkP.
      intros.
      apply in_app_iff in H4. simpl in H4.
      destruct H4 as [H4 | [H4 | H4]]...
      inversion H4; subst; clear H4.
      edestruct H1 as [ctrlEp [switchEp [tbl Hsafewire]]]...
    + inversion H0; subst.
      eapply MkP...
      intros.
      eapply in_app_iff in H3; simpl in H3.
      destruct H3 as [H3 | [H3 | H3]]...
      inversion H3; subst; clear H3...
  Qed.

  Lemma controller_send_pres_P : forall sws ofLinks0 ofLinks1
    ctrl0 ctrl1 swId msg switchm ctrlm,
    Send ctrl0 ctrl1 swId msg  ->
    Invariant sws
              (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
              ctrl0 ->
    Invariant sws 
              (ofLinks0 ++ (OpenFlowLink swId switchm (msg :: ctrlm)) :: ofLinks1)
              ctrl1.
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
    + inversion H0; subst.
      eapply MkP.
      intros.
      apply in_app_iff in H3; simpl in H3.
      destruct H3 as [H3 | [H3 | H3]]...
      inversion H3; subst.
      edestruct H4 as [HNotPktOuts [ctrlEp [switchEp [tbl Hsafewire]]]]...
      split...
      exists ctrlEp.
      exists switchEp.
      split...
      idtac "TODO(arjun): need a lemma that you can always remove a PacketOut and 
             preserve Safewire with dependencies intact.".
      admit.
    + inversion H0; subst.
      eapply MkP.
      intros.
      destruct sw.
      simpl in *; subst.
      destruct (TotalOrder.eqdec swId1 swId0)...
      - subst.
        assert (OpenFlowLink swId0 switchmLst ctrlmLst = 
                OpenFlowLink swId0 switchm0 (msg::ctrlm0)) as X.
        { idtac "TODO(arjun): need to know that all OF link ids are unique".
          admit. } 
        inversion X; clear H3; subst; clear X.
        assert (SwitchState swId0 pendingMsgs = SwitchState swId0 msgs) as X.
        { idtac "TODO(arjun): need to know that all state ids are unique".
          admit. }
        inversion X; clear H1; subst; clear X.
        edestruct H4 with 
          (sw := Switch swId0 pts0 tbl0 inp0 outp0 ctrlm1 switchm1) 
          (swId1 := swId0)
          as [HNotPktOuts [ctrlEp [switchEp [tbl Hsafewire]]]]...
        split...
        assert (msg::msgs = [msg] ++ msgs) as X...
        rewrite -> X in Hsafewire; clear X.
        rewrite -> rev_app_distr in Hsafewire.
        rewrite <- app_assoc in Hsafewire.
        simpl in Hsafewire...
      - edestruct H4 with (sw := Switch swId1 pts0 tbl0 inp0 outp0 ctrlm1 switchm1) 
                            (swId1:=swId1) as [ctrlEp [switchEp [tbl Hsafewire]]]...
        { apply in_app_iff in H1; simpl in H1.
          destruct H1 as [H1 | [H1 | H1]]...
          inversion H1; subst. contradiction n... }
        { apply in_app_iff in H3; simpl in H3.
          destruct H3 as [H3 | [H3 | H3]]...
          inversion H3; subst. contradiction n... }
  Qed.

  Lemma controller_step_pres_P : forall sws ofLinks ctrl0 ctrl1,
    Step ctrl0 ctrl1 ->
    Invariant sws ofLinks ctrl0 ->
    Invariant sws ofLinks ctrl1.
  Proof.
    intros.
    inversion H.
  Qed.

  Local Open Scope bag_scope.
  Hint Constructors NotFlowMod.

  Lemma SwitchEP_vary : forall swId0 pts0 tbl0 inp0 outp0 cms0 sms0 ep
                               inp1 outp1 sms1,
    SwitchEP (Switch swId0 pts0 tbl0 inp0 outp0 cms0 sms0) ep ->
    SwitchEP (Switch swId0 pts0 tbl0 inp1 outp1 cms0 sms1) ep.
  Proof with auto with datatypes.
    intros.
    inversion H; subst...
    + eapply NoFlowModsInBuffer...
    + eapply OneFlowModInBuffer...
      intros.
      apply H8...
  Qed.

  Lemma SwitchEP_packetOut : forall swId0 pts0 tbl0 inp0 outp0 cms0 sms0 ep
                               pt pk,
    SwitchEP (Switch swId0 pts0 tbl0 inp0 outp0 (({|PacketOut pt pk|}) <+> cms0) sms0) ep <->
    SwitchEP (Switch swId0 pts0 tbl0 inp0 outp0 cms0 sms0) ep.
  Proof with auto with datatypes.
    split.
    { intros.
    inversion H; subst...
    + eapply NoFlowModsInBuffer...
      intros.
      apply H8.
      apply Bag.in_union...
    + assert (In (PacketOut pt pk) (to_list (({|FlowMod f|}) <+> ctrlm1))).
      { rewrite <- H9. apply Bag.in_union; simpl... }
      apply Bag.in_union in H0. simpl in H0.
      destruct H0 as [[H0 | H0] | H0]; try solve[inversion H0].
      destruct (Bag.in_split TotalOrder_fromController _ _ H0) as [rest Heq].
      subst.
      rewrite <- Bag.union_assoc in H9.
      rewrite -> (Bag.union_comm _ ({|FlowMod f|})) in H9.
      rewrite -> Bag.union_assoc in H9.
      apply Bag.pop_union_l in H9.
      subst.
      eapply OneFlowModInBuffer...
      intros.
      apply H8.
      apply Bag.in_union... }
    intros.
    inversion H; subst...
    + apply NoFlowModsInBuffer...
      intros.
      apply Bag.in_union in H0; simpl in H0.
      destruct H0 as [[H0|H0]|H0]...
      subst...
      inversion H0.
    + rewrite <- Bag.union_assoc.
      rewrite <-(Bag.union_comm _ ({|FlowMod f|})).
      rewrite -> Bag.union_assoc.
      apply OneFlowModInBuffer with (ctrlm0 := (({|PacketOut pt pk|}) <+> ctrlm1))... 
      intros.
      apply Bag.in_union in H0; simpl in H0.
      destruct H0 as [[H0|H0]|H0]...
      subst... inversion H0.
  Qed.

  Lemma Invariant_ofLink_vary : forall sws swId switchm0 switchm1 ctrlm 
                                   ofLinks0 ofLinks1 ctrl,
    Invariant sws 
              (ofLinks0 ++ OpenFlowLink swId switchm0 ctrlm :: ofLinks1)
              ctrl ->
    Invariant sws 
              (ofLinks0 ++ OpenFlowLink swId switchm1 ctrlm :: ofLinks1)
              ctrl.
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
    eapply MkP.
    intros.
    apply in_app_iff in H3; simpl in H3.
    destruct H3 as [H3 | [H3 | H3]]...
    destruct sw. simpl in *. subst.
    inversion H3; clear H3; subst...
  Qed.

  Lemma Invariant_sw_vary : forall swId pts tbl inp outp cms sms sws
                                   ofLinks ctrl inp' outp' sms',
    Invariant (({|Switch swId pts tbl inp outp cms sms|}) <+> sws)
              ofLinks
              ctrl ->
    Invariant (({|Switch swId pts tbl inp' outp' cms sms'|}) <+> sws)
              ofLinks
              ctrl.
  Proof with eauto with datatypes.
    intros.
    inversion H; subst.
    eapply MkP.
    intros.
    apply Bag.in_union in H2; simpl in H2.
    destruct H2 as [[H2 | H2] | H2]...
    - subst. simpl in *.
      remember (Switch swId0 pts0 tbl0 inp0 outp0 cms sms) as sw.
      assert (swId0 = swId sw) as X.
      { subst. simpl... }
      edestruct H0 as [HNotPktOuts [ctrlEp [switchEp [Hep Hsafewire]]]]...
      { apply Bag.in_union. left. simpl... }
      split...
      exists ctrlEp.
      exists switchEp.
      split...
      eapply SwitchEP_vary... rewrite -> Heqsw in Hep. exact Hep.
    - inversion H2.
    - destruct sw. simpl in *.
      rewrite -> H4 in *; clear swId2 H4.
      assert (swId1 = swId (Switch swId1 pts1 tbl1 inp1 outp1 ctrlm0 switchm0)) as X.
      { simpl... }
      edestruct H0 as [HNotPktOuts [ctrlEp [switchEp [Hep Hsafewire]]]]...
      { apply Bag.in_union. right... }
  Qed.
  
  Lemma SafeWire_switchEp_safe : forall swId ctrlEp lst switchEp,
    SafeWire swId ctrlEp lst switchEp ->
    FlowTableSafe swId (table_at_endpoint switchEp).
  Proof with eauto with datatypes.
    intros.
    generalize dependent ctrlEp.
    induction lst; intros.
    inversion H; subst...
    inversion H; subst...
  Qed.


  Lemma step_preserves_P : forall sws0 sws1 links0 links1 ofLinks0 ofLinks1 
    ctrl0 ctrl1 obs,
    step (State sws0 links0 ofLinks0 ctrl0)
         obs
         (State sws1 links1 ofLinks1 ctrl1) ->
    Invariant sws0 ofLinks0 ctrl0 ->
    Invariant sws1 ofLinks1 ctrl1.
  Proof with eauto with datatypes.
    intros.
    destruct ctrl1.
    inversion H0; subst.
    inversion H; subst.
    - eapply Invariant_sw_vary...
    - eapply MkP.
      intros.
      apply Bag.in_union in H3; simpl in H3.
      destruct H3 as [[H3 | H3] | H3]...
      + subst. simpl in *.
        remember (Switch swId0 pts0 tbl0 inp0 outp0 (({|Atoms.FlowMod fm|}) <+> ctrlm0) switchm0) as swInit.
        destruct H1 with 
          (sw := swInit)
          (pendingMsgs:=pendingMsgs)
          (swId0:=swId0)
          (switchmLst:=switchmLst)
          (ctrlmLst:=ctrlmLst) as [HNotPktOuts [ctrlEp [swEp [Hep Hsafewire]]]]...
        { apply Bag.in_union. simpl... }
        { rewrite -> HeqswInit... }
        split...
        exists ctrlEp.
        { inversion Hep; subst.
          + inversion H6; subst.
            assert (NotFlowMod (FlowMod fm)) as Hcontra.
            { apply H3. apply Bag.in_union. simpl... }
            inversion Hcontra.
          + inversion H7; subst; clear H7.
            assert (({|FlowMod f|}) <+> ctrlm2 = ({|FlowMod fm|}) <+> ctrlm0) as Heq.
            { apply Bag.ordered_irr. unfold to_list. simpl. exact H13.  }
            clear H13.
            assert (In (FlowMod f) (to_list (({|FlowMod fm|}) <+> ctrlm0))) as X.
            { 
              rewrite <- Heq.
              apply Bag.in_union. simpl... }
            apply Bag.in_union in X; simpl in X.
            destruct X as [[X | X] | X].
            inversion X; subst; clear X.
            assert (ctrlm2 = ctrlm0) as X.
            { rewrite -> Bag.pop_union_l. exact Heq. }
            subst; clear Heq.
            eexists.
            split...
            eapply NoFlowModsInBuffer...
            inversion X.
            assert (In (FlowMod fm) (to_list (({|FlowMod f|}) <+> ctrlm2))) as Y.
            { rewrite -> Heq. apply Bag.in_union; simpl... }
            apply Bag.in_union in Y; simpl in Y.
            destruct Y as [[Y | Y] | Y].
            inversion Y; subst.
            rewrite <- Bag.pop_union_l in Heq.
            subst.
            assert (NotFlowMod (FlowMod fm)) as Hcontra...
            inversion Hcontra.
            inversion Y.
            apply H3 in Y. inversion Y. }
      + inversion H3.
      + edestruct H1...
        { apply Bag.in_union. right... }
    - eapply MkP.
      intros.
      apply Bag.in_union in H3; simpl in H3.
      destruct H3 as [[H3 | H3] | H3]...
      + subst. simpl in *.
        remember (Switch swId0 pts0 tbl0 inp0 outp0 (({|PacketOut pt pk|}) <+> ctrlm0) switchm0) as swInit.
        destruct H1 with 
          (sw := swInit)
          (pendingMsgs:=pendingMsgs)
          (swId0:=swId0)
          (switchmLst:=switchmLst)
          (ctrlmLst:=ctrlmLst) as [HNotPktOuts [ctrlEp [swEp [Hep Hsafewire]]]]...
        { apply Bag.in_union. simpl... }
        { rewrite -> HeqswInit... }
        split...
        exists ctrlEp.
        rewrite -> HeqswInit in Hep.
        apply SwitchEP_packetOut in Hep.
        apply SwitchEP_vary with (inp1:=inp0) (outp1:=({|(pt,pk)|}) <+> outp0) (sms1:=switchm0) in Hep.
        exists swEp.
        split...
      + inversion H3.
      + edestruct H1...
        { apply Bag.in_union... }
    - eapply Invariant_sw_vary...
    - eapply Invariant_sw_vary...
    - eapply controller_step_pres_P...
    - eapply controller_recv_pres_P...
    - eapply controller_send_pres_P...
    - eapply Invariant_sw_vary...
      eapply Invariant_ofLink_vary...
    - eapply MkP.
      intros.
      destruct sw; subst; simpl in *.
      destruct (TotalOrder.eqdec swId0 swId2).
      + subst.
        assert (OpenFlowLink swId2 switchmLst ctrlmLst = 
                OpenFlowLink swId2 fromSwitch0 fromCtrl) as X.
        { idtac "TODO(arjun): need to know that all OF link ids are unique".
          admit. } 
        inversion X; clear H4; subst; clear X.
        assert (Switch swId2 pts1 tbl1 inp1 outp1 ctrlm0 switchm1 =
                Switch swId2 pts0 tbl0 inp0 outp0 ({||}) (({|BarrierReply xid|})<+>switchm0)) as X.
        { idtac "TODO(arjun): need to know that all switch  ids are unique".
          admit. } 
        inversion X; clear H3; subst; clear X.
        edestruct H1 as [HPktOuts [ctrlEp [switchEp [J J0]]]]...
        { apply Bag.in_union. left. simpl. left. reflexivity. }
        { simpl... }
        split...
        rewrite -> app_assoc in J0.
        remember (rev pendingMsgs ++ fromCtrl) as lst.
        destruct lst.
        * simpl in J0.
          inversion J0; subst.
          inversion H8; subst.
          apply SwitchEP_vary with (inp1:=inp0) (outp1:=outp0) (sms1:=({|BarrierReply xid|})<+>switchm0) in J.
          exists switchEp.
          exists switchEp.
          split...
        * apply SwitchEP_vary with (inp1:=inp0) (outp1:=outp0) (sms1:=({|BarrierReply xid|})<+>switchm0) in J.
          apply SafeWire_dequeue_BarrierRequest in J0.
          destruct J0 as [switchEp' [Hsafewire Htbl]].
          exists ctrlEp, switchEp'.
          split...
          apply NoFlowModsInBuffer...
          intros. simpl in H3. inversion H3.
          inversion J; subst...
          idtac "TODO(arjun): stupid contradiction in H12 that is easy to discharge.".
          admit. 
      + assert (In (Switch swId2 pts1 tbl1 inp1 outp1 ctrlm0 switchm1) (to_list sws)) as J.
        { apply Bag.in_union in H3; simpl in H3.
          destruct H3 as [[H3 | H3] | H3]...
          inversion H3; subst. contradiction n...
          inversion H3. }
        clear H3.
        eapply H1...
        { apply Bag.in_union... }
        { instantiate (1:=switchmLst).
          assert (In (OpenFlowLink swId2 switchmLst ctrlmLst) (ofLinks2 ++ ofLinks3)).
          { apply in_app_iff in H4; simpl in H4. 
            destruct H4 as [H4 | [H4 | H4]]...
            inversion H4; subst; contradiction n... }
          apply in_app_iff in H3.
          destruct H3... }
    - eapply MkP.
      intros.
      destruct sw; subst; simpl in *.
      destruct (TotalOrder.eqdec swId0 swId2).
      + subst.
        assert (OpenFlowLink swId2 switchmLst ctrlmLst = 
                OpenFlowLink swId2 fromSwitch0 fromCtrl) as X.
        { idtac "TODO(arjun): need to know that all OF link ids are unique".
          admit. } 
        inversion X; clear H4; subst; clear X.
        assert (Switch swId2 pts1 tbl1 inp1 outp1 ctrlm1 switchm1 =
                Switch swId2 pts0 tbl0 inp0 outp0 (({|msg|}) <+> ctrlm0) switchm0) as X.
        { idtac "TODO(arjun): need to know that all switch  ids are unique".
          admit. } 
        inversion X; clear H3; subst; clear X.
        edestruct H1 as [HPktOuts [ctrlEp [switchEp [J J0]]]]...
        { apply Bag.in_union. left. simpl. left. reflexivity. }
        { simpl... }
        split...
        rewrite -> app_assoc in J0.
        { destruct msg.
          + apply SafeWire_dequeue_PacketOut in J0.
            exists ctrlEp, switchEp.
            split...
            apply SwitchEP_packetOut...
          + idtac "TODO(arjun): barriers on bags again ffs".
            admit.
          + remember J0 as J1 eqn:Z; clear Z.
            apply SafeWire_dequeue_FlowMod in J0.
            exists ctrlEp, (Endpoint_NoBarrier (modify_flow_table f (table_at_endpoint switchEp))).
            split...
            { inversion J; subst.
              + simpl in *.
                destruct switchEp.
                simpl in *.
                apply SafeWire_fm_nb_false in J1. inversion J1. 
                eapply OneFlowModInBuffer...
                simpl in *.
                apply SafeWire_switchEp_safe in J0...
              + apply SafeWire_fm_nb_false in J1. inversion J1.  } }
      + assert (In (Switch swId2 pts1 tbl1 inp1 outp1 ctrlm1 switchm1) (to_list sws)) as J.
        { apply Bag.in_union in H3; simpl in H3.
          destruct H3 as [[H3 | H3] | H3]...
          inversion H3; subst. contradiction n...
          inversion H3. }
        clear H3.
        eapply H1...
        { apply Bag.in_union... }
        { instantiate (1:=switchmLst).
          assert (In (OpenFlowLink swId2 switchmLst ctrlmLst) (ofLinks2 ++ ofLinks3)).
          { apply in_app_iff in H4; simpl in H4. 
            destruct H4 as [H4 | [H4 | H4]]...
            inversion H4; subst; contradiction n... }
          apply in_app_iff in H3.
          destruct H3... }
  Qed.

  Definition relate_helper (sd : srcDst) : swPtPks :=
    match topo (pkSw sd,dstPt sd) with
      | None => {| |}
      | Some (sw',pt') => {| (sw',pt',dstPk sd) |}
    end.

  Definition relate_controller (st : controller) := 
    unions (map relate_helper (pktsToSend st)).

  Lemma ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.
  Proof with auto.
    intros. inversion H.
  Qed.

  Lemma ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl = select_packet_out sw msg <+>
    relate_controller ctrl'.
  Proof with auto.
    intros.
    inversion H; subst.
    + unfold relate_controller.
      simpl.
      unfold relate_helper.
      simpl.
      rewrite -> Bag.unions_cons.
      reflexivity.
    + simpl.
      unfold relate_controller.
      simpl.
      { destruct msg.
        - idtac "TODO(arjun): Cannot pre-emit packetouts (need P here).".
          admit.
        - simpl. rewrite -> Bag.union_empty_l...
        - simpl. rewrite -> Bag.union_empty_l... }
  Qed.

  Lemma like_transfer : forall srcPt srcPk sw ptpk,
    relate_helper (mkPktOuts_body sw srcPt srcPk ptpk) =
    transfer sw ptpk.
  Proof with auto.
    intros.
    unfold mkPktOuts_body.
    unfold relate_helper.
    unfold transfer.
    destruct ptpk.
    simpl.
    reflexivity.
  Qed.

  Lemma like_transfer_abs : forall sw pt pk lst,
    map
      (fun x : portId * packet => relate_helper (mkPktOuts_body sw pt pk x))
      lst =
    map (transfer sw) lst.
  Proof with auto.
    intros.
    induction lst...
    simpl.
    rewrite -> like_transfer.
    rewrite -> IHlst.
    reflexivity.
  Qed.

  Lemma ControllerRecvRemembersPackets : forall ctrl ctrl' sw msg,
    controller_recv ctrl sw msg ctrl' ->
    relate_controller ctrl' = select_packet_in sw msg <+> 
    (relate_controller ctrl).
  Proof with auto.
    intros.
    inversion H; subst.
    (* receive barrierreply *)
    unfold relate_controller.
    simpl. 
    rewrite -> Bag.union_empty_l...
    (* case packetin *)
    unfold relate_controller.
    simpl.
    rewrite -> map_app.
    rewrite -> Bag.unions_app.
    apply Bag.pop_union_r.
    unfold mkPktOuts.
    rewrite -> map_map.
    rewrite -> like_transfer_abs...
  Qed.
  
  Definition P := Invariant.

  Axiom ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 
                                        ofLinks0,
    In (sw,pt,pk) (to_list (relate_controller ctrl0)) ->
    exists  ofLinks10 ofLinks11 ctrl1 swTo ptTo switchmLst ctrlmLst,
      (multistep 
         step (State sws0 links0 ofLinks0 ctrl0) nil
         (State sws0 links0
                (ofLinks10 ++ 
                 (OpenFlowLink swTo switchmLst 
                  (PacketOut ptTo pk :: ctrlmLst)) ::
                 ofLinks11) 
                ctrl1)) /\
      select_packet_out swTo (PacketOut ptTo pk) = ({|(sw,pt,pk)|}).
  (* Idiotic pre-conditions needed. 
    Proof with auto with datatypes.
      intros.
      destruct ctrl0.
      unfold relate_controller in H.
      simpl in H.
      apply Bag.in_unions in H.
      destruct H as [b [HBagIn HpkIn]].
      apply in_map_iff in HBagIn.
      destruct HBagIn as [srcDst0 [Hhelper HtoSend]].
      destruct srcDst0.
      unfold relate_helper in Hhelper.
      simpl in Hhelper.
      destruct (topo
      *)

  Axiom ControllerRecvLiveness : forall sws0 links0 ofLinks0 sw switchm0 m 
    ctrlm0 ofLinks1 ctrl0,
     exists ctrl1,
      (multistep 
         step
         (State 
            sws0 links0 
            (ofLinks0 ++ (OpenFlowLink sw (switchm0 ++ [m]) ctrlm0) :: ofLinks1)
            ctrl0)
         nil
         (State 
            sws0 links0 
            (ofLinks0 ++ (OpenFlowLink sw switchm0 ctrlm0) :: ofLinks1)
            ctrl1)) /\
       exists (lps : swPtPks),
         (select_packet_in sw m) <+> lps = relate_controller ctrl1.

  Lemma SafeWire_app : forall swId ep0 ep1 lst lst0,
    SafeWire swId ep0 (lst ++ lst0) ep1 ->
    exists ep, SafeWire swId ep lst0 ep1.
  Proof with eauto with datatypes.
    intros.
    generalize dependent ep0.
    induction lst; intros...
    simpl in H.
    inversion H; subst...
  Qed.


  Lemma SwitchEP_injective : forall sw ep0 ep1,
    SwitchEP sw ep0 ->
    SwitchEP sw ep1 ->
    table_at_endpoint ep0 = table_at_endpoint ep1.
  Proof with eauto with datatypes.
    intros.
    destruct sw.
    inversion H; inversion H0; subst...
    + assert (NotFlowMod (FlowMod f)).
      apply H9.
      apply Bag.in_union; simpl...
      inversion H1.
    + assert (NotFlowMod (FlowMod f)) as X; subst...
      apply H20.
      apply Bag.in_union; simpl...
      inversion X.
    + assert (f = f0) as X.
      { assert (In (FlowMod f) (to_list (({|FlowMod f0|}) <+> ctrlm4))) as X.
        rewrite <- H21.
        apply Bag.in_union; simpl...
        apply Bag.in_union in X.
        simpl in X; destruct X as [[X|X]|X].
        inversion X...
        inversion X.
        assert (NotFlowMod (FlowMod f)) as Y.
        apply H20...
        inversion Y. }
      subst...
  Qed.

  Lemma SafeWire_injective_lhs : forall swId ep0 ep1 lst tbl,
    SafeWire swId ep0 lst (Endpoint_NoBarrier tbl) ->
    SafeWire swId ep1 lst (Endpoint_Barrier tbl) ->
    table_at_endpoint ep0 = table_at_endpoint ep1.
  Proof with eauto with datatypes.
    intros.
    generalize dependent ep0.
    generalize dependent ep1.
    induction lst; intros.
    + inversion H; inversion H0; subst...
    + destruct a.
      - inversion H; inversion H0; subst...
      - inversion H; inversion H0; subst...
        destruct (IHlst _ H12 _ H6)...
      - inversion H; inversion H0; subst...
        remember (IHlst (Endpoint_Barrier tbl2) H14 (Endpoint_Barrier tbl1) H7) as X.
        simpl in X.
        rewrite -> X.
        reflexivity.
  Qed.

  Lemma SafeWire_insane_1 : forall swId ctrlEp0 lst tbl tbl0,
    SafeWire swId ctrlEp0 lst (Endpoint_Barrier tbl) ->
    SafeWire swId (Endpoint_Barrier tbl0) lst (Endpoint_NoBarrier tbl) ->
    SafeWire swId (Endpoint_Barrier tbl0) lst (Endpoint_Barrier tbl).
  Proof with eauto with datatypes.
    intros.
    generalize dependent ctrlEp0.
    induction lst; intros...
    + inversion H0.
    + destruct a.
      - eapply SafeWire_PktOut. 
        inversion H0; subst.
        eapply IHlst...
        inversion H...
      - assert (table_at_endpoint (Endpoint_Barrier tbl1) = table_at_endpoint ctrlEp0) as X.
        { eapply SafeWire_injective_lhs... }
        destruct ctrlEp0. 
        inversion H.
        simpl in X.
        subst.
        exact H.
      - inversion H0.
  Qed.

  Lemma SafeWire_insane_2 : forall swId ctrlEp0 lst tbl tbl0,
    SafeWire swId ctrlEp0 lst (Endpoint_NoBarrier tbl) ->
    SafeWire swId (Endpoint_Barrier tbl0) lst (Endpoint_Barrier tbl) ->
    SafeWire swId (Endpoint_Barrier tbl0) lst (Endpoint_NoBarrier tbl).
  Proof with eauto with datatypes.
    intros.
    generalize dependent ctrlEp0.
    induction lst; intros...
    + admit. (* need to relax SafeWire_nil. *)
    + destruct a.
      - eapply SafeWire_PktOut. 
        inversion H0; subst.
        eapply IHlst...
        inversion H...
      - assert (table_at_endpoint ctrlEp0 =  table_at_endpoint (Endpoint_Barrier tbl1)) as X.
        { eapply SafeWire_injective_lhs... }
        destruct ctrlEp0. 
        inversion H.
        simpl in X.
        subst.
        exact H.
      - inversion H0.
  Qed.

  Lemma ControllerFMS : forall swId ctrl0 ctrl1 msg ctrlm
    switchm sws links ofLinks0 ofLinks1 switchEp ctrlEp0
    pts tbl inp outp swCtrlm swSwitchm,
    SafeWire swId ctrlEp0 ctrlm switchEp ->
    P sws
      (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
      ctrl0 ->
    controller_send ctrl0 ctrl1 swId msg ->
    step
      (State
        sws
        links
        (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
        ctrl0)
      None
      (State
         sws
         links
         (ofLinks0 ++ (OpenFlowLink swId switchm (msg :: ctrlm)) :: ofLinks1)
         ctrl1) ->
     In (Switch swId pts tbl inp outp swCtrlm swSwitchm) (to_list sws) ->
     SwitchEP (Switch swId pts tbl inp outp swCtrlm swSwitchm) switchEp ->
      exists ctrlEp1,
        SafeWire swId ctrlEp1 (msg :: ctrlm) switchEp.
    Proof with eauto with datatypes.
      intros.
      inversion H1; subst.
      + exists ctrlEp0.
        apply SafeWire_PktOut...
      + remember H2 as J eqn:X; clear X.
        apply step_preserves_P in H2...
        clear H1.
        inversion H2; subst.
        edestruct H7 as [HNotPktOuts [ctrlEp2 [switchEp0 [Hep Hsafewire]]]]...
        destruct msg.
        - exists ctrlEp0.
          apply SafeWire_PktOut...
        - exists (Endpoint_Barrier (table_at_endpoint ctrlEp0)).
          apply SafeWire_BarrierRequest...
        - assert (table_at_endpoint switchEp = table_at_endpoint switchEp0) as X.
          { eapply SwitchEP_injective... }
          destruct switchEp, switchEp0; simpl in X; subst.
          * apply SafeWire_app in Hsafewire.
            destruct Hsafewire as [ctrlEp3 Hsafewire].
            inversion Hsafewire; subst.
            exists (Endpoint_NoBarrier (modify_flow_table f tbl1)).
            eapply SafeWire_FlowMod...
          * apply SafeWire_app in Hsafewire.
            destruct Hsafewire as [ctrlEp3 Hsafewire].
            inversion Hsafewire; subst.
            exists (Endpoint_NoBarrier (modify_flow_table f tbl1)).
            eapply SafeWire_FlowMod...
            eapply SafeWire_insane_2...
          * apply SafeWire_app in Hsafewire.
            destruct Hsafewire as [ctrlEp3 Hsafewire].
            inversion Hsafewire; subst.
            exists (Endpoint_NoBarrier (modify_flow_table f tbl1)).
            eapply SafeWire_FlowMod...
            eapply SafeWire_insane_1...
          * apply SafeWire_app in Hsafewire.
            destruct Hsafewire as [ctrlEp3 Hsafewire].
            inversion Hsafewire; subst.
            exists (Endpoint_NoBarrier (modify_flow_table f tbl1)).
            eapply SafeWire_FlowMod...
    Qed.

End MakeController.