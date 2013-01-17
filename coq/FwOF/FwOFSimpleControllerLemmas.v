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
Require Import Common.Bisimulation.
Require FwOF.FwOFRelation.
Require Import Common.AllDiff.
Require Import FwOF.FwOFSimpleController.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.
 
Module MakeController (NetAndPol : NETWORK_AND_POLICY).
  Module Import Atoms := Make (NetAndPol).
  (* Module Import Controller := FwOF.FwOFSimpleController.Make (Atoms). *)
  Module Import ConcreteSemantics := ConcreteSemantics (Atoms).

  Definition relate_helper (sd : srcDst) :=
    match topo (pkSw sd,dstPt sd) with
      | None => {| |}
      | Some (sw',pt') => {| (sw',pt',dstPk sd) |}
    end.

  Definition relate_controller (st : controller) := 
    Bag.unions (map relate_helper (pktsToSend st)).

  Lemma ControllerRemembersPackets :
    forall (ctrl ctrl' : controller),
      controller_step ctrl ctrl' ->
      relate_controller ctrl = relate_controller ctrl'.
  Proof with auto.
    intros. inversion H.
  Qed.

  Lemma ControllerSendForgetsPackets : forall ctrl ctrl' sw msg,
    controller_send ctrl ctrl' sw msg ->
    relate_controller ctrl === select_packet_out sw msg <+>
    relate_controller ctrl'.
  Proof with auto.
    intros.
    inversion H; subst.
    unfold relate_controller.
    simpl.
    apply Bag.pop_union_r.
    unfold relate_helper.
    simpl.
    apply reflexivity.
    simpl.
    rewrite -> Bag.union_empty_l.
    apply reflexivity.
    rewrite -> Bag.union_empty_l.
    apply reflexivity.
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
    relate_controller ctrl' === select_packet_in sw msg <+> 
    (relate_controller ctrl).
  Proof with auto.
    intros.
    inversion H; subst.
    (* receive barrierreply *)
    unfold relate_controller.
    simpl.
    rewrite -> Bag.union_empty_l.
    apply reflexivity.
    (* case packetin *)
    unfold relate_controller.
    simpl.
    rewrite -> map_app.
    rewrite -> Bag.bag_unions_app.
    apply Bag.pop_union_r.
    unfold mkPktOuts.
    rewrite -> map_map.
    rewrite -> like_transfer_abs.
    apply reflexivity.
  Qed.
  
  Fixpoint FlowTableSafe_iter (sw : switchId) (tbl : flowTable) 
           (fms : list flowMod) :=
    match fms with
      | nil => True
      | fm :: fms' =>
        FlowTableSafe sw (modify_flow_table fm tbl) /\
        FlowTableSafe_iter sw  (modify_flow_table fm tbl) fms'
    end.

  Inductive CompleteFMS : switch -> openFlowLink -> switchState -> Prop :=
  | BarrierCFMS : forall swId pts tbl inp outp ctrlm switchm
                    ctrlmList switchmList swEp
                    ctrlFms ctrlTbl,
    SwitchEP (Switch swId pts tbl inp outp ctrlm switchm) swEp ->
    SafeWire swId (Endpoint_Barrier ctrlTbl) ctrlmList swEp ->
    FlowTableSafe_iter swId ctrlTbl ctrlFms ->
    CompleteFMS
      (Switch swId pts tbl inp outp ctrlm switchm)
      (OpenFlowLink swId switchmList ctrlmList)
      (SwitchState swId (Atoms.Endpoint_Barrier ctrlTbl) ctrlFms)
  | NoBarrierCFMS : forall swId pts tbl inp outp ctrlm switchm
                    ctrlmList switchmList swEp
                    ctrlFms ctrlTbl,
    SwitchEP (Switch swId pts tbl inp outp ctrlm switchm) swEp ->
    SafeWire swId (Endpoint_NoBarrier ctrlTbl) ctrlmList swEp  ->
    FlowTableSafe_iter swId ctrlTbl ctrlFms ->
    CompleteFMS
      (Switch swId pts tbl inp outp ctrlm switchm)
      (OpenFlowLink swId switchmList ctrlmList)
      (SwitchState swId (Atoms.Endpoint_NoBarrier ctrlTbl) ctrlFms).
  
  Inductive P : bag switch ->  list openFlowLink -> controller -> Prop :=
  | MkP : forall sws ofLinks swsts pktOuts,
      (forall swId ctrlEp ctrlFlowMods,
         In (SwitchState swId ctrlEp ctrlFlowMods) swsts ->
         
         (exists pts tbl inp outp ctrlm switchm switchmLst ctrlmLst,
            Mem (Switch swId pts tbl inp outp ctrlm switchm) sws /\
            In (OpenFlowLink swId switchmLst ctrlmLst) ofLinks /\
            CompleteFMS 
              (Switch swId pts tbl inp outp ctrlm switchm)
              (OpenFlowLink swId switchmLst ctrlmLst)
              (SwitchState swId ctrlEp ctrlFlowMods))) ->
        P sws ofLinks (Atoms.State pktOuts swsts).

  Hint Constructors P CompleteFMS.

  Lemma step_preserves_P : forall sws0 sws1 links0 links1 ofLinks0 ofLinks1 
    ctrl0 ctrl1 obs,
    step (State sws0 links0 ofLinks0 ctrl0)
         obs
         (State sws1 links1 ofLinks1 ctrl1) ->
    P sws0 ofLinks0 ctrl0 ->
    P sws1 ofLinks1 ctrl1.
  Proof with eauto with datatypes.
    intros.
    destruct H0; subst.
    inversion H; subst.
    admit.
    admit.
    admit.
    admit.
    admit.
    admit.
    (* controller step *)
    solve [inversion H6].
    (* controller recv *)
(*    assert (exists tbl pts inp outp ctrlm switchm,
              Mem (Switch swId0 pts tbl inp outp ctrlm switchm) sws1) as X.
    admit.
    destruct X as [tbl [pts [inp [outp [ctrlm [switchm HMem]]]]]].
*)
    inversion H6; subst.
    (* recv BarrierReply *)
    apply MkP; intros.
    destruct (H0 swId1 ctrlEp ctrlFlowMods H1) as
      [pts [tbl [inp [outp [ctrlm [switchm [switchmLst [ctrlmLst 
      [HSwMem [HOFLinkMem HCFMS]]]]]]]]]].
    apply in_app_iff in HOFLinkMem. simpl in HOFLinkMem.
    destruct HOFLinkMem as [W | [W | W]].

    exists pts. exists tbl. exists inp. exists outp. exists ctrlm.
    exists switchm. exists fromSwitch0. exists ctrlmLst.
    split...
    split...
    apply in_app_iff.
    left...
    assert (



    exists pts. exists tbl. exists inp. exists outp. exists ctrlm.
    exists switchm. exists fromSwitch0. exists ctrlmLst.
    split...
    split...

    exists 



  Lemma ControllerFMS : forall swId ctrl0 ctrl1 ctrlEp0 switchEp msg ctrlm
    switchm sws links ofLinks0 ofLinks1,
    P sws (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1) ctrl0 ->
    SafeWire swId ctrlEp0 ctrlm switchEp ->
    step
      (State
        sws links
        (ofLinks0 ++ (OpenFlowLink swId switchm ctrlm) :: ofLinks1)
        ctrl0)
      None
      (State
        sws links
        (ofLinks0 ++ (OpenFlowLink swId switchm (msg :: ctrlm)) :: ofLinks1)
        ctrl1) ->
      exists ctrlEp1,
        SafeWire swId ctrlEp1 (msg :: ctrlm) switchEp.
  Proof with auto with datatypes.
    intros.
    inversion H1.
    (* idiotic case of equivalent states *)
    admit.
    admit.
    admit.
    admit.
    admit.
    admit.
    admit.
    2: admit.
    2: admit.
    2: admit.
    subst.
    assert (msg = msg0) as X by admit.
    assert (swId0 = swId1) by admit.
    assert (fromSwitch0 = switchm0) by admit.
    assert (fromCtrl = ctrlm0) by admit.
    assert (ofLinks2 = ofLinks0) by admit.
    assert (ofLinks' = ofLinks1) by admit.
    subst.
    inversion H3; subst. (* consider the types of messages the controller may send. *)
    exists ctrlEp0. (* PacketOut *)
    solve [ apply SafeWire_PktOut; auto ].
    exists (Endpoint_Barrier (table_at_endpoint ctrlEp0)). (* BarrierRequest *)
    solve [ apply SafeWire_BarrierRequest; auto ].
    clear H5 H7.
    inversion H; subst.
    destruct (H6 swId1 (Atoms.Endpoint_Barrier tbl0) (fm::fms)) as
         [pts [tbl [inp [outp [ctrlm [switchm [switchmLst [ctrlmLst
                     [HMemSw [HInOfLnk HCompleteFMS]]]]]]]]]]...
    clear H6 H.
    assert (ctrlmLst = ctrlm0) as X by admit.
    subst.
    inversion HCompleteFMS; subst.
    assert (swEp0 = switchEp) as X by admit.
    subst.
    exists (Endpoint_NoBarrier (modify_flow_table fm tbl0)).
    apply SafeWire_FlowMod...
    simpl in H15.
    destruct H15...
  Qed.

  Lemma ControllerLiveness : forall sw pt pk ctrl0 sws0 links0 ofLinks0,
    Mem (sw,pt,pk) (relate_controller ctrl0) ->
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
  Proof.
    admit.
  Qed.
  Admitted.

End MakeController.
