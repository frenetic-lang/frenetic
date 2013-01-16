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
(*Require FwOF.FwOFRelation.*)
Require Import Common.AllDiff.
Require Import FwOF.FwOFSimpleController.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.
 
Module MakeController (NetAndPol : NETWORK_AND_POLICY). (* <: ATOMS_AND_CONTROLLER. *)

  Module Atoms := Make (NetAndPol).
  Export Atoms.

  Module Export RelationDefs := ConcreteSemantics (Atoms).

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

  Check map_map.

  Check transfer.
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

  Lemma ControllerFMS : forall swId ctrl0 ctrl1 ctrlEp0 switchEp msg ctrlm
    switchm sws links ofLinks0 ofLinks1,
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
    inversion H0.
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
    inversion H2; subst. (* consider the types of messages the controller may send. *)
    exists ctrlEp0. (* PacketOut *)
    solve [ apply SafeWire_PktOut; auto ].
    exists (Endpoint_Barrier (table_at_endpoint ctrlEp0)). (* BarrierRequest *)
    solve [ apply SafeWire_BarrierRequest; auto ].
    destruct ctrlEp0. (* FlowMod *)
    (* TODO(arjun): must eliminate this impossible case *)
    admit.
    exists (Endpoint_NoBarrier (modify_flow_table fm tbl0)).
    apply SafeWire_FlowMod.
    (* TODO(arjun): must show that the flow table is safe *)
    admit.
    (* TODO(arjun): must show that the flow table in the EP is what the controller
       thinks there is too. *)
    admit.
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
