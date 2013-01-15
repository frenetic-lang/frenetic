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

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (NetAndPol : NETWORK_AND_POLICY) <: ATOMS.
  Include NetAndPol.
  Export NetAndPol.

  Inductive Endpoint : Type :=
  | Endpoint_NoBarrier : flowTable -> Endpoint
  | Endpoint_Barrier : flowTable -> Endpoint.

  Record switchState := SwitchState {
    theSwId : switchId;
    swEp : Endpoint;
    flowModsForSw : list flowMod
  }.

  Record srcDst := SrcDst {
    pkSw : switchId;
    srcPt : portId;
    srcPk : packet;
    dstPt : portId;
    dstPk : packet
  }.

  Record state := State {
    pktsToSend : list srcDst;
    switchStates : list switchState
  }.

  Definition mkPktOuts (sw : switchId) (srcPt : portId) (srcPk : packet) :=
    map 
      (fun ptpk =>
         match ptpk with
           | (dstPt,dstPk) => SrcDst sw srcPt srcPk dstPt dstPk
         end)
      (abst_func sw srcPt srcPk).

  Definition controller := state.

  Inductive Recv : controller -> switchId -> fromSwitch -> controller -> Prop :=
  | RecvBarrierReply : forall st swId n,
    Recv st swId (BarrierReply n) st
  | RecvPacketIn : forall swsts pksToSend sw pt pk,
    Recv (State pksToSend swsts) 
         sw (PacketIn pt pk)
         (State (mkPktOuts sw pt pk ++ pksToSend) swsts).

  Inductive Send : state -> state -> switchId -> fromController -> Prop :=
  | SendPacketOut : forall swsts srcPt srcPk dstPt dstPk sw lps,
    Send (State ((SrcDst sw srcPt srcPk dstPt dstPk)::lps) swsts)
         (State lps swsts)
         sw
         (PacketOut dstPt dstPk)
  | SendBarrier : forall sw tbl flowMods stsws stsws',
      Send 
        (State nil 
               (stsws ++ 
                (SwitchState sw (Endpoint_NoBarrier tbl) flowMods) ::
                stsws'))
        (State nil 
               (stsws ++
                (SwitchState sw (Endpoint_Barrier tbl) flowMods) ::
                stsws'))
        sw
        (BarrierRequest 0)
  | SendFlowMod : forall sw tbl fm fms stsws stsws',
      Send
        (State nil 
               (stsws ++ 
                (SwitchState sw (Endpoint_Barrier tbl) (fm::fms)) :: 
                stsws'))
        (State nil 
               (stsws ++
                (SwitchState sw (Endpoint_NoBarrier (modify_flow_table fm tbl))
                             fms) :: 
                stsws'))
        sw
        (FlowMod fm).

  Inductive Step : state -> state -> Prop :=
  .
           
  Definition controller_recv := Recv.
  Definition controller_step := Step.
  Definition controller_send := Send.


End Make.
  
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
    unfold relate_helper.
    rewrite -> map_map.
    simpl.
    rewrite -> fold_right_app.
    simpl.
    simpl.
    rewrite -> map_app.
    rewrite -> Bag.bag_unions_app.
    rewrite -> like_transfer.
    apply reflexivity.
  Qed.


    unfold affixSw.
    rewrite -> map_map.
    simpl.
    simpl.
    simpl.
    induction pksToSend.
    simpl.
    rewrite -> app_nil_r.
    rewrite -> Bag.FromList_nil_is_Empty.
    rewrite -> Bag.union_empty_r.
    remember (abst_func sw pt pk) as pks.
    induction pks.
    simpl.
    rewrite -> Bag.FromList_nil_is_Empty.
    apply reflexivity.
    


    simpl.
    rewrite -> fold_right_app.
    Check fold_right_app.

    destruct (topo (sw,pt)).
    destruct p.
    rewrite -> Bag.from_list_cons.
    apply reflexivity.
    rewrite -> Bag.union_empty_l.
    apply reflexivity.
    (* case 2*)
    unfold relate_controller.
    simpl.
    rewrite -> Bag.union_empty_l.
    apply reflexivity.
    (* case 3 *)
    unfold relate_controller.
    simpl.
    rewrite -> Bag.union_empty_l.
    apply reflexivity.





    

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


Print ControllerLiveness.
End MakeController.

Print MakeController.