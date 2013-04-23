Set Implicit Arguments.

Require Import Bag.TotalOrder.
Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import FwOF.FwOFSignatures.
Require Import Classifier.Classifier.
Require OpenFlow.OpenFlow0x01Types.
Require Network.NetworkPacket.
Require Network.PacketTotalOrder.
Require Import NetCore.NetCoreEval.
Require Import Pattern.Pattern.
Require Import Word.WordTheory.

Import ListNotations.

Local Open Scope list_scope.

Module NetworkAtoms <: NETWORK_ATOMS.

  Definition packet  := (Network.NetworkPacket.packet * OpenFlow.OpenFlow0x01Types.bufferId) % type.
  Definition switchId := OpenFlow.OpenFlow0x01Types.switchId.
  Definition portId := Network.NetworkPacket.portId.
  Definition flowTable := 
    list (nat * pattern * list (NetCore.NetCoreEval.act)).
  Inductive fm : Type :=
    | AddFlow : nat -> pattern -> list (NetCore.NetCoreEval.act) -> fm.

  Definition flowMod := fm.

  Inductive fromController : Type :=
  | PacketOut : portId -> packet -> fromController
  | BarrierRequest : nat -> fromController
  | FlowMod : flowMod -> fromController.

  Inductive fromSwitch : Type :=
  | PacketIn : portId -> packet -> fromSwitch
  | BarrierReply : nat -> fromSwitch.

  Definition strip_prio (x : nat * pattern * list (NetCore.NetCoreEval.act)) :=
    match x with
      | (prio,pat,act) => (pat,Some act)
    end.
  Require Import Common.Types.

  Definition eval_act (pt : portId) (pk : packet) (act : act) := 
    match act with
      (* We ignore modifications. *)
      | Forward _ (OpenFlow.OpenFlow0x01Types.PhysicalPort pt') => [(pt',pk)]
      (* And queries. *)
      | _ => nil
    end.

  (** Produces a list of packets to forward out of ports, and a list of packets
      to send to the controller. *)
  Definition process_packet (tbl : flowTable) (pt : portId) (pk : packet) :=
    match pk with
      | (actualPk, buf) =>
        match scan None (map strip_prio tbl) pt actualPk with
          | None => (nil, [pk])
          | Some acts => (concat_map (eval_act pt pk) acts, nil)
        end
    end.
    
  Definition modify_flow_table (fm : flowMod) (ft : flowTable) :=
    match fm with
      | AddFlow prio pat act => 
        (* Need to bring in LE for fixnums, without running afoul of 
           extraction. *)
        (prio,pat,act) :: ft
    end.


  Section TotalOrderings.
    
    Definition proj_fromController msg := 
      match msg with
        | PacketOut pt pk => inl (pt, pk)
        | BarrierRequest n => inr (inl n)
        | FlowMod f => inr (inr f)
      end.

    Definition inj_fromController sum :=
      match sum with
        | inl (pt, pk) => PacketOut pt pk
        | inr  (inl n ) => BarrierRequest n
        | inr (inr f) => FlowMod f
      end.

    Definition proj_fromSwitch msg :=
      match msg with
        | PacketIn pt pk => inl (pt, pk)
        | BarrierReply n => inr n
      end.
    
    Definition inj_fromSwitch sum :=
      match sum with
        | inl (pt, pk) => PacketIn pt pk
        | inr n => BarrierReply n
      end.

  End TotalOrderings.

  Definition packet_le := PairOrdering Network.PacketTotalOrder.packet_le Word32.le.

  Definition switchId_le := Word64.le.
  Definition portId_le := Word16.le.
  Parameter flowTable_le : Relation_Definitions.relation flowTable.
  Parameter flowMod_le : Relation_Definitions.relation flowMod.

  Definition fromSwitch_le :=
    ProjectOrdering proj_fromSwitch (SumOrdering (PairOrdering portId_le packet_le) le).

  Definition fromController_le :=
    ProjectOrdering proj_fromController (SumOrdering (PairOrdering portId_le packet_le) (SumOrdering le flowMod_le)).

  Instance TotalOrder_packet : TotalOrder packet_le.
  Proof. apply TotalOrder_pair; auto. exact Network.PacketTotalOrder.TotalOrder_packet. Qed.

  Definition TotalOrder_switchId := Word64.TotalOrder.
  Definition TotalOrder_portId := Word16.TotalOrder.

  Instance TotalOrder_flowMod : TotalOrder flowMod_le.
  Admitted.
  Instance TotalOrder_flowTable : TotalOrder flowTable_le.
  Admitted.

  Instance TotalOrder_fromController : TotalOrder fromController_le.
  Proof with auto.
    apply TotalOrder_Project with (g := inj_fromController)...
    + apply TotalOrder_sum.
      apply TotalOrder_pair.
      apply TotalOrder_portId.
      apply TotalOrder_packet.
      apply TotalOrder_sum.
      apply TotalOrder_nat.
      apply TotalOrder_flowMod.
    + unfold inverse.
      intros.
      destruct x...
  Qed.

  Instance TotalOrder_fromSwitch : TotalOrder fromSwitch_le.
  Proof with auto.
    apply TotalOrder_Project with (g := inj_fromSwitch)...
    + apply TotalOrder_sum.
      apply TotalOrder_pair.
      apply TotalOrder_portId.
      apply TotalOrder_packet.
      apply TotalOrder_nat.
    + unfold inverse.
      intros.
      destruct x...
  Qed.

End NetworkAtoms.
