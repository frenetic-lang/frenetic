Set Implicit Arguments.

Require Import Bag.TotalOrder.
Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import FwOF.FwOFSignatures.
Require OpenFlow.MessagesDef.
Require Network.Packet.
Require NetCore.NetCoreEval.
Require Import Pattern.Pattern.

Import ListNotations.

Local Open Scope list_scope.

Module NetworkAtoms <: NETWORK_ATOMS.

  Definition packet := Network.Packet.packet.
  Definition switchId := OpenFlow.MessagesDef.switchId.
  Definition portId := Network.Packet.portId.
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

  (** Produces a list of packets to forward out of ports, and a list of packets
      to send to the controller. *)
  Parameter process_packet : flowTable -> portId -> packet -> 
    list (portId * packet) * list packet.

  Parameter modify_flow_table : flowMod -> flowTable -> flowTable.

  Parameter packet_le : Relation_Definitions.relation packet.
  Parameter switchId_le : Relation_Definitions.relation switchId.
  Parameter portId_le : Relation_Definitions.relation portId.
  Parameter flowTable_le : Relation_Definitions.relation flowTable.
  Parameter flowMod_le : Relation_Definitions.relation flowMod.
  Parameter fromSwitch_le : Relation_Definitions.relation fromSwitch.
  Parameter fromController_le : Relation_Definitions.relation fromController.

  Instance TotalOrder_packet : TotalOrder packet_le.
  Admitted.
  Instance TotalOrder_switchId : TotalOrder switchId_le.
  Admitted.
  Instance TotalOrder_portId : TotalOrder portId_le.
  Admitted.
  Instance TotalOrder_flowTable : TotalOrder flowTable_le.
  Admitted.
  Instance TotalOrder_flowMod : TotalOrder flowMod_le.
  Admitted.
  Instance TotalOrder_fromSwitch : TotalOrder fromSwitch_le.
  Admitted.
  Instance TotalOrder_fromController : TotalOrder fromController_le.
  Admitted.

End NetworkAtoms.
