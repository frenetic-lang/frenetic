Set Implicit Arguments.

Require Import Bag.TotalOrder.
Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import FwOF.FwOFSignatures.
Require Import Classifier.Classifier.
Require OpenFlow.OpenFlow0x01Types.
Require Network.Packet.
Require Import NetCore.NetCoreEval.
Require Import Pattern.Pattern.

Import ListNotations.

Local Open Scope list_scope.

Module NetworkAtoms <: NETWORK_ATOMS.

  Definition packet  := (Network.Packet.packet * OpenFlow.OpenFlow0x01Types.bufferId) % type.
  Definition switchId := OpenFlow.OpenFlow0x01Types.switchId.
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

  Extract Constant TotalOrder_packet =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".
  Extract Constant TotalOrder_switchId =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".
  Extract Constant TotalOrder_portId =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".
  Extract Constant TotalOrder_flowMod =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".
  Extract Constant TotalOrder_flowTable =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".
  Extract Constant TotalOrder_fromSwitch =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".
  Extract Constant TotalOrder_fromController =>
    "{ TotalOrder.compare = (fun x y -> x <= y);
       TotalOrder.eqdec = (fun x y -> x = y) }".

End NetworkAtoms.
