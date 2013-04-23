Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import FwOF.FwOFExtractableSignatures.
Require OpenFlow.OpenFlow0x01Types.
Require Network.NetworkPacket.
Require NetCore.NetCoreEval.
Require Import Pattern.Pattern.

Local Open Scope list_scope.

Module Type POLICY.
  Require Import OpenFlow.OpenFlow0x01Types.
  Require Import Network.NetworkPacket.

  Parameter abst_func : switchId -> portId -> (packet * bufferId) -> list (portId * (packet * bufferId)).

End POLICY.

Module MakeAtoms (Policy : POLICY) <: EXTRACTABLE_ATOMS.
  Definition switchId := OpenFlow.OpenFlow0x01Types.switchId.
  Definition portId := Network.NetworkPacket.portId.
  Definition packet := (Network.NetworkPacket.packet * OpenFlow.OpenFlow0x01Types.bufferId) %type.
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
  
  Definition abst_func := Policy.abst_func.
  
End MakeAtoms.

Module MakeController (Atoms_ : EXTRACTABLE_ATOMS) <: EXTRACTABLE_CONTROLLER.
  
  Import Atoms_.

  Record switchState := SwitchState {
    theSwId : switchId;
    pendingCtrlMsgs : list fromController
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
  
  Definition mkPktOuts_body sw srcPt srcPk ptpk :=
    match ptpk with
      | (dstPt,dstPk) => SrcDst sw srcPt srcPk dstPt dstPk
    end.

  Definition mkPktOuts (sw : switchId) (srcPt : portId) (srcPk : packet) :=
    map (mkPktOuts_body sw srcPt srcPk)
      (abst_func sw srcPt srcPk).

  Definition controller := state.

  Fixpoint send_queued (swsts : list switchState) : option (list switchState * switchId * fromController) :=
    match swsts with
      | nil => None
      | (SwitchState sw (msg :: msgs)) :: ss =>
        Some (SwitchState sw msgs :: ss, sw, msg)
      | (SwitchState sw nil) :: ss =>
        match send_queued ss with
          | None => None
          | Some (ss', sw',msg) => Some (SwitchState sw nil :: ss', sw', msg)
        end
    end.

  Fixpoint send (st : state) : option (state * switchId * fromController) :=
    match st with
      | State ((SrcDst sw _ _ pt pk) :: pks) sws =>
        Some (State pks sws, sw, PacketOut pt pk)
      | State nil ss => 
        match send_queued ss with
          | None => None
          | Some (ss', sw, msg) => Some (State nil ss', sw, msg)
        end
    end.

  Fixpoint recv (st : state) (sw : switchId) (msg : fromSwitch) :=
    match msg with
      | BarrierReply _ => st
      | PacketIn pt pk =>
        match st with
          | State pktOuts ss =>
            State (mkPktOuts sw pt pk ++ pktOuts) ss
        end
    end.

  Module Atoms := Atoms_.

End MakeController.

