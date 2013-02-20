Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.Bag.
Require Import FwOF.FwOFSignatures.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Module Make (NetAndPol : NETWORK_AND_POLICY) <: ATOMS.
  Include NetAndPol.

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
  
  Definition mkPktOuts_body sw srcPt srcPk ptpk :=
    match ptpk with
      | (dstPt,dstPk) => SrcDst sw srcPt srcPk dstPt dstPk
    end.

  Definition mkPktOuts (sw : switchId) (srcPt : portId) (srcPk : packet) :=
    map (mkPktOuts_body sw srcPt srcPk)
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
