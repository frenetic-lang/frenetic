Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import FwOF.FwOFSignatures.
Require Import Common.Bisimulation.
Require Import Common.AllDiff.

Local Open Scope list_scope.

(*

Need to reason about traces for Controller_FMS:

Do not try to prove Controller_FMS from a random state, but from a trace of states:

i.e., let the controller state be a history of all states it has been in, along
with the relationship between successive states.

- Need to know flow tables at switches on all states.
- Need to know that when history is empty, all switches have empty flow tables

LIVENESS:
  - Liveness is easy, I think!

SAFETY:

Informally:

If controller_send sends msg onto an OpenFlow link and INVARIANT holds and the
SafeWire holds for that link, then SafeWire holds after the msg is sent.


CHANGE: ControllerFMS should be able to assume that SafeWire holds over the
existing wire.


BAD:: step_preserves_P is very difficult to work with. Instead, show that
the INVARIANT holds if messages are dequeued by the switch.


NEW INVARIANT:

For all switches, sw consider the list of messages to be sent and the list of
messages on the wire (lst0 and lst1):
- SafeWire swId nil (lst0 ++ lst1) switchEp1 holds

If lst is the list of messages to be sent, an


THOUGHT:

If the controller is in a FwOF system, then P holds (I already have this!!!)

Proof:


*)


Module Make (NetAndPol : NETWORK_AND_POLICY) <: ATOMS.
  Include NetAndPol.

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
  | SendMessage : forall sw stsws stsws' msg msgs,
      Send 
        (State nil 
               (stsws ++ (SwitchState sw (msg::msgs)) :: stsws'))
        (State nil 
               (stsws ++ (SwitchState sw msgs) :: stsws'))
        sw
        msg.

  Inductive Step : state -> state -> Prop := .
           
  Definition controller_recv := Recv.
  Definition controller_step := Step.
  Definition controller_send := Send.
(*

  Fixpoint send (st : state) -> option (st * switchId * fromController) :=
    match st with
      | State ((SrcDst sw _ _ pt pk) :: pks) sws =>
        (State pks sws, sw, PacketOut pt pk)
      | State nil ((SwitchState sw (msg :: msgs)) :: sws) =>
        (State nil ((SwitchState sw msgs) :: sws), sw, msg)
      | State nil (SwitchState 
*)
End Make.
