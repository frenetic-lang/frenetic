Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import NetCore.NetCoreEval.
Require Import Common.Types.
Require Import Classifier.Classifier.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
(* TODO: MJR Move 'switchId' from messagesDef so that we don't have to include this whole thing *)
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreTypes.
Require Import Network.NetworkPacket.

Set Implicit Arguments.

Import ListNotations.

Fixpoint desugar_pred (p : predicate) := match p with
  | DlSrc eth => PrHdr (Pattern.dlSrc eth)
  | DlDst eth => PrHdr (Pattern.dlDst eth)
  (* | DlTyp typ => PrHdr (Pattern.dlType typ) *)
  (* | DlVlan (Some vlan) => PrHdr (Pattern.dlVlan vlan) *)
  (* | DlVlan None => PrHdr (Pattern.dlVlan VLAN_NONE) *)
  (* | NwProto proto => PrHdr (Pattern.nwProto proto) *)
  | Switch sw => PrOnSwitch sw
  | InPort pt => PrHdr (Pattern.inPort pt)
  | And p1 p2 =>
      (* de Morgan's law *)
      PrNot (PrOr (PrNot (desugar_pred p1)) (PrNot (desugar_pred p2)))
  | Or p1 p2 => PrOr (desugar_pred p1) (desugar_pred p2)
  | Not p => PrNot (desugar_pred p)
  | All => PrAll
  | NoPackets => PrNone
end.

Definition desugar_action (a : action) := 
  match a with
    | To p => Forward unmodified (PhysicalPort  p)
    | ToAll => Forward unmodified AllPorts
    | GetPacket f => ActGetPkt (MkId 0)
  end.

Fixpoint desugar_actions acts :=
  match acts with
    | [] => []
    | a :: acts => (desugar_action a) :: desugar_actions acts
end.

Fixpoint desugar_pol' p pr := 
  match p with
    | Policy pred act => PoAtom (desugar_pred (And pred pr)) (desugar_actions act)
    | Par p1 p2 => PoUnion (desugar_pol' p1 pr) (desugar_pol' p2 pr)
    (* | Restrict p pr' => desugar_pol' p (And pr' pr) *)
  end.

Definition desugar_pol (p : policy) := desugar_pol' p All.
