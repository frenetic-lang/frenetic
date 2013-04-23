(** An in-progress surface semantics for NetCore. The rest of the Coq
    development uses NetCore's primitive syntax and relies on desugaring
    in OCaml. *)
Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Utilities.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Classifier.Classifier.
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreTypes.
Require Import Bag.Bag2.
Require Import NetCore.NetCoreEval.
Require Import NetCore.NetCoreDesugar.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Ltac destruct_eq_dec :=
  match goal with
    | [ |- context [eqdec ?A ?B] ] => destruct (eqdec A B)
    | [ H : context [eqdec ?A ?B] |- _ ] => destruct (eqdec A B)
  end.

Lemma Is_true_eqb_eq : forall {A} `{Eq A} x y,
  Is_true (beqdec x y) <-> (x = y).
Proof.
  crush; unfold beqdec in *; destruct_eq_dec; crush.
Qed.

Hint Rewrite Is_true_eqb_eq.

Notation "x ==b y" := (beqdec x y).

Instance Eq_switchId : Eq switchId. 
Proof.
  split.
  exact Word64.eq_dec.
Qed.

Instance Eq_portId : Eq portId. 
Proof.
  split.
  exact Word16.eq_dec.
Qed.

Instance Eq_dlAddr : Eq dlAddr. 
Proof.
  split.
  exact Word48.eq_dec.
Qed.

Fixpoint matches pred sw p pk :=
  match (pred, pk) with
    | (DlSrc addr, Packet addr1 addr2 typ vlan vlanPcp proto) => addr ==b addr1
    | (DlDst addr, Packet addr1 addr2 typ vlan vlanPcp proto) => addr ==b addr2
    (* | (DlTyp typ', Packet addr1 addr2 typ vlan vlanPcp proto) => typ' ==b typ *)
    (* | (DlVlan (Some vlan'), MkPacket _ _ _ vlan vlanPcp _) => vlan' ==b vlan *)
    (* | (DlVlan None, MkPacket _ _ _ vlan vlanPcp _) => vlan ==b VLAN_NONE *)
      (* ATM we only model IP packets *)
    (* | (NwProto proto, MkPacket _ _ _ _ vlanPcp (MkIPHeader _ _ _ _ _ _ _ _ _)) => proto ==b PROTO_IP *)
    | (Switch sw', _) => sw' ==b sw
    | (InPort p', _) => p' ==b p
    | (And pr1 pr2, pk) => (matches pr1 sw p pk) && (matches pr2 sw p pk)
    | (Or pr1 pr2, pk) => (matches pr1 sw p pk) || (matches pr2 sw p pk)
    | (Not pr1, pk) => negb (matches pr1 sw p pk)
    | (All, _) => true
    | (PredNone, _) => false
  end.

(* This stuff comes with FwOF.FwOFSignatures.FwOFMachine (i.e., functorize) *)

Require Import Bag.TotalOrder.
Parameter packet_le : Relation_Definitions.relation packet.
Parameter switchId_le : Relation_Definitions.relation switchId.
Parameter portId_le : Relation_Definitions.relation portId.

Definition swPtPks : Type :=
  bag (PairOrdering (PairOrdering switchId_le portId_le)
                    packet_le).
Parameter Instance TotalOrder_swPtPks : TotalOrder
(PairOrdering (PairOrdering switchId_le portId_le)
                    packet_le).

(* *** end FwOF stuff needed for total-order on located packets *** *)

Definition apply_action act (sw : switchId) (pkt : packet) : swPtPks :=
  match act with
    | To p => singleton (sw, p, pkt)
    (* TODO: make ToAll, GetPacket work *)
    | _ => {| |} (* (sw, p, pkt) *)
  end.

Fixpoint apply_actions acts (sw : switchId) (pkt : packet) :=
  match acts with
    | [] => empty
    | (a :: acts) => union (apply_action a sw pkt) (apply_actions acts sw pkt)
  end.
      

(* Generalize semantics over a base predicate language *)
Inductive produces : policy -> switchId -> portId -> packet -> swPtPks -> Prop :=
| AtomicProduces :
    forall pred acts sw p pkt,
      matches pred sw p pkt = true -> produces (Policy pred acts) sw p pkt (apply_actions acts sw pkt)
| AtomicEmptyProduces :
    forall pred acts sw p pkt,
      matches pred sw p pkt = false -> produces (Policy pred acts) sw p pkt empty
(* | RestrictProduces : *)
(*     forall pol pred sw p pkt pkts, *)
(*       matches pred sw p pkt = true ->  *)
(*       produces pol sw p pkt pkts ->  *)
(*       produces (Restrict pol pred) sw p pkt pkts *)
(* | RestrictEmptyProduces : *)
(*     forall pol pred sw p pkt, *)
(*       matches pred sw p pkt = false ->  *)
(*       produces (Restrict pol pred) sw p pkt Bag.empty *)
| UnionProduces :
    forall pol1 pol2 sw p pkt pkts1 pkts2,
      produces pol1 sw p pkt pkts1 -> 
      produces pol2 sw p pkt pkts2 -> 
      produces (Par pol1 pol2) sw p pkt (union pkts1 pkts2).

Hint Constructors produces.

Fixpoint produce p sw pt pkt :=
  match p with
    | Policy pred acts => 
      match matches pred sw pt pkt with
        | true => apply_actions acts sw pkt
        | false => empty
      end
    (* | Restrict p' pred => match matches pred sw pt pkt with *)
    (*                         | true => produce p' sw pt pkt *)
    (*                         | false => Bag.empty *)
    (*                       end *)
    | Par p1 p2 => union (produce p1 sw pt pkt) (produce p2 sw pt pkt)
  end.

Lemma produce_is_produces : forall p sw pt pkt,
  produces p sw pt pkt (produce p sw pt pkt).
Proof.
  crush.
  induction p; crush.
  case_eq (matches p sw pt pkt); crush.
Qed.
