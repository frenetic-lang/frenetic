(** The module NetCore is defined in OCaml, which is why this is called 
    NetCore semantics. *)
Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Utilities.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Classifier.Classifier.
Require Import Network.Packet.
Require Import Pattern.Pattern.
Require Import OpenFlow.MessagesDef.
Require Import NetCore.NetCoreTypes.
Require Import Bag.Bag.
Require Import Bag.BagNotation.

Local Open Scope list_scope.

Inductive id : Type := MkId : nat -> id.

Inductive act : Type :=
| Forward : pseudoPort -> act
| ActGetPkt : id -> act.

Inductive pred : Type := 
| PrHdr : pattern ->  pred
| PrOnSwitch : switchId -> pred
| PrOr : pred -> pred -> pred
| PrNot : pred -> pred
| PrAll : pred
| PrNone : pred.

Inductive pol : Type :=
| PoAtom : pred -> list act -> pol
| PoUnion : pol -> pol -> pol.

Inductive input : Type :=
| InPkt : switchId -> portId -> packet -> option bufferId -> input.

Inductive output : Type :=
| OutPkt : switchId -> pseudoPort -> packet -> bufferId + bytes -> output
| OutGetPkt : id -> switchId -> portId -> packet -> output
| OutNothing : output.

Fixpoint match_pred (pr : pred) (sw : switchId) (pt : portId) (pk : packet) := 
  match pr with
    | PrHdr pat => Pattern.match_packet pt pk pat
    | PrOnSwitch sw' => match Word64.eq_dec sw sw' with
                          | left _ => true
                          | right _ => false
                        end
    | PrOr p1 p2 => orb (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrNot p' => negb (match_pred p' sw pt pk)
    | PrAll => true
    | PrNone => false
  end.

Axiom marshal_pkt : packet -> bytes.

Extract Constant marshal_pkt => "PacketParser.marshal_packet".

Definition eval_action (inp : input) (act : act) : output := 
  match (act, inp)  with
    | (Forward pp, InPkt sw _ pk buf) => OutPkt sw pp pk 
        (match buf with
           | Some b => inl b
           | None => inr (marshal_pkt pk)
         end)
    | (ActGetPkt x, InPkt sw pt pk buf) => OutGetPkt x sw pt pk
  end.

Fixpoint classify (p : pol) (inp : input) := 
  match p with
    | PoAtom pr actions => 
      match inp with
        | InPkt sw pt pk buf => 
          if match_pred pr sw pt pk then 
            map (eval_action inp) actions 
          else nil
      end
    | PoUnion p1 p2 => classify p1 inp ++ classify p2 inp
  end.

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

Definition apply_action act (sw : switchId) (pkt : packet) :=
  match act with
    | To p => Singleton (sw, p, pkt)
    (* TODO: make ToAll, GetPacket work *)
    | _ => Empty (* (sw, p, pkt) *)
  end.

Fixpoint apply_actions acts (sw : switchId) (pkt : packet) :=
  match acts with
    | [] => Empty
    | (a :: acts) => Union (apply_action a sw pkt) (apply_actions acts sw pkt)
  end.
      

(* Generalize semantics over a base predicate language *)
Inductive produces : policy -> switchId -> portId -> packet -> bag (switchId * portId * packet) -> Prop :=
| AtomicProduces :
    forall pred acts sw p pkt,
      matches pred sw p pkt = true -> produces (Policy pred acts) sw p pkt (apply_actions acts sw pkt)
| AtomicEmptyProduces :
    forall pred acts sw p pkt,
      matches pred sw p pkt = false -> produces (Policy pred acts) sw p pkt Empty
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
      produces (Par pol1 pol2) sw p pkt (Union pkts1 pkts2).

Hint Constructors produces.

Fixpoint produce p sw pt pkt :=
  match p with
    | Policy pred acts => 
      match matches pred sw pt pkt with
        | true => apply_actions acts sw pkt
        | false => Empty
      end
    (* | Restrict p' pred => match matches pred sw pt pkt with *)
    (*                         | true => produce p' sw pt pkt *)
    (*                         | false => Bag.empty *)
    (*                       end *)
    | Par p1 p2 => Union (produce p1 sw pt pkt) (produce p2 sw pt pkt)
  end.

Lemma produce_is_produces : forall p sw pt pkt,
  produces p sw pt pkt (produce p sw pt pkt).
Proof.
  crush.
  induction p; crush.
  case_eq (matches p sw pt pkt); crush.
Qed.