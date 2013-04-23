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
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import OpenFlow.OpenFlow0x01Types.

Local Open Scope list_scope.

Inductive id : Type := MkId : nat -> id.

Record modification : Type := Modification {
  modifyDlSrc : option dlAddr;
  modifyDlDst : option dlAddr;
  modifyDlVlan : option (option dlVlan);
  modifyDlVlanPcp : option dlVlanPcp;
  modifyNwSrc : option nwAddr;
  modifyNwDst : option nwAddr;
  modifyNwTos : option nwTos;
  modifyTpSrc : option tpPort;
  modifyTpDst : option tpPort
}.

Definition unmodified : modification :=
 Modification None None None None None None None None None.

Inductive act : Type :=
| Forward : modification -> pseudoPort -> act
| ActGetPkt : id -> act.

Inductive pred : Type := 
| PrHdr : pattern ->  pred
| PrOnSwitch : switchId -> pred
| PrOr : pred -> pred -> pred
| PrAnd : pred -> pred -> pred
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
    | PrAnd p1 p2 => andb (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrNot p' => negb (match_pred p' sw pt pk)
    | PrAll => true
    | PrNone => false
  end.

Parameter serialize_pkt : packet -> bytes.

Extract Constant serialize_pkt => "Packet_Parser.serialize_packet".

Definition maybe_modify {A : Type} (newVal : option A) 
           (modifier : packet -> A -> packet) (pk : packet) : packet :=
  match newVal with
    | None => pk
    | Some v => modifier pk v
  end.

Definition withVlanNone maybeVlan := 
  match maybeVlan with
    | None => None
    | Some None => Some VLAN_NONE
    | Some (Some n) => Some n
  end.

Section Modification.

(* Better than Haskell, IMO. $ is not a function. *)
Local Notation "f $ x" := (f x) (at level 51, right associativity).
  (* ask me in person why I picked this level *)

Definition modify_pkt (mods : modification) (pk : packet) :=
  match mods with
    | Modification dlSrc dlDst dlVlan dlVlanPcp 
                   nwSrc nwDst nwTos
                   tpSrc tpDst =>
      maybe_modify dlSrc setDlSrc $
      maybe_modify dlDst setDlDst $
      maybe_modify (withVlanNone dlVlan) setDlVlan $
      maybe_modify dlVlanPcp setDlVlanPcp $
      maybe_modify nwSrc setNwSrc $
      maybe_modify nwDst setNwDst $
      maybe_modify nwTos setNwTos $
      maybe_modify tpSrc setTpSrc $
      maybe_modify tpDst setTpDst pk
  end.

End Modification.

Definition eval_action (inp : input) (act : act) : output := 
  match (act, inp)  with
    | (Forward mods pp, InPkt sw _ pk buf) => 
      OutPkt sw pp (modify_pkt mods pk)
             (match buf with
                | Some b => inl b
                | None => inr (serialize_pkt (modify_pkt mods pk))
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
