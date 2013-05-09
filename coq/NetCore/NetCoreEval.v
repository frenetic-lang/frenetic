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

Record act : Type := Act {
  modifications : modification;
  toPorts : list pseudoPort;
  queries : list id
}.

Definition empty_action := Act unmodified nil nil.

Definition is_some {A : Type} (v : option A) : bool :=
  match v with
    | Some _ => true
    | None => false
  end.

(** TODO(arjun): does not mask most modifications (irritating to create a valid pattern) *)
Definition mod_mask (mod : modification) : pattern :=
  match mod with
    | (Modification dlSrc dlDst dlVlan dlVlanPcp
                    nwSrc nwDst nwTos
                    tpSrc tpDst) =>
      List.fold_right
        Pattern.inter Pattern.all
        [ if is_some dlSrc then Pattern.dlSrc Word48.zero else Pattern.all;
          if is_some dlDst then Pattern.dlDst Word48.zero else Pattern.all ]
  end.

Definition action_mask (a : act) : pattern :=
  mod_mask (modifications a).

(** TODO(arjun): We *will* get junk if both modifications are different. *)
Definition par_action (a1 a2 : act) : act :=
  match (a1, a2) with
    | (Act m1 p1 q1, Act m2 p2 q2) => Act m1 (p1 ++ p2) (q1 ++ q2)
  end.

Definition override {A : Type} (x y : option A) : option A :=
  match y with
    | Some _ => y
    | None => x
  end.

Definition seq_mod (m1 m2 : modification) : modification :=
  match (m1, m2) with
    | (Modification dlSrc1 dlDst1 dlVlan1 dlVlanPcp1
                    nwSrc1 nwDst1 nwTos1 tpSrc1 tpDst1,
       Modification dlSrc2 dlDst2 dlVlan2 dlVlanPcp2
                    nwSrc2 nwDst2 nwTos2 tpSrc2 tpDst2) =>
      Modification
        (override dlSrc1 dlSrc2)
        (override dlDst1 dlDst2)
        (override dlVlan1 dlVlan2)
        (override dlVlanPcp1 dlVlanPcp2)
        (override nwSrc1 nwSrc2)
        (override nwDst1 nwDst2)
        (override nwTos1 nwTos2)
        (override tpSrc1 tpSrc2)
        (override tpDst1 tpDst2)
  end.

(** NOTE: Sequencing does not modify queries, but instead produces both
    queries from a1 and a2. *)
Definition seq_action (a1 a2 : act) : act :=
  match (a1, a2) with
    | (Act m1 p1 q1, Act m2 p2 q2) => Act (seq_mod m1 m2) p2 (q1 ++ q2)
  end.
  
Inductive pred : Type := 
| PrHdr : pattern ->  pred
| PrOnSwitch : switchId -> pred
| PrOr : pred -> pred -> pred
| PrAnd : pred -> pred -> pred
| PrNot : pred -> pred
| PrAll : pred
| PrNone : pred.

Inductive pol : Type :=
| PoAtom : pred -> act -> pol
| PoUnion : pol -> pol -> pol
| PoSeq : pol -> pol -> pol.

Inductive input : Type :=
| InPkt : switchId -> portId -> packet -> option bufferId -> input.

Inductive output : Type :=
| OutPkt : switchId -> pseudoPort -> packet -> bufferId + bytes -> output
| OutGetPkt : id -> switchId -> portId -> packet -> output
| OutNothing : output.

Definition is_OutPkt outp := 
  match outp with
    | OutPkt _ _ _ _ => true
    | _ => false
  end.

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

Definition outp_to_inp outp :=
  match outp with
    (* TODO(arjun): this looks bogus. When the first policy floods, what port
       does the input packet(s) to the 2nd policy have? *)
    | OutPkt sw (PhysicalPort pt) pk (inl bufId) => Some (InPkt sw pt pk (Some bufId))
    | _ => None
  end.

Definition eval_action (inp : input) (act : act) : list output := 
  match (act, inp)  with
    | (Act mods ports queries, InPkt sw pt pk buf) => 
      map 
        (fun pt =>
           OutPkt sw pt (modify_pkt mods pk)
                  (match buf with
                     | Some b => inl b
                     | None => inr (serialize_pkt (modify_pkt mods pk))
                   end))
        ports ++
      map (fun qid => OutGetPkt qid sw pt pk) queries
  end.

Fixpoint classify (p : pol) (inp : input) := 
  match p with
    | PoAtom pr actions => 
      match inp with
        | InPkt sw pt pk buf => 
          eval_action inp
            (if match_pred pr sw pt pk then actions else empty_action)
      end
    | PoUnion p1 p2 => 
      classify p1 inp ++ classify p2 inp
    | PoSeq p1 p2 =>
      let (outPkts1, queries1) := List.partition is_OutPkt (classify p1 inp) in
      queries1 ++ (concat_map (classify p2) (filter_map outp_to_inp outPkts1))
  end.
