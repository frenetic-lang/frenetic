(** The module NetCore is defined in OCaml, which is why this is called 
    NetCore semantics. *)
Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Classifier.Classifier.
Require Import Network.Packet.
Require Import Pattern.Pattern.
Require Import OpenFlow.MessagesDef.

Local Open Scope list_scope.

Inductive Id : Type := MkId : nat -> Id.

Inductive Action : Type :=
| Forward : pseudoPort -> Action
| ActGetPkt : Id -> Action.

Inductive Pred : Type := 
| PrHdr : pattern ->  Pred
| PrOnSwitch : switchId -> Pred
| PrOr : Pred -> Pred -> Pred
| PrNot : Pred -> Pred
| PrAll : Pred
| PrNone : Pred.

Inductive Pol : Type :=
| PoAtom : Pred -> list Action -> Pol
| PoUnion : Pol -> Pol -> Pol.

Inductive In : Type :=
| InPkt : switchId -> portId -> packet -> option bufferId -> In.

Inductive Out : Type :=
| OutPkt : switchId -> pseudoPort -> packet -> bufferId + bytes -> Out
| OutGetPkt : Id -> switchId -> portId -> packet -> Out
| OutNothing : Out.

Fixpoint match_pred (pr : Pred) (sw : switchId) (pt : portId) (pk : packet) := 
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

Definition eval_action (inp : In) (act : Action) : Out := 
  match (act, inp)  with
    | (Forward pp, InPkt sw _ pk buf) => OutPkt sw pp pk 
        (match buf with
           | Some b => inl b
           | None => inr (marshal_pkt pk)
         end)
    | (ActGetPkt x, InPkt sw pt pk buf) => OutGetPkt x sw pt pk
  end.

Fixpoint classify (p : Pol) (inp : In) := 
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