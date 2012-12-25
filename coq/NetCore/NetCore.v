Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Classifier.Defs.
Require Import Network.Packet.
Require Import Pattern.Defs.
Require Import OpenFlow.MessagesDef.

Local Open Scope list_scope.

Inductive Id : Type := MkId : nat -> Id.

Inductive Action : Type :=
| Forward : pseudoPort -> Action
| ActGetPkt : Id -> Action.

Inductive Pred : Type := 
| PrHdr : Pattern ->  Pred
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

Extract Constant marshal_pkt => "fun _ -> failwith ""marshal_pkt missing""".

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

Fixpoint compile_pred (opt : Classifier bool -> Classifier bool) 
         (pr : Pred) (sw : switchId) : Classifier bool := 
  match pr with
    | PrHdr pat => [(pat, true)]
    | PrOnSwitch sw' => 
      match Word64.eq_dec sw sw' with
        | left _ => [(Pattern.all, true)]
        | right _ => [(Pattern.all, false)]
      end
    | PrOr pr1 pr2 => opt (union orb (compile_pred opt pr1 sw) 
                                 (compile_pred opt pr2 sw))
    | PrNot pr' => 
      opt (map (second negb) (compile_pred opt pr' sw ++ [(Pattern.all, false)]))
    | PrAll => [(Pattern.all, true)]
    | PrNone => nil
  end.

Definition apply_act (a : list Action) (b : bool) := 
  match b with
    | true => a
    | false => nil
  end.

Fixpoint compile_pol (opt : forall (A : Type), Classifier A -> Classifier A) (p : Pol) (sw : switchId) : Classifier (list Action) :=
  match p with
    | PoAtom pr act => 
      opt _ (map (second (apply_act act)) (compile_pred (opt bool) pr sw ++ [(Pattern.all, false)]))
    | PoUnion pol1 pol2 => 
      opt _ (union (@app Action) (compile_pol opt pol1 sw) (compile_pol opt pol2 sw))
  end.

Fixpoint strip_empty_rules (A : Type) (cf : Classifier A) : Classifier A :=
  match cf with
    | nil => nil
    | (pat, acts) :: cf => 
      if Pattern.is_empty pat
      then strip_empty_rules cf
      else (pat, acts) :: strip_empty_rules cf
  end.

Definition no_opt (A : Type) := @id (Classifier A).

Definition compile_no_opt := compile_pol no_opt.

Definition compile_opt := compile_pol ((fun A x  => @strip_empty_rules A (@elim_shadowed A x))).
