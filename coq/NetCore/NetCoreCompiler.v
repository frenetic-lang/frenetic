Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import NetCore.NetCoreSemantics.
Require Import Common.Types.
Require Import Classifier.Classifier.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
Require Import OpenFlow.MessagesDef.

Set Implicit Arguments.

Local Open Scope list_scope.

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

(** TODO(arjun): rank-2 polymorphism. The extracted code makes me nervous. *)
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