Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.Types.
Require Classifier.ClassifierImpl.
Require Classifier.BoolAction.
Require Import Word.WordInterface.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import Network.NetworkPacket.
Require Import NetCore.NetCoreAction.
Require Import NetCore.NetCoreEval.

Import ListNotations.
Local Open Scope list_scope.

Module Classifier := Classifier.ClassifierImpl.Make (NetCoreAction).
Module BoolAction := Classifier.BoolAction.Make (NetCoreAction.PatternSpec).
Module BoolClassifier := Classifier.ClassifierImpl.Make (BoolAction).

(** Tempting to use NetCoreAction.drop and NetCoreAction.pass as actions instead. But,
    those would create duplicate packets during parallel composition. *)
Fixpoint compile_pred (pr : pred) (sw : switchId) : BoolClassifier.t :=
  match pr with
    | PrHdr pat => [(pat, true)]
    | PrOnSwitch sw' => 
      match Word64.eq_dec sw sw' with
        | left _ => [(Pattern.all, true)]
        | right _ => []
      end
    | PrOr pr1 pr2 => BoolClassifier.union (compile_pred pr1 sw) (compile_pred pr2 sw)
    | PrAnd pr1 pr2 => BoolClassifier.sequence (compile_pred pr1 sw) (compile_pred pr2 sw)
    | PrNot pr' => map (second negb) (compile_pred pr' sw ++ [(Pattern.all, false)])
    | PrAll => [(Pattern.all, true)]
    | PrNone => [(Pattern.all, false)]
  end.

Definition maybe_action (a : NetCoreAction.t) (b : bool) := 
  match b with
    | true => a
    | false => NetCoreAction.drop
  end.

Fixpoint compile_pol  (p : pol) (sw : switchId) : Classifier.t :=
  match p with
    | PoAction action => 
      List.fold_right
        (fun e tbl => Classifier.union [(NetCoreAction.domain e, [e])] tbl)
        [(Pattern.all, NetCoreAction.drop)]
        (NetCoreAction.atoms action)
    | PoFilter pred =>
      map (second (maybe_action NetCoreAction.pass)) (compile_pred pred sw)
    | PoUnion pol1 pol2 => 
      Classifier.union (compile_pol pol1 sw) (compile_pol pol2 sw)
    | PoSeq pol1 pol2 =>
      Classifier.sequence (compile_pol pol1 sw) (compile_pol pol2 sw)
  end.
