Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import NetCore.NetCoreEval.
Require Import Common.Types.
Require Import Classifier.Classifier.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
(* TODO: MJR Move 'switchId' from messagesDef so that we don't have to include this whole thing *)
Require Import OpenFlow.OpenFlow0x01Types.
Require Import Network.NetworkPacket.
Require Import NetCore.NetCoreAction.

Import ListNotations.
Local Open Scope list_scope.

Fixpoint compile_pred (opt : Classifier bool -> Classifier bool) 
         (pr : pred) (sw : switchId) : Classifier bool := 
  match pr with
    | PrHdr pat => [(pat, true)]
    | PrOnSwitch sw' => 
      match Word64.eq_dec sw sw' with
        | left _ => [(Pattern.all, true)]
        | right _ => []
      end
    | PrOr pr1 pr2 => opt (union orb (compile_pred opt pr1 sw) 
                                 (compile_pred opt pr2 sw))
    | PrAnd pr1 pr2 => opt (inter andb (compile_pred opt pr1 sw)
                                  (compile_pred opt pr2 sw))
    | PrNot pr' => 
      opt (map (second negb) 
               (compile_pred opt pr' sw ++ [(Pattern.all, false)]))
    | PrAll => [(Pattern.all, true)]
    | PrNone => [(Pattern.all, false)]
  end.

Definition maybe_action (a : NetCoreAction.t) (b : bool) := 
  match b with
    | true => a
    | false => NetCoreAction.zero
  end.


(** TODO(arjun): rank-2 polymorphism. The extracted code makes me nervous. *)
Fixpoint compile_pol 
  (opt : forall (A : Type), Classifier A -> Classifier A) 
  (p : pol) (sw : switchId) : Classifier NetCoreAction.t :=
  match p with
    | PoAction action =>
      opt _ (NetCoreAction.compile action)
    | PoFilter pred =>
      opt _ (map (second (maybe_action NetCoreAction.one)) 
                 (compile_pred (opt bool) pred sw ++ [(Pattern.all, false)]))
    | PoUnion pol1 pol2 => 
      opt _ (union NetCoreAction.par_action
                   (compile_pol opt pol1 sw) 
                   (compile_pol opt pol2 sw))
    | PoSeq pol1 pol2 =>
      opt _ (sequence 
               NetCoreAction.zero
               NetCoreAction.par_action
               NetCoreAction.seq_action 
               NetCoreAction.mask_pat
               NetCoreAction.atoms
               (compile_pol opt pol1 sw) 
               (compile_pol opt pol2 sw))
  end.

Fixpoint strip_empty_rules (A : Type) (cf : Classifier A) : Classifier A :=
  match cf with
    | nil => nil
    | (pat, acts) :: cf => 
      if Pattern.is_empty pat
      then strip_empty_rules cf
      else (pat, acts) :: strip_empty_rules cf
  end.

Definition no_opt (A : Type) := @Datatypes.id (Classifier A).

Definition compile_no_opt := compile_pol no_opt.

Definition compile_opt := 
  compile_pol ((fun A x  => @strip_empty_rules A (@elim_shadowed A x))).
