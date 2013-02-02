Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import NetCore.NetCoreSemantics.
Require Import Common.Types.
Require Import Classifier.Classifier.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.
(* TODO: MJR Move 'switchId' from messagesDef so that we don't have to include this whole thing *)
Require Import OpenFlow.MessagesDef.
(* Require Import NetCore.NetCoreTypes. *)

Set Implicit Arguments.

Local Open Scope list_scope.

(* Fixpoint desugar_pred (p : predicate) := match p with *)
(*   | DlSrc eth => PrHdr (Pattern.dlSrc eth) *)
(*   | DlDst eth => PrHdr (Pattern.dlDst eth) *)
(*   (* | DlTyp typ => PrHdr (Pattern.dlType typ) *) *)
(*   (* | DlVlan (Some vlan) => PrHdr (Pattern.dlVlan vlan) *) *)
(*   (* | DlVlan None => PrHdr (Pattern.dlVlan VLAN_NONE) *) *)
(*   (* | NwProto proto => PrHdr (Pattern.nwProto proto) *) *)
(*   | Switch sw => PrOnSwitch sw *)
(*   | InPort pt => PrHdr (Pattern.inPort pt) *)
(*   | And p1 p2 =>  *)
(*       (* de Morgan's law *) *)
(*       PrNot (PrOr (PrNot (desugar_pred p1)) (PrNot (desugar_pred p2))) *)
(*   | Or p1 p2 => PrOr (desugar_pred p1) (desugar_pred p2) *)
(*   | Not p => PrNot (desugar_pred p) *)
(*   | All => PrAll *)
(*   | NoPackets => PrNone *)
(* end. *)

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