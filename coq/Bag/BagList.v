Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Coq.Structures.Equalities.
Require Import Common.Types.
Require Import Bag.BagInterface.
Require Import Omega.

Local Open Scope list_scope.
Local Open Scope signature_scope.
Local Open Scope equiv_scope.

Module Make (Elt : MiniDecidableType) : BAG.

  Definition elt := Elt.t.

  Definition t := list elt.

  Definition multiplicity (x : elt) (bag : t) := 
      List.fold_right (fun y sum =>
        match Elt.eq_dec x y with
          | left _ => S sum
          | right _ => sum
        end) 0 bag.

  Definition bag_equiv (b b0 : t) :=
    forall elt, multiplicity elt b = multiplicity elt b0.

  Hint Unfold bag_equiv.

  Lemma  bag_equiv_is_Equivalence : Equivalence bag_equiv.
  Proof with auto.
    split.
    unfold Reflexive...
    unfold Symmetric...
    unfold Transitive. autounfold. intros. rewrite -> H...
  Qed.

  Instance Bag_Equivalence : Equivalence bag_equiv.
  Proof. 
    exact bag_equiv_is_Equivalence.
  Qed.

  Definition empty : t := nil.

  Definition singleton (e : elt) : t := [e].

  Definition union (x y : t) := x ++ y.

  Lemma empty_spec : forall e, multiplicity e empty = 0.
  Proof. 
    reflexivity. 
  Qed.

  Lemma singleton_spec₁_1  : forall e, multiplicity e (singleton e) = 1.
  Proof with auto.
    intros.
    simpl.
    destruct (Elt.eq_dec e e)... 
    contradiction n...
  Qed.

  Lemma singleton_spec_2 : forall e e0,
    e <> e0 ->
    multiplicity e (singleton e0) = 0.
  Proof with auto.
    intros.
    simpl...
    destruct (Elt.eq_dec e e0)...
    contradiction H...
  Qed.

  Lemma union_spec : forall e x1 x2,
    multiplicity e (union x1 x2) = multiplicity e x1 + multiplicity e x2.
  Proof with auto.
    intros.
    unfold union.
    induction x1...
    simpl...
    destruct (Elt.eq_dec e a)...
    omega.
  Qed.

  Definition  to_list (x : t) : list elt := x.

  Definition from_list (lst : list elt) : t := lst.

  Lemma to_list_spec : forall e, from_list (to_list e) === e.
  Proof with auto.
    intros.
    unfold to_list.
    unfold from_list.
    apply reflexivity.
  Qed.

End Make.
