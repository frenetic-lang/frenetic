Set Implicit Arguments.

Require Import Coq.Logic.ProofIrrelevance.
Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import Bag.TotalOrder.
Require Import Bag.OrderedLists.
Require Import Bag.Bag2Defs.
Require Import Bag.Bag2Notations.

Import ListNotations.
Local Open Scope list_scope.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module OL := Bag.OrderedLists.

Section Methods.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.

  Lemma ordered_irr: forall (b : bag R) (lst : list A) (o : Ordered R lst),
    to_list b = lst ->
    b = Bag lst o.
  Proof with auto.
    intros.
    destruct b.
    simpl in *.
    subst.
    assert (o = order).
      apply proof_irrelevance.
    subst...
  Qed.

  Hint Resolve ordered_irr.

  Lemma union_comm : forall b1 b2, b1 <+> b2 = b2 <+> b1.
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply union_comm...
    destruct b1...
    destruct b2...
  Qed.

  Lemma union_assoc : forall x y z, (x <+> y) <+> z = x <+> (y <+> z).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    symmetry.
    apply union_assoc...
    destruct x...
    destruct y...
    destruct z...
  Qed.
    
  Lemma union_empty_l : forall x, empty <+> x = x.
  Proof with auto.
    intros.
    destruct x.
    apply ordered_irr.
    simpl.
    apply union_nil_l...
  Qed.

  Lemma union_empty_r : forall x, x <+> empty = x.
  Proof.
    intros.
    destruct x; auto.
  Qed.

  Lemma unions_app : forall (lst lst0 : list (bag R)),
    unions  (lst ++ lst0) = unions lst <+> unions lst0.
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    induction lst...
    simpl.
    rewrite -> union_nil_l...
    apply unions_order_pres.
    simpl.
    rewrite <- OL.union_assoc...
    rewrite -> IHlst...
    destruct a...
    apply unions_order_pres...
    apply unions_order_pres...
  Qed.

  Lemma pop_union_l : forall (b b0 b1: bag R),
    b0 = b1 ->
    b <+> b0 = b <+> b1.
  Proof.
    intros. subst. reflexivity.
  Qed.

  Lemma pop_union_r : forall (b b0 b1: bag R),
    b0 = b1 ->
    b0 <+> b = b1 <+> b.
  Proof.
    intros. subst. reflexivity.
  Qed.

  Lemma rotate_union : forall (b b0 b1 : bag R),
    union b b0 = b1 ->
    union b0 b = b1.
  Proof.
    intros. subst. apply union_comm.
  Qed.

  Lemma from_list_cons : forall x xs,
    from_list (x :: xs) = ({| x |}) <+> (from_list xs).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply from_list_cons.
  Qed.

  Lemma from_list_app : forall lst1 lst2,
    from_list (lst1 ++ lst2) = union (from_list lst1) (from_list lst2).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply from_list_app.
  Qed.

  Lemma from_list_nil_is_empty : from_list nil = empty.
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl...
  Qed.

End Methods.

Section BinaryMethods.

  Variable A B: Type.
  Variable RA : relation A.
  Variable RB : relation B.
  Variable AOrder : TotalOrder RA.
  Variable BORder : TotalOrder RB.

  Lemma map_union : forall (f : A -> B) (bag1 bag2 : bag RA),
    from_list (map f (to_list (union bag1 bag2))) =
      (union (from_list (map f (to_list bag1))) 
        (from_list (map f (to_list bag2)))).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply map_union...
    destruct bag1...
    destruct bag2...
  Qed.


  Lemma in_unions_map : forall (b : B) (lst: list A) (f : A -> bag RB),
   In b (to_list (unions (map f lst))) ->
    exists (a : A), In a lst /\ In b (to_list (f a)).
  Proof with eauto with datatypes.
    intros.
    induction lst.
    + simpl in H. inversion H.
    + simpl in H.
      apply In_union in H.
      destruct H...
      apply IHlst in H.
      clear IHlst.
      destruct H as [a0 [Ha0In HbIn]].
      exists a0...
      destruct (f a)...
      apply unions_order_pres...
  Qed.

End BinaryMethods.


