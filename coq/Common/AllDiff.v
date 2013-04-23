Set Implicit Arguments.

Require Import Common.Types.
Require Import Coq.Lists.List.

Local Open Scope list_scope.

Fixpoint AllDiff (A B : Type) (f : A -> B) (lst : list A) : Prop :=
  match lst with
    | nil => True
    | x :: xs => (forall (y : A), In y xs -> f x <> f y) /\ AllDiff f xs
  end.

Lemma AllDiff_uniq : forall 
  (A B : Type) (f : A -> B) (lst : list A),
  AllDiff f lst ->
  forall (x y : A),
    In x lst ->
    In y lst ->
    f x = f y ->
    x = y.
Proof with auto with datatypes.
  intros.
  induction lst.
  inversion H0.
  simpl in H.
  destruct H as [HDiffHd HDiffTl].
  simpl in H0.
  simpl in H1.
  destruct H0 as [H0 | H0]; destruct H1 as [H1 | H1]; subst...
  apply HDiffHd in H1.
  contradiction.
  apply HDiffHd in H0.
  symmetry in H2.
  contradiction.
Qed.

Lemma map_eq_inj : forall (A B : Type) (f : A -> B) (lst1 lst2 : list A) (x : A),
  map f lst1 = map f lst2 ->
  In x lst1 ->
  exists (y : A),
    In y lst2 /\ f x = f y.
Proof with auto with datatypes.
  intros.
  generalize dependent lst1.
  induction lst2; intros.
  simpl in H. destruct lst1. inversion H0. simpl in H. inversion H.
  destruct lst1.
  simpl in H. inversion H0.
  simpl in H.
  inversion H.
  simpl in H0.
  destruct H0.
  subst. exists a...
  apply IHlst2 in H0...
  destruct H0 as [y0 [HIn2 HEq]].
  exists y0...
Qed.

Lemma AllDiff_preservation : forall
  (A B : Type) (f : A -> B) (lst1 lst2 : list A),
  AllDiff f lst1 ->
  map f lst1 = map f lst2 ->
  AllDiff f lst2.
Proof with auto with datatypes.
  intros.
  generalize dependent lst1.
  induction lst2; intros.
  simpl...
  destruct lst1.
  simpl in H0. inversion H0.
  simpl in H0.
  inversion H0.
  simpl in H.
  destruct H as [HDiffHd HDiffTl].
  simpl.
  split.
  intros.
  apply map_eq_inj with (f := f) (lst2 := lst1) in H...
  destruct H as [y0 [HIn2 HEqyy0]].
  apply HDiffHd in HIn2.
  rewrite -> HEqyy0.
  rewrite <- H2.
  trivial.
  apply IHlst2 in H3...
Qed.
