Set Implicit Arguments.

Require Import Common.Types.
Require Import Coq.Lists.List.

Local Open Scope list_scope.

Print Coq.Lists.List.

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