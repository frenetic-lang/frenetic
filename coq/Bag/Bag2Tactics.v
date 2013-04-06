Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import Bag.TotalOrder.
Require Import Bag.Bag2Defs.
Require Import Bag.Bag2Lemmas.
Require Import Bag.Bag2Notations.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Ltac bag_perm n :=
  match goal with
    | |- ?bag = ?bag =>
      reflexivity
    | |- ?b <+> ?lst = ?b <+> ?lst0 =>
      apply pop_union_l;
         bag_perm (pred n)
    | |- ?b <+> ?lst1  = ?lst2 =>
      match eval compute in n with
        | O => fail "out of time / not equivalent"
        | S _ =>
          apply rotate_union;
            repeat rewrite -> union_assoc;
              bag_perm (pred n)
      end
  end.

Ltac solve_bag_permutation :=
  bag_perm 100.

Module Examples.

  Variable A : Type.
  Variable R : relation A.
  Variable O : TotalOrder R.

  Variable b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 : bag R.

  Example test_identity : b0 <+> b1 <+> b2 = b0 <+> b1 <+> b2.
  Proof.
    solve_bag_permutation.
  Qed.

  Example test_rotate : b0 <+> b1 <+> b2 = b1 <+> b2 <+> b0.
  Proof.
    solve_bag_permutation.
  Qed.

  Example test3 : b0 <+> b1 <+> b2 = b1 <+> b0 <+> b2.
  Proof.
    solve_bag_permutation.
  Qed.

  Example test4 : 
    b3 <+> b0 <+> b5 <+> b1 <+> b4 <+> b2 <+> b6 =
    b1 <+> b4 <+> b5 <+> b6 <+> b3 <+> b0 <+> b2.
  Proof.
    bag_perm 20.
  Qed.

  Example test_termination1 : False -> b0 <+> b2 = b1 <+> b2.
  Proof.
    intros.
    try solve [clear H; bag_perm 10].
    inversion H. (* "Error: No such unproven subgoal" if the line above succeeds. *)
  Qed.

  Example test_termination2 :
    b3 <+> b0 <+> b5 <+> b1 <+> b4 <+> b2 <+> b6 =
    b1 <+> b4 <+> b5 <+> b6 <+> b3 <+> b0 <+> b2.
  Proof.
    try solve [bag_perm 10].
    bag_perm 20. (* "Error: No such unproven subgoal" if the line above succeeds. *)
  Qed.

End Examples.
