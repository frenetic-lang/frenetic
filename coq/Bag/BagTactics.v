Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Omega.
Require Import Bag.BagDef.
Require Import Bag.BagLemmas.
Require Import Bag.BagNotation.

Local Open Scope list_scope.
Local Open Scope signature_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Existing Instances Bag_Equivalence.

Ltac bag_perm n :=
  match goal with
    | |- ?bag === ?bag =>
      apply reflexivity
    | |- ?b <+> ?lst === ?b <+> ?lst0 =>
      let newn := eval compute in (depth lst) in
        apply pop_union_l;
          bag_perm newn
    | |- ?b <+> ?lst1  === ?lst2 =>
      match eval compute in n with
        | O => fail "out of time / not equivalent"
        | _ =>
          apply rotate_union;
            repeat rewrite -> union_assoc;
              bag_perm (pred n)
      end
  end.

Ltac solve_bag_permutation :=
  bag_perm 100.

Module Examples.

  Variable A : Type.
  Variable E : Eq A.

  Variable b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 : bag A.

  Example solve_bag_perm_example1 : b0 <+> b1 <+> b2 === b0 <+> b1 <+> b2.
  Proof.
    intros.
    solve_bag_permutation.
  Qed.

  Example solve_bag_perm_example2 :
    b0 <+> b1 <+> b2 === b1 <+> b2 <+> b0.
  Proof.
    intros.
    solve_bag_permutation.
  Qed.

  Example solve_bag_perm_example3 : b0 <+> b1 <+> b2 === b1 <+> b0 <+> b2.
  Proof.
    intros.
    solve_bag_permutation.
  Qed.

  Example solve_bag_perm_example4 : 
    b3 <+> b0 <+> b5 <+> b1 <+> b4 <+> b2 <+> b6 === 
    b1 <+> b4 <+> b5 <+> b6 <+> b3 <+> b0 <+> b2.
  Proof.
    intros.
    bag_perm 100.
  Qed.

  (** TODO(arjun): how to test for failure? *)
  Example solve_bag_perm_example5 :
    b0 <+> b2 === b1 <+> b2.
  Proof.
    intros.
  Admitted.

End Examples.
