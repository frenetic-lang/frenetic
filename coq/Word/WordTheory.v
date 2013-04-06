Set Implicit Arguments.

Require Import Coq.Logic.ProofIrrelevance.
Require Import Coq.Structures.Equalities.
Require Import PArith.BinPos.
Require Import NArith.BinNat.
Require Import Bag.TotalOrder.
Require Import Word.WordInterface.

Local Open Scope N_scope.

Module Make (Import W : MAKEWORD).

  Module Export Word := W.

  Lemma eq_dec : forall (m n : t), { m = n } + { m <> n }.
  Proof.
    intros.
    destruct m, n.
    assert ({v = v0} + {v <> v0}) as J.
    { apply N.eq_dec. }
    destruct J; subst.
    + left.
      f_equal.
      apply proof_irrelevance.
    + right.
      unfold not.
      intros.
      inversion H; subst.
      contradiction n.
      reflexivity.
  Qed.

  Definition le (x y : t) : Prop :=
    match (x, y) with
      | (Mk m _, Mk n _) => m <= n
    end.

  Lemma le_reflexivity : forall (x : t), le x x.
  Proof with auto.
    intros.
    destruct x...
    apply N.le_refl.
  Qed.

  Lemma le_antisymmetry : forall (x y : t), le x y -> le y x -> x = y.
  Proof with eauto.
    intros.
    destruct x, y.
    unfold le in *.
    apply N.lt_eq_cases in H.
    apply N.lt_eq_cases in H0.
    destruct H, H0...
    + assert (v < v). eapply N.lt_trans...
      apply N.lt_irrefl in H1. inversion H1.
    + subst...
      assert (l = l0). apply proof_irrelevance.
      subst...
    + subst...
      assert (l = l0). apply proof_irrelevance.
      subst...
    + clear H0; subst.
      assert (l = l0). apply proof_irrelevance.
      subst...
  Qed.

  Lemma le_transitivity : forall (x y z : t), le x y -> le y z -> le x z.
  Proof with eauto.
    intros.
    destruct x, y, z.
    unfold le in *.
    eapply N.le_trans...
  Qed.

  Lemma le_compare : forall (x y : t), { le x y } + { le y x }.
  Proof with eauto.
    intros.
    destruct x, y.
    unfold le in *.
    remember (N.compare v v0) as cmp eqn:J.
    symmetry in J.
    destruct cmp.
    + apply N.compare_eq_iff in J.
      subst.
      left...
      apply N.lt_eq_cases...
    + rewrite ->  N.compare_lt_iff in J.
      left.
      apply N.lt_eq_cases...
    + rewrite ->  N.compare_gt_iff in J.
      right.
      apply N.lt_eq_cases...
  Qed.
 
  Instance TotalOrder :  TotalOrder le := {
    reflexivity := le_reflexivity;
    antisymmetry := le_antisymmetry;
    transitivity := le_transitivity;
    compare := le_compare;
    eqdec := eq_dec
  }.

End Make.

Module Word8 := Make (Word8).
Module Word12 := Make (Word12).
Module Word16 := Make (Word16).
Module Word32 := Make (Word32).
Module Word48 := Make (Word48).
Module Word64 := Make (Word64).

Existing Instances Word8.TotalOrder Word16.TotalOrder Word12.TotalOrder
  Word32.TotalOrder Word48.TotalOrder Word64.TotalOrder.
