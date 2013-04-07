Set Implicit Arguments.

Require Import Coq.Structures.Equalities.

Require Import Common.Types.
Require Import Wildcard.Wildcard.

Import Wildcard.

Create HintDb wildcard.

Section Lemmas.

  Variable A : Type.
  Variable eq_dec : (forall (x y : A), { x = y } + { x <> y }).

  Hint Unfold inter inter is_empty is_exact.
  Hint Constructors Wildcard.

  Ltac destruct_eq x y := 
    destruct (eq_dec x y).

  Lemma inter_all_r : forall x, inter eq_dec x WildcardAll = x.
  Proof with auto with wildcard. 
    intros. 
    destruct x... 
  Qed.

  Lemma inter_all_l : forall x, inter eq_dec WildcardAll x = x.
  Proof with auto with wildcard. 
    intros. 
    destruct x... 
  Qed.


  Lemma inter_exact_eq : forall v,
    inter eq_dec (WildcardExact v) (WildcardExact v) = WildcardExact v.
  Proof with auto.
    intros.
    autounfold.
    destruct_eq v v...
    unfold not in n.
    contradiction (n eq_refl).
  Qed.

  Lemma inter_exact_neq : forall v v',
    v <> v' ->
    inter eq_dec (WildcardExact v) (WildcardExact v') = WildcardNone.
  Proof with auto.
    intros.
    autounfold.
    destruct_eq v v'...
    contradiction.
  Qed.

  Lemma inter_none_r : forall x, 
    inter eq_dec x WildcardNone = WildcardNone.
  Proof.
    intros.
    destruct x; auto.
  Qed.

  Lemma inter_none_l : forall y, 
    inter eq_dec WildcardNone y = WildcardNone.
  Proof.
    intros.
    destruct y; auto.
  Qed.

  Hint Rewrite inter_all_l inter_all_r.
  Hint Rewrite inter_exact_neq inter_exact_eq.
  Hint Rewrite inter_none_l inter_none_r.

  Lemma inter_comm : forall x y, 
    inter eq_dec x y = inter eq_dec y x.
  Proof with auto.
    intros.
    destruct x; destruct y...
    destruct_eq a a0; subst;
      autorewrite with core using (subst;auto)...
  Qed.

  Lemma inter_assoc : forall x y z,
    inter eq_dec x (inter eq_dec y z) = inter eq_dec (inter eq_dec x y) z.
  Proof with auto.
    intros.
    destruct x; destruct y; destruct z...
    destruct_eq a a0; destruct_eq a a1; destruct_eq a0 a1; subst;
      autorewrite with core using (subst;auto)...
    destruct_eq a a0; subst; autorewrite with core using (subst;auto)...
    destruct_eq a a0; subst; autorewrite with core using (subst;auto)...
    destruct_eq a a0; subst; autorewrite with core using (subst;auto)...
    destruct_eq a a0; subst; autorewrite with core using (subst;auto)...
  Qed.

  Definition is_empty_false_distr_l : forall x y,
    is_empty (inter eq_dec x y) = false -> 
    is_empty x = false.
  Proof with auto.
    intros.
    destruct x; destruct y...
  Qed.

  Definition is_empty_false_distr_r : forall x y,
    is_empty (inter eq_dec x y) = false -> 
    is_empty y = false.
  Proof with auto.
    intros.
    destruct x; destruct y...
  Qed.

  Lemma exact_match_inter_l : forall x y,
    is_exact x = true ->
    is_empty (inter eq_dec x y) = false ->
    inter eq_dec x y = x.
  Proof with auto.
    intros.
    destruct x; simpl in H; try solve [ inversion H ].
    destruct y; simpl in H0; try solve [ inversion H0 ].
    autounfold in *.
    remember (eq_dec a a0) as H1.
    destruct H1...
    inversion H0.
    intuition.
  Qed.

  Lemma exact_match_inter_r : forall x y,
    is_exact y = true ->
    is_empty (inter eq_dec x y) = false ->
    inter eq_dec x y = y.
  Proof with auto.
    intros.
    rewrite -> inter_comm in *...
    apply exact_match_inter_l...
  Qed.

  Lemma is_empty_true_l : forall x y,
    is_empty x = true ->
    is_empty (inter eq_dec x y) = true.
  Proof with auto with wildcard.
    intros.
    destruct x...
    inversion H.
    inversion H.
    autorewrite with core using (subst;auto)...
  Qed.

  Lemma is_empty_true_r : forall x y,
    is_empty y = true ->
    is_empty (inter eq_dec x y) = true.
  Proof with auto.
    intros.
    rewrite -> inter_comm.
    apply is_empty_true_l...
  Qed.

 Lemma is_exact_split_l : 
    forall f  (x : A) (y : Wildcard A),
      inter f (WildcardExact x) y = WildcardExact x \/
      inter f (WildcardExact x) y = WildcardNone.
  Proof with auto.
    intros.
    destruct y...
    unfold inter.
    destruct (f x a)...
  Qed.

  Lemma is_exact_split_r : 
    forall f (y : Wildcard A) (x : A),
      inter f y (WildcardExact x)  = WildcardExact x \/ 
      inter f y (WildcardExact x)  = WildcardNone.
  Proof with auto.
    intros.
    destruct y...
    unfold inter.
    destruct (f a x)...
    subst...
  Qed.

End Lemmas.

Hint Rewrite inter_all_l inter_all_r : wildcard.
Hint Rewrite inter_exact_eq : wildcard.
Hint Rewrite inter_none_l inter_none_r : wildcard.
Hint Rewrite inter_assoc : wildcard.
Hint Rewrite is_empty_false_distr_l is_empty_false_distr_r : wildcard.
Hint Resolve exact_match_inter_l exact_match_inter_r : wildcard.
Hint Resolve inter_exact_neq is_empty_true_l is_empty_true_r : wildcard.
