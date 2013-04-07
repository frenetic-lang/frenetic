Set Implicit Arguments.

Require Import Coq.Relations.Relations.

Class TotalOrder {A : Type} (R : relation A) := {
  reflexivity : forall (x : A), R x x;
  antisymmetry : forall (x y : A), R x y -> R y x -> x = y;
  transitivity : forall (x y z : A), R x y -> R y z -> R x z;
  compare : forall (x y : A), { R x y } + { R y x };
  eqdec : forall (x y : A), { x = y } + { x <> y }
}.

Require Import Coq.Arith.Le.
Require Import Coq.Arith.Compare_dec.
Require Import Coq.Arith.Peano_dec.

Instance TotalOrder_nat : @TotalOrder nat le.
Proof with auto with arith.
  split...
  intros. eauto with arith.
  intros.
  assert ({ x <= y } + {x >= y}) as H.
  { apply le_ge_dec. }
  destruct H...
Qed.

Inductive PairOrdering 
  {A B : Type} 
  (AR : relation A) (BR : relation B) : (A * B) -> (A * B) -> Prop :=
| MkPairOrdering1 : forall a b1 b2,
  BR b1 b2 ->
  PairOrdering AR BR (a,b1) (a,b2)
| MkPairOrdering2 : forall a1 a2 b1 b2,
  AR a1 a2 ->
  a1 <> a2 ->
  PairOrdering AR BR (a1,b1) (a2,b2).

Inductive SumOrdering  {A B : Type} 
  (AR : relation A) (BR : relation B) : relation (A + B) :=
| MkSumOrdering1 : forall x y,
  SumOrdering AR BR (inl x) (inr y)
| MkSumOrdering2 : forall x y,
  AR x y ->
  SumOrdering AR BR (inl x) (inl y)
| MkSumOrdering3 : forall x y,
  BR x y ->
  SumOrdering AR BR (inr x) (inr y).

Section SumOrdering.

  Hint Constructors SumOrdering.

  Variable A B : Type.
  Variable AR : relation A.
  Variable BR : relation B.
  Variable OrdA : TotalOrder AR.
  Variable OrdB : TotalOrder BR.

  Lemma SumOrdering_reflexivity : forall x, SumOrdering AR BR x x.
  Proof with eauto.
    destruct x...
    apply MkSumOrdering2...
    apply reflexivity.
    apply MkSumOrdering3...
    apply reflexivity.
  Qed.

  Lemma SumOrdering_antisymmetry : forall x y, 
    SumOrdering AR BR x y ->
    SumOrdering AR BR y x ->
    x = y.
  Proof with eauto.
    intros.
    inversion H; subst.
    - inversion H0; subst.
    - inversion H0; subst.
      f_equal.
      apply antisymmetry...
    - inversion H0; subst.
      f_equal.
      apply antisymmetry...
  Qed.

  Lemma SumOrdering_transitivity : forall x y z,
    SumOrdering AR BR x y ->
    SumOrdering AR BR y z ->
    SumOrdering AR BR x z.
  Proof with eauto.
    intros.
    destruct x.
    destruct y.
    destruct z.
    inversion H; inversion H0; subst.
    - apply MkSumOrdering2...
      eapply transitivity...
    - apply MkSumOrdering1...
    - destruct z...
      inversion H0.
    - destruct y.
      inversion H.
      destruct z...
      inversion H0.
      eapply MkSumOrdering3...
      inversion H; subst.
      inversion H0; subst.
      eapply transitivity...
  Qed.

  Lemma SumOrdering_compare : forall x y,
    { SumOrdering AR BR x y } + { SumOrdering AR BR y x }.
  Proof with eauto.
    intros.
    destruct x.
    destruct y.
    + assert ({AR a a0} + {AR a0 a})...
      apply compare...
      destruct H...
    + left...
    + destruct y.
      - right...
      - assert ({BR b b0} + {BR b0 b})...
        apply compare...
        destruct H...
  Qed.

  Lemma SumOrdering_eqdec : forall (x y : A + B), 
    { x = y } + { x <> y }.
  Proof with eauto.
    destruct x.
    destruct y.
    decide equality. 
    + apply eqdec. 
    + apply eqdec.
    + right. unfold not. intros. inversion H.
    + destruct y.
      - right. unfold not. intros. inversion H.
      - destruct (eqdec b b0); subst...
        right. unfold not. intros. inversion H. subst. contradiction n...
  Qed.

  Instance TotalOrder_sum :  TotalOrder (SumOrdering AR BR) := {
    reflexivity := SumOrdering_reflexivity;
    antisymmetry := SumOrdering_antisymmetry;
    transitivity := SumOrdering_transitivity;
    compare := SumOrdering_compare;
    eqdec := SumOrdering_eqdec
  }.

End SumOrdering.
  
Section PairOrdering.

  Hint Constructors PairOrdering.

  Variable A B : Type.
  Variable AR : relation A.
  Variable BR : relation B.
  Variable OrdA : TotalOrder AR.
  Variable OrdB : TotalOrder BR.

  Lemma PairOrdering_reflexivity : forall x, PairOrdering AR BR x x.
  Proof with eauto.
    destruct x...
    apply MkPairOrdering1...
    apply reflexivity.
  Qed.

  Lemma PairOrdering_antisymmetry : forall x y, 
    PairOrdering AR BR x y ->
    PairOrdering AR BR y x ->
    x = y.
  Proof with eauto.
    intros.
    inversion H; subst.
    - inversion H0; subst.
      assert (b1 = b2). apply antisymmetry...
      subst...
      contradiction H7...
    - inversion H0; subst.
      contradiction H2...
      assert (a1 = a2). apply antisymmetry...
      subst.
      contradiction H2...
  Qed.

  Lemma PairOrdering_transitivity : forall x y z,
    PairOrdering AR BR x y ->
    PairOrdering AR BR y z ->
    PairOrdering AR BR x z.
  Proof with eauto.
    intros.
    destruct x.
    destruct y.
    destruct z.
    inversion H; inversion H0; subst.
    - apply MkPairOrdering1...
      eapply transitivity...
    - apply MkPairOrdering2...
    - apply MkPairOrdering2...
    - apply MkPairOrdering2...
      eapply transitivity...
      assert ({a = a1 } + { a <> a1 }). apply eqdec.
      destruct H1...
      subst.
      assert (a0 = a1). apply antisymmetry...
      subst...
  Qed.

  Lemma PairOrdering_compare : forall x y,
    { PairOrdering AR BR x y } + { PairOrdering AR BR y x }.
  Proof with eauto.
    intros.
    destruct x.
    destruct y.
    assert ({AR a a0} + {AR a0 a})...
    apply compare...
    assert ({BR b b0} + {BR b0 b})...
    apply compare...
    assert ({a = a0 } + { a <> a0 }). apply eqdec.
    destruct H1; subst...
    - destruct H0...
    - destruct H...
  Qed.

  Lemma PairOrdering_eqdec : forall (x y : A * B), 
    { x = y } + { x <> y }.
  Proof with eauto.
    destruct x.
    destruct y.
    decide equality. 
    apply eqdec. 
    apply eqdec.
  Qed.

  Instance TotalOrder_pair :  TotalOrder (PairOrdering AR BR) := {
    reflexivity := PairOrdering_reflexivity;
    antisymmetry := PairOrdering_antisymmetry;
    transitivity := PairOrdering_transitivity;
    compare := PairOrdering_compare;
    eqdec := PairOrdering_eqdec
  }.

End PairOrdering.

Existing Instances TotalOrder_pair TotalOrder_sum.

Definition inverse (A B : Type) (f : A -> B) (g : B -> A) : Prop :=
  forall x, g (f x) = x.

Inductive ProjectOrdering (A B : Type) (f : A -> B) (BR : relation B) : relation A :=
  | MkProjOrdering : forall x y,
    BR (f x) (f y) ->
    ProjectOrdering f BR x y.
  
Hint Constructors ProjectOrdering.

Lemma TotalOrder_Project : forall (A B : Type) (f : A -> B) 
   (g : B -> A)
   (RB : relation B)
   (OrdB : TotalOrder RB),
   inverse f g ->
   TotalOrder (ProjectOrdering f RB).
Proof with eauto.
  intros.
  destruct OrdB.
  split; intros.
  + eapply MkProjOrdering...
  + unfold inverse in H.
    inversion H0; inversion H1; subst...
    assert (f x = f y) as X.
    apply antisymmetry0...
    rewrite <- H.
    rewrite <- (H x).
    rewrite -> X...
  + inversion H0; inversion H1; subst...
  + destruct (compare0 (f x) (f y))...
  + destruct (eqdec0 (f x) (f y))...
    - left.
      rewrite <- H.
      rewrite <- (H x).
      rewrite -> e...
    - right.
      unfold not in *.
      intros.
      rewrite -> H0 in n.
      apply n.
      reflexivity.
Qed.
