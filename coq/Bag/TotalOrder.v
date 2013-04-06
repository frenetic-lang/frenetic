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

Hint Constructors PairOrdering.

Section PairOrdering.

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

Check PairOrdering_reflexivity.

Definition ProjectOrdering (A B C : Type) (f : A -> B * C)
  (BR : relation B) (CR : relation C) (a1 : A) (a2 : A) : Prop :=
  PairOrdering BR CR (f a1) (f a2).
  
Definition inverse (A B : Type) (f : A -> B) (g : B -> A) : Prop :=
  forall x, g (f x) = x.


Lemma TotalOrder_Project : forall (A B C : Type) (f : A -> B * C) 
   (g : B * C -> A)
   (RB : relation B) (RC : relation C)
   (OrdB : TotalOrder RB)
   (OrdC : TotalOrder RC),
   inverse f g ->
   TotalOrder (ProjectOrdering f RB RC).
Proof with eauto.
  intros.
  unfold ProjectOrdering.
  split; intros.
  + apply PairOrdering_reflexivity...
  + unfold inverse in H.
    rewrite <- H.
    rewrite <- (H x).
    rewrite -> (PairOrdering_antisymmetry OrdB OrdC H0 H1)...
  + eapply PairOrdering_transitivity...
  + eapply PairOrdering_compare...
  +  unfold inverse in H.
    remember (PairOrdering_eqdec OrdB OrdC (f x) (f y)) as s.
    destruct s.
    - left.
      rewrite <- H.
      rewrite <- (H x).
      rewrite -> e...
    - right.
      unfold not in *.
      intros.
      clear Heqs.
      rewrite -> H0 in n.
      apply n.
      reflexivity.
Qed.

Extract Constant TotalOrder_nat =>
"{ compare = (fun x y -> x <= y);
   eqdec = (fun x y -> x = y) }".

Extract Constant TotalOrder_pair =>
"fun _ _ -> { compare = (fun x y -> x <= y);
              eqdec = (fun x y -> x = y) }".

