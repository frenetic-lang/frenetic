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

Instance TotalOrder_pair `(A : Type, B : Type,
                           AR : relation A, BR : relation B,
                           OrdA : TotalOrder A AR, 
                           OrdB : TotalOrder B BR) :
  TotalOrder (PairOrdering AR BR).
Proof with eauto.
  split; intros...
  + destruct x...
    apply MkPairOrdering1...
    apply reflexivity.
  + inversion H; subst.
    - inversion H0; subst.
      assert (b1 = b2). apply antisymmetry...
      subst...
      contradiction H7...
    - inversion H0; subst.
      contradiction H2...
      assert (a1 = a2). apply antisymmetry...
      subst.
      contradiction H2...
  + destruct x.
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
  + destruct x.
    destruct y.
    assert ({AR a a0} + {AR a0 a})...
    apply compare...
    assert ({BR b b0} + {BR b0 b})...
    apply compare...
    assert ({a = a0 } + { a <> a0 }). apply eqdec.
    destruct H1; subst...
    - destruct H0...
    - destruct H...
  + destruct x.
    destruct y.
    decide equality. 
    apply eqdec. 
    apply eqdec.
Qed.
