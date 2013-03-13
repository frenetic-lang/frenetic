Set Implicit Arguments.

Require Import Coq.Relations.Relations.

Class TotalOrder {A : Type} (R : relation A) := {
  reflexivity : forall (x : A), R x x;
  antisymmetry : forall (x y : A), R x y -> R y x -> x = y;
  transitivity : forall (x y z : A), R x y -> R y z -> R x z;
  compare : forall (x y : A), { R x y } + { R y x }
}.
