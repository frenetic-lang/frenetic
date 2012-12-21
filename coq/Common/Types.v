Set Implicit Arguments.

Require Import Arith.Peano_dec.
Require Import Coq.Lists.List.
Open Local Scope list_scope.

Notation "[ a ; .. ; b ]" := (a :: .. (b :: nil) ..) : list_scope.

Definition Eqdec (A : Type) :=
  forall (x y : A), { x = y } + { x <> y }.

Definition second {A B C: Type} (f : B -> C) (pair : (A * B)) := 
  match pair with
    | (a,b) => (a, (f b))
  end.

(** Types with decidable equality. Doesn't this type class already exist?
 * I just can't find it... *)
Class Eq (a : Type) := {
  eqdec : forall (x y : a), { x = y } + { ~ x = y }
}.

Definition beqdec { A : Type } { E : Eq A } (x y : A) := match eqdec x y with
  | left _ => true
  | right _ => false
end.

Instance Eq_nat : Eq nat := { 
  eqdec := eq_nat_dec
}.

Lemma option_eq : forall { a : Type } { E : Eq a } (x y : option a), { x = y } + { ~ x = y }.
Proof.
  decide equality. apply eqdec.
Defined.

Instance Eq_option `(a : Type, E : Eq a) : Eq (option a) := {
  eqdec := option_eq
}.

Lemma pair_eq : forall { A B : Type } { EqA : Eq A } { EqB : Eq B }
                       (x y : A * B),
                       { x = y } + { ~ x = y }.
Proof.
  decide equality. apply eqdec. apply eqdec.
Defined.

Instance Eq_pair `(A : Type, B : Type, EA : Eq A, EB : Eq B) : Eq (A * B) := {
  eqdec := pair_eq
}. 

Reserved Notation "x == y" (at level 70, no associativity).

Notation "x == y" := (beqdec x y) : of_scope.
