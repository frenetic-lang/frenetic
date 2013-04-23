Set Implicit Arguments.

Require Import Arith.Peano_dec.
Require Import Coq.Lists.List.
Open Local Scope list_scope.

(* TODO(arjun): try to Require Export ListNotations (submodule of List) instead. *)
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

Lemma list_eq : forall (A : Type) (E : Eq A) (lst1 lst2 : list A),
  { lst1 = lst2 } + { lst1 <> lst2 }.
Proof with auto.
  intros.
  decide equality.
  apply eqdec.
Qed.

Extract Constant list_eq => "(fun _ x y -> x = y)".

Instance Eq_list `(A : Type, E : Eq A) : Eq (list A) := {
  eqdec := list_eq E
}.

Reserved Notation "x == y" (at level 70, no associativity).

Notation "x == y" := (beqdec x y) : of_scope.

Section List.

Definition concat_map {A B : Type} (f : A -> list B) (lst : list A) : list B :=
  fold_right (fun a bs => f a ++ bs) nil lst.

Lemma concat_map_app : forall {A B : Type} (f : A -> list B) (l1 l2 : list A),
  concat_map f (l1 ++ l2) = (concat_map f l1) ++ (concat_map f l2).
Proof with auto.
  intros.
  induction l1.
  simpl...
  simpl.
  rewrite <- app_assoc.
  rewrite -> IHl1...
Qed.

Definition filter_map_body {A B : Type} (f : A -> option B) a bs := match f a with
  | Some b => b :: bs
  | None => bs
end.

Definition filter_map {A B : Type} (f : A -> option B) (lst : list A) :=
  fold_right (filter_map_body f) nil lst.

Lemma filter_map_app : forall (A B : Type) (f : A -> option B) (lst1 lst2 : list A),
  filter_map f (lst1 ++ lst2) = filter_map f lst1 ++ filter_map f lst2.
Proof with auto.
  intros.
  induction lst1...
  simpl.
  rewrite -> IHlst1.
  unfold filter_map_body.
  destruct (f a)...
Qed.

(** inserts [v] after each element. *)
Definition intersperse {A : Type} (v : A) (lst : list A) : list A :=
  fold_right (fun x xs => x :: v :: xs) nil lst.

Lemma nil_cons_false : forall {A : Type} (x : A) (xs : list A),
  nil = xs ++ [x] -> False.
Proof with auto.
  intros.
  destruct xs; simpl in H; inversion H.
Qed.

Hint Resolve nil_cons_false : datatypes.

Lemma cons_tail : forall {A : Type} (x y : A) (l1 l2 : list A),
  l1 ++ [x] = l2 ++ [y] -> l1 = l2 /\ x = y.
Proof with auto with datatypes.
  intros A x y l1.
  induction l1; intros...
Qed.

Hint Resolve cons_tail : datatypes.

End List.
