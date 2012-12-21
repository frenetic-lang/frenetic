Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.

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
