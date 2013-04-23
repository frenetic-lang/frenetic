Set Implicit Arguments.

Require Import Coq.Logic.ProofIrrelevance.
Require Import Coq.Relations.Relations.
Require Import Coq.Lists.List.
Require Import Bag.TotalOrder.
Require Import Bag.OrderedLists.

Import ListNotations.
Local Open Scope list_scope.

Record bag {A : Type} (R : relation A) : Type := Bag {
  to_list : list A;
  order : Ordered R to_list
}.

Arguments Bag {A R} to_list order.


Section Definitions.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.

  Lemma singleton_ordered : forall (x : A), Ordered R [x].
  Proof. 
    intros. 
    apply Ordered_cons. 
    intros. 
    simpl in H.
    inversion H. 
    apply Ordered_nil.
  Qed.

  Definition singleton (x : A) := Bag [x] (singleton_ordered x).

  Definition union (b1 b2 : bag R) :=
    Bag (union (to_list b1) (to_list b2)) 
        (union_order_pres Order (order b1) (order b2)).

  Definition from_list (lst : list A) :=
    Bag (from_list lst) (from_list_order Order lst).

  Lemma unions_order_pres : forall (bags : list (bag R)),
    Ordered R (unions (map (@to_list A R) bags)).
  Proof with auto.
    intros.
    apply unions_order_pres.
    intros.
    rewrite -> in_map_iff in H.
    destruct H as [bag [Heq HIn]].
    destruct bag.
    simpl in Heq.
    subst...
  Qed.

  Definition unions (bags : list (bag R)) :=
    Bag (unions (map (@to_list A R) bags)) (unions_order_pres bags).

  Definition empty := Bag [] (Ordered_nil R).

End Definitions.

(* When writing singleton sets in isolation, R will not be inferrable. But,
   this is convenient when singletons appear in a context that determines 
   R. *)
Arguments singleton [A R] x.
Arguments union [A R Order] b1 b2.
Arguments from_list [A R Order] lst.
Arguments empty [A R].
Arguments unions [A R Order] bags.
