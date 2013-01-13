Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.EquivDec.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Omega.
Require Import Bag.BagDef.
Require Import Bag.BagNotation.

Local Open Scope list_scope.
Local Open Scope signature_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

Existing Instances Bag_Equivalence.

Section Methods.

  Variable A : Type.
  Variable R : relation A.
  Variable E : Equivalence R.
  Variable ED : EqDec A R.
    
  Fixpoint to_list (bag : bag A) :=
    match bag with
      | Empty => nil
      | Singleton x => [x]
      | Union bag1 bag2 => to_list bag1 ++ to_list bag2
      | FromList lst => lst
    end.

  Lemma Mem_equiv : forall (x : A) (b1 b2 : bag A),
    b1 === b2 ->
    Mem x b1 ->
    Mem x b2.
  Proof with auto.
  Admitted.

  Lemma union_iff : forall (x : A) (b1 b2 : bag A),
    multiplicity x (Union b1 b2) = multiplicity x b1 + multiplicity x b2.
  Proof with auto.
    reflexivity.
  Qed.

  Instance union_m : 
    Proper (Bag_equiv ED ==> Bag_equiv ED ==> Bag_equiv ED) Union.
  Proof with auto.
    unfold Proper.
    unfold respectful.
    unfold Bag_equiv.
    intros.
    rewrite union_iff.
    rewrite union_iff...
  Qed.

  Add Morphism Union with signature 
    Bag_equiv ED ++> Bag_equiv ED ++> Bag_equiv ED as union_s_m.
    apply union_m.
  Qed.

  Lemma union_comm : forall x y, Union x y === Union y x.
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    omega.
  Qed.

  Lemma union_assoc : forall x y z,  
    Union (Union x y) z === Union x (Union y z).
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    omega.
  Qed.
    
  Lemma union_empty_l : forall x, Union Empty x === x.
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    reflexivity.
  Qed.

  Lemma union_empty_r : forall x, Union x Empty === x.
  Proof.
    intros.
    rewrite union_comm. apply union_empty_l.
  Qed.

  Definition unions (bags : list (bag A)) : bag A :=
    fold_right Union Empty bags.

  Lemma bag_unions_app : forall (lst lst0 : list (bag A)),
    unions (lst ++ lst0) === Union (unions lst) (unions lst0).
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    induction lst...
    simpl.
    simpl in IHlst.
    omega.
  Qed.
  
  Lemma pop_union_l : forall (b b0 b1: bag A),
    b0 === b1 ->
    Union b b0 === Union b b1.
  Proof.
    intros.
    simpl.
    rewrite -> H.
    apply reflexivity.
  Qed.

  Lemma pop_union_r : forall (b b0 b1: bag A),
    b0 === b1 ->
    Union b0 b === Union b1 b.
  Proof.
    intros.
    do 2 rewrite -> (union_comm _ b).
    apply pop_union_l.
    trivial.
  Qed.

  Lemma equiv_singleton : forall (x y : A),
    x === y ->
    Singleton x === Singleton y.
  Proof with auto.
    intros.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    destruct (equiv_dec e x); destruct (equiv_dec e y)...
    rewrite <- e0 in H.
    contradiction.
    rewrite <- e0 in H.
    apply symmetry in H.
    contradiction.
  Qed.
  
  Fixpoint depth (b : bag A) :=
    match b with
      | Singleton _ => 1
      | Empty => 1
      | FromList _ => 1
      | Union l r => max (depth l) (depth r)
    end.

  Lemma rotate_union : forall (b b0 b1 : bag A),
    Union b b0 === b1 ->
    Union b0 b === b1.
  Proof with auto.
    intros.
    rewrite -> union_comm.
    exact H.
  Qed.

  Lemma unions_app : forall (lst lst0 : list (bag A)),
    unions (lst ++ lst0) === Union (unions lst) (unions lst0).
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    induction lst...
    simpl in *.
    omega.
  Qed.

  Lemma from_list_cons : forall x xs,
    FromList (x :: xs) === Union (Singleton x) (FromList xs).
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    destruct (equiv_dec e x)...
  Qed.

  Lemma FromList_app : forall lst1 lst2,
    FromList (lst1 ++ lst2) === Union (FromList lst1) (FromList lst2).
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    simpl.
    induction lst1...
    simpl in *.
    destruct (equiv_dec e a); subst...
    omega.
  Qed.

  Lemma FromList_nil_is_Empty : FromList nil === Empty.
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros...
  Qed.

  Axiom Mem_unions : forall (x : A) lst, 
    Mem x (unions lst) ->
    exists elt, In elt lst /\ Mem x elt.

  Lemma unpop_unions : 
    forall (b b0 b1 : bag A),
      b <+> b0 === b <+> b1 ->
      b0 === b1.
  Proof.
    intros.
    unfold Equivalence.equiv in *.
  Admitted.
  
  Axiom to_list_singleton : forall (x : A),
    to_list (Singleton x) === [x].
  
  Axiom from_list_singleton : forall (x : A),
    FromList [x] === Singleton x.

  Axiom mem_in_to_list : forall  (x : A) (bag : bag A),
    In x (to_list bag) -> Mem x bag.

  Axiom mem_split : forall (x : A) (b : bag A),
    Mem x b ->
    exists b1, b === ({| x |}) <+> b1.

  Axiom mem_union : forall (x : A) (b1 b2 : bag A),
    Mem x (b1 <+> b2) <-> Mem x b1 \/ Mem x b2.

  Axiom mem_equiv : forall (x : A) (b1 b2 : bag A),
    Mem x b1 ->
    b1 === b2 ->
    exists (y : A), Mem y b2 /\ x === y.

  Axiom in_map_mem : forall (B : Type) (f : A -> B) (y : B) (b : bag A),
    In y (map f (to_list b)) ->
    exists (x : A), Mem x b /\ y = f x.

End Methods.

Section BinaryMethods.

  Variable A : Type.
  Variable B : Type.
  Variable RA : relation A.
  Variable RB : relation B.
  Variable EA : Equivalence RA.
  Variable EB : Equivalence RB.
  Variable EDA : EqDec A RA.
  Variable EDB : EqDec B RB.

  Lemma map_union : forall (f : A -> B) (bag1 bag2 : bag A),
    FromList (map f (to_list (Union bag1 bag2))) ===
      (Union (FromList (map f (to_list bag1))) 
        (FromList (map f (to_list bag2)))).
  Proof with auto.
    intros.
    unfold Equivalence.equiv.
  Admitted.

  Lemma unions_unlist : forall (f : A -> bag B) (lst : list A) (bag : bag A),
    unions (map f (to_list (FromList lst <+>  bag))) ===
    unions (List.map f lst) <+> unions (List.map f (to_list bag)).
  Proof with auto.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    induction lst...
    simpl.
    simpl in IHlst.
    rewrite -> IHlst.
    omega.
  Qed.

  Lemma unions_unlist_2 : forall (f : A -> bag B) 
    (lst : list A) (bag : bag A),
    (unions (map f (lst ++ to_list bag))) ===
      (Union
        (unions (map f lst)) 
        (unions (map f (to_list bag)))).
  Proof with auto with datatypes.
    unfold Equivalence.equiv.
    unfold Bag_equiv.
    intros.
    induction lst...
    simpl in *.
    omega.
  Qed.

  Lemma FromList_map_iff : forall
    (f : A -> B) (bag1 bag2 : bag A),
    bag1 === bag2 ->
    FromList (map f (to_list bag1)) === FromList (map f (to_list bag2)).
  Proof with auto.
    intros.
    unfold Equivalence.equiv in *.
    unfold Bag_equiv in *.
    intros.
  Admitted.

  Lemma unions_iff : forall 
    (f : A -> bag B) (bag1 bag2 : bag A),
    bag1 === bag2 ->
    unions (map f (to_list bag1)) === unions (map f (to_list bag2)).
  Proof with auto.
  Admitted.

  Axiom mem_unions_map : forall (b : B) (lst: list A) (f : A -> bag B),
    Mem b (unions (map f lst)) ->
    exists (a : A), In a lst /\ Mem b (f a).

End BinaryMethods.


