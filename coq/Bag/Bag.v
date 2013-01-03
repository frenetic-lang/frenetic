Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.

Local Open Scope list_scope.
Local Open Scope signature_scope.
Local Open Scope equiv_scope.

(* TODO(arjun): I'm not paying attention to levels and associativity. Fix. *)
Reserved Notation "{| x |}" (at level 70, no associativity).
Reserved Notation "{| |}" (at level 70, no associativity).
Reserved Notation "x <+> y" (at level 69, right associativity).


Module Type BAG.

  Parameter Bag : Type -> Type.

  Parameter empty : forall {A : Type}, Bag A.

  Parameter Bag_equiv : forall {A : Type} {E : Eq A}, Bag A -> Bag A -> Prop.

  Section Methods.
  
  Variable A : Type.
  Variable E : Eq A.

  Parameter singleton : A -> Bag A.
  Parameter add : A -> Bag A -> Bag A.
  Parameter Bag_equiv_is_Equivalence : Equivalence Bag_equiv.
  Parameter map : forall {B : Type} {E' : Eq B}, (A -> B) -> Bag A -> Bag B.

  Parameter union : Bag A -> Bag A -> Bag A.
  Parameter from_list : list A -> Bag A.
  Parameter to_list : Bag A -> list A.

  Parameter union_m : Proper (Bag_equiv ==> Bag_equiv ==> Bag_equiv) union.

  Parameter from_list_cons : forall x xs bag,
    from_list (x :: xs) === bag ->
    union (singleton x) (from_list xs) === bag.

  Add Morphism union with signature Bag_equiv ++> Bag_equiv ++> Bag_equiv as union_s_m.
    apply union_m.
  Qed.

  Parameter union_comm : forall x y, union x y === union y x.

  Parameter union_assoc : forall x y z,  union (union x y) z === union x (union y z).

  Parameter from_list_comm_union : forall l1 l2,
    from_list (l1 ++ l2) === union (from_list l1) (from_list l2).

  Parameter from_list_nil : from_list nil === empty.
  Parameter union_empty_l : forall x, union empty x === x.
  Parameter union_empty_r : forall x, union x empty === x.

  Parameter to_list_singleton : forall x, to_list (singleton x) = [x].

  Parameter nil_list : to_list empty = nil.
  Parameter nil_bag : from_list nil = empty.

  Parameter size : Bag A -> nat.
  Parameter size_equiv : forall x y, x === y -> size x = size y.
  Parameter size_comm_union : forall x y, size (union x y) = size x + size y.
  Parameter size_to_list : forall x, size x = length (to_list x).

  Parameter in_comm_union : forall x bag1 bag2, 
    In x (to_list (union bag1 bag2)) ->
    In x (to_list bag1) \/ In x (to_list bag2).

  End Methods.

  Parameter concat_map_union : forall {A B : Type} {EA : Eq A} {EB : Eq B} (f : A -> list B)
    (bag1 bag2 : Bag A),
      (from_list (concat_map f (to_list (union bag1 bag2)))) ===
      (union (from_list (concat_map f (to_list bag1))) 
             (from_list (concat_map f (to_list bag2)))).

End BAG.

Module Bag : BAG.

  Inductive T (A : Type) := 
    | MkBag : list A -> T A.
  Definition Bag := T.

  Definition empty {A : Type} := MkBag (@nil A).

  Section Methods.

    Variable A : Type.
    Variable E : Eq A.

    Definition to_list (bag : Bag A) : list A := match bag with
      | MkBag lst => lst
    end.

    Definition multiplicity (x : A) (bag : Bag A) := 
      List.fold_right (fun y sum =>
        match eqdec x y with
          | left _ => S sum
          | right _ => sum
        end) 0 (to_list bag).

    Definition Bag_equiv (x y : Bag A) := forall (e : A),
      multiplicity e x = multiplicity e y.

    Lemma Bag_equiv_is_Equivalence : Equivalence Bag_equiv.
    Proof with auto.
      split.
      unfold Reflexive. unfold Bag_equiv...
      unfold Symmetric. unfold Bag_equiv.
        intros.
        remember (H e)...
      unfold Transitive. unfold Bag_equiv.
        intros.
        remember (H e).
        remember (H0 e).
        rewrite -> e0...
    Qed. 

    Instance Bag_Equivalence : Equivalence Bag_equiv.
      exact Bag_equiv_is_Equivalence.
    Qed.

    Definition singleton (x : A) := MkBag [x].

    Fixpoint add (x : A) (bag : Bag A) := match bag with
      | MkBag lst => MkBag (x :: lst)
    end.

    Definition from_list (lst : list A) : Bag A := MkBag lst.

    Parameter map : forall {B : Type} {E' : Eq B}, (A -> B) -> Bag A -> Bag B.

    Definition union (b1 b2 : Bag A) : Bag A:= 
     MkBag (app (to_list b1) (to_list b2)).

    Lemma from_list_cons : forall x xs bag,
      from_list (x :: xs) === bag ->
      union (singleton x) (from_list xs) === bag.
    Proof.
      intros.
      simpl.
      unfold from_list in *.
      trivial.
    Qed.

    Lemma union_iff : forall (x : A) (b1 b2 : Bag A),
      multiplicity x (union b1 b2) = multiplicity x b1 + multiplicity x b2.
    Proof with auto.
      intros.
      destruct b1. destruct b2.
      induction l...
      unfold union in *.
      simpl in *.
      unfold multiplicity.
      unfold to_list.
      simpl.
      remember (eqdec x a) as b.
      destruct b...
      unfold multiplicity in IHl.
      simpl in IHl.
      rewrite -> IHl...
    Qed.

    Instance union_m : Proper (Bag_equiv ==> Bag_equiv ==> Bag_equiv) union.
    Proof with auto.
      unfold Proper.
      unfold Bag_equiv.
      unfold respectful.
      intros.
      rewrite union_iff.
      rewrite union_iff...
    Qed.

    Add Morphism union with signature Bag_equiv ++> Bag_equiv ++> Bag_equiv as union_s_m.
      apply union_m.
    Qed.

    Lemma union_comm : forall x y, union x y === union y x.
    Proof with auto.
      intros.
      unfold Equivalence.equiv.
      unfold Bag_equiv.
      intros.
      unfold union.
      destruct x; destruct y.
      generalize dependent l0.
      induction l; intros.
      simpl.
      rewrite -> app_nil_r...
      simpl in *.
      unfold multiplicity in *.
      simpl in *.
      remember (eqdec e a) as b.
      destruct b.
      induction l0.
      simpl.
      rewrite -> app_nil_r.
      rewrite <- Heqb...
      rewrite <- app_comm_cons.
      simpl.
      remember (eqdec e a0) as b1.
      destruct b1.
      f_equal.
      rewrite -> IHl.
      rewrite <- app_comm_cons.
      simpl.
      rewrite <- Heqb1.
      rewrite <- IHl.
      trivial.
      rewrite -> IHl.
      rewrite <- app_comm_cons.
      simpl.
      rewrite <- Heqb1.
      rewrite <- IHl.
      trivial.
      rewrite -> fold_right_app.
      rewrite -> fold_right_app.
      simpl.
      rewrite <- Heqb.
      rewrite <- fold_right_app.
      rewrite <- fold_right_app.
      rewrite <- IHl...
    Qed.

    Lemma union_assoc : forall x y z,  union (union x y) z === union x (union y z).
    Proof with auto.
      intros.
      unfold Equivalence.equiv.
      unfold Bag_equiv.
      intros.
      unfold union.
      simpl.
      rewrite -> app_assoc...
    Qed.

  Lemma from_list_comm_union : forall l1 l2,
    from_list (l1 ++ l2) === union (from_list l1) (from_list l2).
  Proof with auto.
    intros.
    unfold union.
    unfold from_list.
    apply reflexivity.
  Qed.

  Lemma from_list_nil : from_list nil === empty.
  Proof with auto.
    unfold from_list.
    unfold empty.
    apply reflexivity.
  Qed.
  
  Lemma union_empty_l : forall x, union empty x === x.
  Proof with auto.
    intros.
    unfold union.
    unfold empty.
    simpl.
    destruct x.
    simpl.
    apply reflexivity.
  Qed.

  Lemma union_empty_r : forall x, union x empty === x.
  Proof.
    intros.
    rewrite union_comm. apply union_empty_l.
  Qed.

  Lemma to_list_singleton : forall x, to_list (singleton x) = [x].
  Proof. intros. simpl. auto. Qed.

  Lemma nil_list : to_list empty = nil.
  Proof. auto. Qed.

  Lemma nil_bag : from_list nil = empty.
  Proof. auto. Qed.

  Definition size (bag : Bag A) := match bag with
    | MkBag lst => List.length lst
  end.
 
  Lemma size_equiv : forall x y, x === y -> size x = size y.
  Proof with auto.
    intros.
    unfold Equivalence.equiv in H.
    unfold Bag_equiv in H.
    destruct x; destruct y.
    induction l.
    simpl.
    destruct l0...
    remember (H a).
    unfold multiplicity in e.
    simpl in e.
    clear Heqe.
    remember (eqdec a a) as J.
    destruct J. inversion e. contradiction n...
    remember (H a). clear Heqe.
  Admitted.
     
  Lemma size_comm_union : forall x y, size (union x y) = size x + size y.
  Proof with auto.
    intros.
    unfold union.
    unfold size.
    destruct x.
    destruct y.
    unfold to_list.
    apply app_length.
  Qed.

  Lemma in_comm_union : forall x bag1 bag2, 
    In x (to_list (union bag1 bag2)) ->
    In x (to_list bag1) \/ In x (to_list bag2).
  Proof with auto with datatypes.
    intros x bag1 bag2 H.
    destruct bag1; destruct bag2.
    simpl in *...
  Qed.

  Lemma size_to_list : forall x, size x = length (to_list x).
  Proof with auto with datatypes.
    intros x.
    destruct x...
  Qed.
  End Methods.

  Lemma concat_map_union : forall {A B : Type} {EA : Eq A} {EB : Eq B} (f : A -> list B)
    (bag1 bag2 : Bag A),
    Bag_equiv EB
      (from_list (concat_map f (to_list (union bag1 bag2))))
      (union (from_list (concat_map f (to_list bag1))) 
             (from_list (concat_map f (to_list bag2)))).
  Proof with auto.
    intros.
    unfold Bag_equiv.
    intros.
    destruct bag1. destruct bag2.
    generalize dependent l0.
    induction l...
    (* inductive case *)
    intros.
    simpl in *.
    unfold from_list in *.
    unfold multiplicity in *.
    simpl.
    rewrite <- app_assoc.
    simpl in IHl.
    rewrite -> fold_right_app.
    rewrite -> fold_right_app.
    rewrite <- IHl.
    trivial.
  Qed.


End Bag.

Notation "x <+> y" := (Bag.union x y) : bag_scope.
Notation "{| x |}" := (Bag.singleton x) : bag_scope.
Notation "{| |}" := (Bag.empty) : bag_scope.

Hint Rewrite Bag.concat_map_union Bag.from_list_comm_union Bag.nil_bag
  Bag.to_list_singleton Bag.nil_list Bag.union_empty_r Bag.union_empty_l
  Bag.union_assoc : bag.

