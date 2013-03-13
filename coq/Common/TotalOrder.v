Set Implicit Arguments.

Require Import Coq.Relations.Relations.
Require Import Coq.Classes.RelationClasses.
Require Import Common.Types.
Require Import Coq.Lists.List.

Local Open Scope list_scope.

Class TotalOrder {A : Type} (R : relation A) := {
  TotalOrder_Reflexive : Reflexive R;
  TotalOrder_Antisymmetry : forall (x y : A), R x y -> R y x -> x = y;
  TotalOrder_Transitivity : forall (x y z : A), R x y -> R y z -> R x z;
  TotalOrder_Comparable : forall (x y : A), { R x y } + { R y x }
}.

Definition bag (A : Type) := list A.

Inductive OrderedBag (A : Type) (R : relation A) : bag A -> Prop :=
| OrderedBag_nil : OrderedBag R nil
| OrderedBag_cons : forall x xs,
   (forall y, In y xs -> R x y) ->
   OrderedBag R xs ->
   OrderedBag R (x :: xs).

Section Defs.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.
  
  Fixpoint insert (x : A) (lst : list A) :=
    match lst with
      | nil => cons x nil
      | cons y ys =>
        match TotalOrder_Comparable x y with
          | left _ => cons x (cons y ys) 
          | right _ => cons y (insert x ys)
        end
    end.

  Lemma insert_in : forall x y b, 
    In x (insert y b) ->
    x = y \/ In x b.
  Proof with auto with datatypes.
    intros.
    induction b...
    simpl in H...
    destruct H...
    simpl in H.
    remember (TotalOrder_Comparable y a) as cmp.
    destruct cmp.
    simpl.
    simpl in H.
    destruct H as [H | [H0 | H1]]...
    simpl in H.
    destruct H.
    subst.
    right...
    apply IHb in H.
    destruct H...
  Qed.
    
  Hint Constructors OrderedBag.

  Lemma insert_order_pres : forall (x : A) (b : bag A),
    OrderedBag R b ->
    OrderedBag R (insert x b).
  Proof with auto.
    intros.
    induction b...
    simpl...
    apply OrderedBag_cons...
    intros.
    inversion H0.
    simpl.
    remember (TotalOrder_Comparable x a) as cmp.
    destruct cmp.
    apply OrderedBag_cons...
    intros.
    simpl in H0.
    destruct H0.
    subst...
    inversion H.
    subst.
    eapply TotalOrder_Transitivity...
    exact r.
    inversion H.
    subst.
    apply OrderedBag_cons...
    intros.
    apply insert_in in H0.
    destruct H0.
    subst...
    apply H2...
  Qed.

  Definition union (b1 b2 : list A) := fold_right insert b1 b2.

  Lemma union_order_pres : forall (b1 b2 : bag A),
    OrderedBag R b1 ->
    OrderedBag R b2 ->
    OrderedBag R (union b1 b2).
  Proof with auto with datatypes.
    intros.
    induction b2...
    simpl.
    inversion H0; subst.
    apply insert_order_pres...
  Qed.

  Hint Resolve insert_order_pres union_order_pres.

  Lemma insert_eq_head : forall x b,
    OrderedBag R b ->
    (forall y, In y b -> R x y) ->
    insert x b = x :: b.
  Proof with eauto with datatypes.
    intros.
    induction b...
    simpl.
    remember (TotalOrder_Comparable x a) as cmp0.
    destruct cmp0...
    assert (R x a)...
    assert (x = a).
    { apply TotalOrder_Antisymmetry... }
    subst.
    rewrite -> IHb...
    inversion H...
  Qed.

  Lemma insert_comm : forall x y b,
    OrderedBag R b ->
    insert x (insert y b) = insert y (insert x b).
  Proof with eauto with datatypes.
    intros.
    induction H...
    + simpl.
      remember (TotalOrder_Comparable x y) as cmp0.
      remember (TotalOrder_Comparable y x) as cmp1.
      destruct cmp0; destruct cmp1...
      assert (x = y).
      { apply TotalOrder_Antisymmetry... }
      subst...
      assert (x = y).
      { apply TotalOrder_Antisymmetry... }
      subst...
    + simpl.
      remember (TotalOrder_Comparable y x0) as cmp0.
      remember (TotalOrder_Comparable x x0) as cmp1.
      destruct cmp0.
      destruct cmp1.
      simpl.
      remember (TotalOrder_Comparable x y) as cmp2.
      rewrite <- Heqcmp1.
      rewrite <- Heqcmp0.
      remember (TotalOrder_Comparable y x) as cmp3.
      destruct cmp2;
      destruct cmp3...
      assert (x = y).
      { apply TotalOrder_Antisymmetry... }
      subst...
      assert (x = y).
      { apply TotalOrder_Antisymmetry... }
      subst...
      simpl.
      rewrite <- Heqcmp0.
      rewrite <- Heqcmp1.
      remember (TotalOrder_Comparable x y) as cmp2.
      destruct cmp2...
      assert (R x x0) as r2.
        eapply TotalOrder_Transitivity...
      assert (x = x0).
      { apply TotalOrder_Antisymmetry... }
      subst.
      clear Heqcmp1 r2 r0.
      assert (x0 = y).
      { apply TotalOrder_Antisymmetry... }
      subst.
      clear Heqcmp2 r1 Heqcmp0 r.
      destruct xs...
      simpl.
      remember (TotalOrder_Comparable y a) as cmp0.
      destruct cmp0...
      assert (R y a)...
      assert (a = y).
      { apply TotalOrder_Antisymmetry... }
      subst.
      rewrite -> insert_eq_head...
      inversion H0...
      destruct cmp1...
      simpl.
      rewrite <- Heqcmp1...
      rewrite <- Heqcmp0...
      remember (TotalOrder_Comparable y x) as cmp2.
      destruct cmp2...
      assert (R x0 x) as r2.
      { eapply TotalOrder_Transitivity... }
      assert (x = x0).
      { apply TotalOrder_Antisymmetry... }
      subst.
      clear Heqcmp1 r2 r0.
      assert (x0 = y).
      { apply TotalOrder_Antisymmetry... }
      subst.
      clear Heqcmp2 r1 Heqcmp0 r.
      destruct xs...
      simpl.
      remember (TotalOrder_Comparable y a) as cmp0.
      destruct cmp0...
      assert (R y a)...
      assert (a = y).
      { apply TotalOrder_Antisymmetry... }
      subst.
      rewrite -> insert_eq_head...
      inversion H0...
      simpl.
      rewrite <- Heqcmp0...
      rewrite <- Heqcmp1...
      rewrite -> IHOrderedBag...
  Qed.

  Lemma union_nil_l : forall b, 
    OrderedBag R b ->
    union nil b = b.
  Proof with auto.
    intros.
    unfold union.
    induction b...
    simpl.
    inversion H.
    subst.
    rewrite -> IHb...
    apply insert_eq_head...
  Qed.

  Lemma union_insert_comm_2 : forall a b1 b2,
    OrderedBag R (a :: b1) ->
    OrderedBag R b2 ->
    union (a :: b1) b2 = insert a (union b1 b2).
  Proof with auto with datatypes.
    intros.
    generalize dependent b1.
    induction b2; intros.
    simpl.
    symmetry.
    inversion H.
    subst.
    apply insert_eq_head...
    simpl.
    inversion H.
    inversion H0.
    subst.
    rewrite -> IHb2...
    rewrite -> insert_comm...
  Qed.

  Lemma In_insert : forall x y b,
    In x (insert y b) ->
    x = y \/ In x b.
  Proof with eauto with datatypes.
    intros.
    induction b...
    simpl in H.
    destruct H...
    simpl in H.
    remember (TotalOrder_Comparable y a) as cmp0.
    destruct cmp0.
    simpl in H.
    destruct H...
    simpl in H.
    destruct H...
    subst.
    right...
    apply IHb in H.
    destruct H...
  Qed.

  Lemma union_insert_l_comm : forall a b1 b2,
    OrderedBag R b1 ->
    OrderedBag R b2 ->
    union (insert a b1) b2 = insert a (union b1 b2).
  Proof with eauto with datatypes.
    intros.
    induction b1...
    simpl.
    rewrite -> union_nil_l...
    unfold union.
    induction b2...
    inversion H0; subst.
    simpl.
    remember (TotalOrder_Comparable a a0) as cmp0.
    destruct cmp0...
    rewrite -> IHb2...
    assert (insert a b2 = a :: b2).
    { apply insert_eq_head... intros. eapply TotalOrder_Transitivity... }
    rewrite -> H1.
    simpl.
    remember (TotalOrder_Comparable a0 a) as cmp1.
    destruct cmp1...
    assert (a0 = a).
    { apply TotalOrder_Antisymmetry... }
    subst...
    rewrite -> insert_eq_head...
    inversion H0.
    subst.
    rewrite -> IHb2...
    rewrite -> insert_eq_head...
    intros.
    apply In_insert in H1...
    destruct H1...
    subst...
    (* case *)
    inversion H; subst.
    rewrite -> union_insert_comm_2...
    rewrite -> insert_comm...
    simpl.
    remember (TotalOrder_Comparable a a0) as cmp0.
    destruct cmp0.
    rewrite -> union_insert_comm_2...
    rewrite -> union_insert_comm_2...
    rewrite -> insert_comm...
    apply OrderedBag_cons...
    intros.
    simpl in H1.
    destruct H1...
    subst...
    eapply TotalOrder_Transitivity...
    (* case *)
    rewrite -> union_insert_comm_2...
    rewrite -> IHb1...
    apply OrderedBag_cons...
    intros.
    apply In_insert in H1.
    destruct H1...
    subst...
  Qed.

  Lemma union_insert_r_comm : forall a b1 b2,
    OrderedBag R b1 ->
    OrderedBag R b2 ->
    union b1 (insert a b2) = insert a (union b1 b2).
  Proof with eauto with datatypes.
    intros.
    induction b1...
    simpl.
    rewrite -> union_nil_l...
    rewrite -> union_nil_l...
    inversion H. subst.
    rewrite -> union_insert_comm_2...
    rewrite -> union_insert_comm_2...
    rewrite -> IHb1...
    rewrite -> insert_comm...
  Qed.

  Lemma union_assoc : forall b1 b2 b3, 
    OrderedBag R b1 ->
    OrderedBag R b2 ->
    OrderedBag R b3 ->
    union b1 (union b2 b3) = union (union b1 b2) b3.
  Proof with auto with datatypes.
    intros.
    induction b2...
    + rewrite -> union_nil_l...
    + simpl.
      inversion H0.
      subst.
      rewrite -> union_insert_l_comm...
      rewrite -> union_insert_comm_2...
      rewrite -> union_insert_r_comm...
      rewrite -> IHb2...
  Qed.

  Lemma union_comm : forall b1 b2, 
    OrderedBag R b1 ->
    OrderedBag R b2 ->
    union b1 b2 = union b2 b1.
  Proof with auto with datatypes.
    intros.
    induction b2...
    unfold union.
    simpl.
    rewrite -> union_nil_l...
    simpl.
    rewrite -> union_insert_comm_2...
    rewrite -> IHb2...
    inversion H0...
  Qed.

End Defs.