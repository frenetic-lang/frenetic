Set Implicit Arguments.

Require Import Coq.Logic.ProofIrrelevance.
Require Import Coq.Lists.List.
Require Import Coq.Relations.Relations.
Require Import Bag.TotalOrder.
Require Import Bag.OrderedLists.
Require Import Bag.Bag2Defs.
Require Import Bag.Bag2Notations.

Import ListNotations.
Local Open Scope list_scope.

Local Open Scope list_scope.
Local Open Scope bag_scope.

Module OL := Bag.OrderedLists.

Section Methods.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.

  Lemma ordered_irr: forall (b : bag R) (lst : list A) (o : Ordered R lst),
    to_list b = lst ->
    b = Bag lst o.
  Proof with auto.
    intros.
    destruct b.
    simpl in *.
    subst.
    assert (o = order).
      apply proof_irrelevance.
    subst...
  Qed.

  Hint Resolve ordered_irr.

  Lemma union_comm : forall b1 b2, b1 <+> b2 = b2 <+> b1.
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply union_comm...
    destruct b1...
    destruct b2...
  Qed.

  Lemma union_assoc : forall x y z, (x <+> y) <+> z = x <+> (y <+> z).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    symmetry.
    apply union_assoc...
    destruct x...
    destruct y...
    destruct z...
  Qed.
    
  Lemma union_empty_l : forall x, empty <+> x = x.
  Proof with auto.
    intros.
    destruct x.
    apply ordered_irr.
    simpl.
    apply union_nil_l...
  Qed.

  Lemma union_empty_r : forall x, x <+> empty = x.
  Proof.
    intros.
    destruct x; auto.
  Qed.

  Lemma unions_cons : forall (x : bag R) (xs : list (bag R)),
    unions (x :: xs) = x <+> unions xs.
  Proof with auto.
    intros.
    apply ordered_irr...
  Qed.

  Lemma unions_app : forall (lst lst0 : list (bag R)),
    unions  (lst ++ lst0) = unions lst <+> unions lst0.
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    induction lst...
    simpl.
    rewrite -> union_nil_l...
    apply unions_order_pres.
    simpl.
    rewrite <- OL.union_assoc...
    rewrite -> IHlst...
    destruct a...
    apply unions_order_pres...
    apply unions_order_pres...
  Qed.

  Lemma pop_union_r : forall (b b0 b1: bag R),
    b0 = b1 <->
    b0 <+> b = b1 <+> b.
  Proof with simpl; auto with datatypes.
    split; intros.
    + subst. reflexivity.
    + destruct b,b0,b1.
      rename to_list into lst,to_list0 into lst0,to_list1 into lst1.
      inversion H.
      induction lst...
      simpl in H1.
      inversion order; subst.
      apply insert_eq in H1...
      apply IHlst with (order:=H4) in H1...
      apply ordered_irr. simpl...
      apply union_order_pres...
      apply union_order_pres...
  Qed.

  Lemma pop_union_l : forall (b b0 b1: bag R),
    b0 = b1 <->
    b <+> b0 = b <+> b1.
  Proof with auto.
    intros.
    do 2 rewrite -> (union_comm b).
    apply pop_union_r...
  Qed.

  Lemma rotate_union : forall (b b0 b1 : bag R),
    union b b0 = b1 ->
    union b0 b = b1.
  Proof.
    intros. subst. apply union_comm.
  Qed.

  Lemma from_list_cons : forall x xs,
    from_list (x :: xs) = ({| x |}) <+> (from_list xs).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply from_list_cons.
  Qed.

  Lemma from_list_app : forall lst1 lst2,
    from_list (lst1 ++ lst2) = union (from_list lst1) (from_list lst2).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply from_list_app.
  Qed.

  Lemma from_list_nil_is_empty : from_list nil = empty.
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl...
  Qed.

  Lemma in_union : forall (x : A) (b1 b2 : bag R),
    In x (to_list (b1 <+> b2)) <->
    In x (to_list b1) \/ In x (to_list b2).
  Proof with auto.
    intros.
    split; intros.
    * simpl in H.
      destruct b1.
      destruct b2.
      simpl in H.
      apply In_union in H...
    * destruct H.
      destruct b1; destruct b2; simpl.
      apply In_union...
      apply In_union...
      destruct b1...
      destruct b2...
  Qed.

  Lemma unions_nil : unions nil = empty.
  Proof with auto.
    intros.
    apply ordered_irr...
  Qed.

  Lemma to_list_nil : forall (b : bag R), to_list b = nil -> b = empty.
  Proof with auto.
    intros.
    apply ordered_irr...
  Qed.

  Lemma in_split : forall x bag,
    In x (to_list bag) ->
    exists rest,
      bag = (union (singleton x) rest).
  Proof with auto with datatypes.
    intros.
    destruct bag.
    simpl in H.
    rename to_list into lst.
    induction lst...
    + simpl in H. inversion H.
    + simpl in H.
      inversion order; subst.
      destruct H.
      - subst...
        exists (Bag lst H3).
        apply ordered_irr.
        simpl.
        symmetry.
        apply union_cons...
      - apply IHlst with (order := H3) in H.
        destruct H as [rest H].
        exists (({|a|}) <+> rest).
        apply ordered_irr.
        simpl.
        assert (Ordered R [x]). { apply Ordered_cons... intros. simpl in H0... inversion H0. apply Ordered_nil. }
        assert (Ordered R [a]). { apply Ordered_cons... intros. simpl in H1... inversion H1. apply Ordered_nil. }
        rewrite -> OL.union_assoc...
        rewrite -> (OL.union_comm Order H0 H1).
        rewrite <- OL.union_assoc...
        unfold union in H.
        simpl in H.
        inversion H.
        rewrite <- H5.
        symmetry.
        apply union_cons...
        destruct rest...
        destruct rest...
  Qed.

  Lemma in_unions : forall (x : A) (lst : list (bag R)),
    In x (to_list (unions lst)) ->
    exists bag, 
      In bag lst /\ In x (to_list bag).
  Proof with eauto with datatypes.
    intros.
    induction lst...
    simpl in H.
    apply OL.In_union in H.
    + destruct H.
      - destruct a.
        simpl in H...
      - apply IHlst in H.
        destruct H as [bag HIn].
        destruct HIn...
    + destruct a...
    + apply unions_order_pres.
  Qed.

  Lemma in_to_from_list : forall x lst,
    In x (to_list (from_list lst)) ->
    In x lst.
  Proof with auto with datatypes.
    intros.
    induction lst...
    simpl in H.
    apply In_insert in H.
    destruct H...
    subst...
  Qed.

  Lemma singleton_eq_singleton : forall x y lst,
    ({|x|}) = ({|y|}) <+> lst -> lst = empty /\ x = y.
  Proof with auto with datatypes.
    intros.
    inversion H.
    rewrite -> OL.union_comm in H1.
    simpl in H1.
    destruct lst.
    rename to_list into lst.
    simpl in H1.
    destruct lst...
    + simpl in H1.
      inversion H1.
      subst.
      split...
      apply ordered_irr...
    + simpl in H1.
      destruct (compare y a).
      - inversion H1.
      - inversion H1.
        subst.
        destruct lst...
        simpl in H3...
        inversion H3.
        simpl in H3.
        destruct (compare y a0)...
        inversion H3.
        inversion H3.
   + apply Ordered_cons. intros. inversion H0. apply Ordered_nil.
   + destruct lst...
  Qed.

  (* This pattern keeps showing up. Typically x and y are FlowMods and we have 
     hypotheses stating that b1 and b2 do not contain FlowMod messages. *)
  Lemma singleton_union_disjoint : forall x y b1 b2,
    ({|x|} <+> b1) = ({|y|} <+> b2) ->
    (In x (to_list b2) -> False) ->
    x = y /\ b1 = b2.
  Proof with auto with datatypes.
    intros.
    assert (In x (to_list ({|y|} <+> b2))) as J.
    { rewrite <- H. apply in_union; simpl... }
    apply in_union in J; simpl in J.
    destruct J as [[J | J] | J].
    + subst.
      apply pop_union_l in H...
    + inversion J.
    + contradiction J.
  Qed.

  Lemma union_from_ordered : forall b1 b2 b3 b4,
    OL.union (to_list b1) (to_list b2) = OL.union (to_list b3) (to_list b4) ->
    (b1 <+> b2) = (b3 <+> b4).
  Proof with auto with datatypes.
    intros.
    destruct b1, b2, b3, b4.
    simpl in H.
    unfold union.
    simpl.
    apply ordered_irr...
  Qed.

End Methods.

Section BinaryMethods.

  Variable A B: Type.
  Variable RA : relation A.
  Variable RB : relation B.
  Variable AOrder : TotalOrder RA.
  Variable BOrder : TotalOrder RB.

  Lemma map_union : forall (f : A -> B) (bag1 bag2 : bag RA),
    from_list (map f (to_list (union bag1 bag2))) =
      (union (from_list (map f (to_list bag1))) 
        (from_list (map f (to_list bag2)))).
  Proof with auto.
    intros.
    apply ordered_irr.
    simpl.
    apply map_union...
    destruct bag1...
    destruct bag2...
  Qed.


  Lemma in_unions_map : forall (b : B) (lst: list A) (f : A -> bag RB),
   In b (to_list (unions (map f lst))) ->
    exists (a : A), In a lst /\ In b (to_list (f a)).
  Proof with eauto with datatypes.
    intros.
    induction lst.
    + simpl in H. inversion H.
    + simpl in H.
      apply In_union in H.
      destruct H...
      apply IHlst in H.
      clear IHlst.
      destruct H as [a0 [Ha0In HbIn]].
      exists a0...
      destruct (f a)...
      apply unions_order_pres...
  Qed.

  Lemma unions_map_insert_comm : forall (x : A) (xs : list A)
    (f : A -> bag RB) ,
    Ordered RA xs ->
    unions (map f (insert x xs)) =  (f x) <+> unions (map f xs).
  Proof with auto.
    intros.
    induction xs...
    apply ordered_irr...
    simpl.
    destruct (compare x a).
    simpl.
    rewrite -> unions_cons...
    simpl.
    rewrite -> unions_cons...
    rewrite -> unions_cons...
    rewrite <- union_assoc.
    assert (f x <+> f a = f a <+> f x) by apply union_comm.
    rewrite -> H0.
    rewrite -> union_assoc.
    rewrite -> IHxs...
    inversion H...
  Qed.

  Lemma unions_map_union_comm : forall (x : A) (xs : bag RA)
    (f : A -> bag RB),
    unions (map f (to_list ((singleton x) <+> xs))) =
    (f x) <+> unions (map f (to_list xs)).
  Proof with eauto with datatypes.
    intros.
    destruct xs.
    induction to_list...
    simpl.
    apply ordered_irr...
    simpl in *.
    inversion order; subst.
    apply IHto_list in H2; clear IHto_list.
    rewrite -> unions_map_insert_comm.
    rewrite -> unions_cons.
    rewrite -> H2.
    assert (f x <+> f a = f a <+> f x) by apply union_comm.
    rewrite <- union_assoc.
    rewrite <- H.
    rewrite -> union_assoc...
    apply union_order_pres...
    apply Ordered_cons...
    intros.
    simpl in H.
    inversion H.
    apply Ordered_nil.
    inversion order...
  Qed.


  Lemma unions_map_union_comm2 : forall (lst0 lst1 : bag RA)
    (f : A -> bag RB),
    unions (map f (to_list (lst0 <+> lst1))) =
    unions (map f (to_list lst0)) <+> unions (map f (to_list lst1)).
  Proof with eauto with datatypes.
    intros.
    destruct lst1.
    induction to_list...
    simpl.
    apply ordered_irr...
    simpl in *.
    inversion order; subst.
    apply IHto_list in H2; clear IHto_list.
    rewrite -> unions_map_insert_comm.
    rewrite -> unions_cons.
    rewrite -> H2.
    rewrite <- union_assoc.
    rewrite -> (union_comm _ (f a)).
    rewrite -> union_assoc...
    apply union_order_pres...
    destruct lst0...
    inversion order...
  Qed.

  Lemma unions_map_bag : forall (lst : list A) (f : A -> bag RB),
    unions (map f (to_list (from_list lst))) = unions (map f lst).
  Proof with eauto with datatypes.
    intros.
    induction lst...
    simpl.
    rewrite -> unions_cons.
    rewrite -> unions_map_insert_comm.
    simpl in IHlst.
    rewrite -> IHlst...
    apply from_list_order.
  Qed.

  Require Import Common.AllDiff.

  Lemma AllDiff_preservation : forall (f : A -> B) x y lst,
      AllDiff f (to_list (({|x|}) <+> lst)) ->
      f x = f y ->
      AllDiff f (to_list (({|y|}) <+> lst)).
  Proof with auto with datatypes.
    intros.
    destruct lst.
    rename to_list into lst.
    unfold singleton in *.
    unfold to_list in *.
    simpl in *.
    apply OrderedLists.AllDiff_preservation with (x:=x)...
  Qed.

End BinaryMethods.


