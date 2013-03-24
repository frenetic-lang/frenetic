Set Implicit Arguments.

Require Import Coq.Relations.Relations.
Require Import Coq.Lists.List.
Require Import Bag.TotalOrder.

(* TODO(arjun): Need a lemma about Ordered (singleton x) to shorten some
   proofs. *)
Import ListNotations.
Local Open Scope list_scope.

Inductive Ordered (A : Type) (R : relation A) : list A -> Prop :=
| Ordered_nil : Ordered R nil
| Ordered_cons : forall x xs,
   (forall y, In y xs -> R x y) ->
   Ordered R xs ->
   Ordered R (x :: xs).

Section Definitions.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.
  
  Fixpoint insert (x : A) (b : list A) : list A :=
    match b with
      | nil => [x]
      | y :: ys =>
        match compare x y with
          | left _ => x :: y :: ys
          | right _ => y :: insert x ys
        end
    end.
  

  Definition union (b1 b2 : list A) : list A := fold_right insert b1 b2.

  Definition from_list (lst : list A) : list A := fold_right insert nil lst.

  Definition unions (lsts : list (list A)) : list A :=  fold_right union nil lsts.

End Definitions.

Arguments insert [A R Order] x b.
Arguments union [A R Order] b1 b2.
Arguments from_list [A R Order] lst.
Arguments unions [A R Order] lsts.

Section Lemmas.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.

  Hint Constructors Ordered.

  Lemma insert_in : forall (x y : A) (b : list A), 
    In x (insert y b) ->
    x = y \/ In x b.
  Proof with auto with datatypes.
    intros.
    induction b...
    simpl in H...
    destruct H...
    simpl in H.
    remember (compare y a) as cmp.
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

  Lemma insert_order_pres : forall (x : A) (b : list A),
    Ordered R b ->
    Ordered R (insert x b).
  Proof with eauto.
    intros.
    induction b...
    simpl...
    apply Ordered_cons...
    intros.
    inversion H0.
    simpl.
    remember (compare x a) as cmp.
    destruct cmp.
    apply Ordered_cons...
    intros.
    simpl in H0.
    destruct H0.
    subst...
    inversion H.
    subst.
    eapply transitivity...
    apply Ordered_cons...
    intros.
    apply insert_in in H0.
    destruct H0.
    subst...
    inversion H...
    subst...
    inversion H...
  Qed.

  Hint Resolve insert_order_pres.

  Lemma singleton_order : forall (x : A),
    Ordered R [x].
  Proof.
    intros. apply Ordered_cons. intros. simpl in H; inversion H.
    apply Ordered_nil.
  Qed.

  Lemma union_order_pres : forall (b1 b2 : list A),
    Ordered R b1 ->
    Ordered R b2 ->
    Ordered R (union b1 b2).
  Proof with auto with datatypes.
    intros.
    induction b2...
    simpl.
    inversion H0; subst...
  Qed.

  Hint Resolve union_order_pres.

  Lemma unions_order_pres : forall (lsts : list (list A)),
    (forall (b : list A), In b lsts -> Ordered R b) ->
    Ordered R (unions lsts).
  Proof with simpl;auto with datatypes.
    intros.
    induction lsts...
  Qed.

  Hint Resolve unions_order_pres union_order_pres.

  Hint Resolve antisymmetry transitivity.

  Lemma insert_eq_head : forall x b,
    Ordered R b ->
    (forall y, In y b -> R x y) ->
    insert x b = x :: b.
  Proof with eauto with datatypes.
    intros.
    induction b...
    simpl.
    destruct (compare x a)...
    assert (x = a)...
    subst.
    rewrite -> IHb...
    inversion H...
  Qed.

  Hint Resolve insert_eq_head.

  Lemma insert_comm : forall x y b,
    Ordered R b ->
    insert x (insert y b) = insert y (insert x b).
  Proof with eauto with datatypes.
    intros.
    induction H...
    + simpl.
      destruct (compare x y); destruct (compare y x)...
      assert (x = y)...
      subst...
      assert (x = y)...
      subst...
    + simpl.
      remember (compare y x0) as cmp0.
      remember (compare x x0) as cmp1.
      destruct cmp0; destruct cmp1.
      simpl.
      remember (compare x y) as cmp2.
      rewrite <- Heqcmp1.
      rewrite <- Heqcmp0.
      remember (compare y x) as cmp3.
      destruct cmp2;
      destruct cmp3...
      assert (x = y)...
      subst...
      assert (x = y)...
      subst...
      simpl.
      rewrite <- Heqcmp0.
      rewrite <- Heqcmp1.
      remember (compare x y) as cmp2.
      destruct cmp2...
      assert (x = x0)...
      subst.
      assert (x0 = y)...
      subst.
      destruct xs...
      simpl.
      remember (compare y a) as cmp0.
      destruct cmp0...
      assert (a = y)...
      subst.
      rewrite -> insert_eq_head...
      inversion H0...
      simpl.
      rewrite <- Heqcmp1...
      rewrite <- Heqcmp0...
      remember (compare y x) as cmp2.
      destruct cmp2...
      assert (x = x0)...
      assert (x0 = y)...
      subst.
      destruct xs...
      simpl.
      remember (compare y a) as cmp0.
      destruct cmp0...
      assert (a = y)...
      subst.
      rewrite -> insert_eq_head...
      inversion H0...
      simpl.
      rewrite <- Heqcmp0...
      rewrite <- Heqcmp1...
      rewrite -> IHOrdered...
  Qed.

  Hint Resolve insert_comm.

  Lemma union_nil_l : forall b, 
    Ordered R b ->
    union nil b = b.
  Proof with auto.
    intros.
    unfold union.
    induction b...
    simpl.
    inversion H.
    subst.
    rewrite -> IHb...
  Qed.

  Hint Resolve union_nil_l.

  Lemma union_cons_insert : forall a b1 b2,
    Ordered R (a :: b1) ->
    Ordered R b2 ->
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
  Qed.

  Lemma insert_nonempty : forall x xs,
    insert x xs = nil -> False.
  Proof with auto with datatypes.
    intros.
    destruct xs...
    simpl in H.
    inversion H.
    simpl in H.
    destruct (compare x a).
    inversion H.
    inversion H.
  Qed.

  Lemma insert_eq : forall x lst0 lst1,
    Ordered R lst0 ->
    Ordered R lst1 ->
    insert x lst0 = insert x lst1 -> lst0 = lst1.
  Proof with auto with datatypes.
    intros.
    generalize dependent lst0.
    induction lst1; intros.
    + simpl in H1.
      destruct lst0...
      simpl in H1.
      destruct (compare x a).
      inversion H1.
      inversion H1.
      apply insert_nonempty in H4.
      inversion H4.
    + destruct lst0.
      simpl in H1.
      destruct (compare x a).
      inversion H1.
      inversion H1.
      symmetry in H4. apply insert_nonempty in H4. inversion H4.
      simpl in H1.
      { destruct (compare x a), (compare x a0).
        + inversion H1...
        + inversion H1; subst.
          inversion H1; subst.
          assert (R a x).
          { destruct lst0. 
            simpl in H4. inversion H4; subst...
            simpl in H4.
            destruct (compare x a0).
            inversion H4... apply reflexivity.
            inversion H4; subst... }
          assert (a = x).
          { apply antisymmetry... }
          subst.
          symmetry.
          inversion H; subst.
          apply insert_eq_head...
        + inversion H1; subst; clear H1.
          rewrite -> H4.
          inversion H0; subst.
          apply insert_eq_head...
        + inversion H1; subst; clear H1.
          f_equal.
          inversion H0; inversion H; subst.
          apply IHlst1...
      }
  Qed.

  Hint Resolve union_cons_insert.

  Lemma In_insert : forall x y b,
    In x (insert y b) <->
    x = y \/ In x b.
  Proof with eauto with datatypes.
    intros.
    split; intros.
    + induction b...
      simpl in H.
      destruct H...
      simpl in H.
      remember (compare y a) as cmp0.
      destruct cmp0.
      simpl in H.
      destruct H...
      simpl in H.
      destruct H...
      subst.
      right...
      apply IHb in H.
      destruct H...
    + destruct H.
      * subst...
        induction b...
        simpl...
        simpl...
        destruct (compare y a)...
      * induction b...
        simpl...
        simpl.
        destruct (compare y a)...
        simpl in *.
        destruct H...
  Qed.

  Lemma union_insert_l_comm : forall a b1 b2,
    Ordered R b1 ->
    Ordered R b2 ->
    union (insert a b1) b2 = insert a (union b1 b2).
  Proof with subst; eauto with datatypes.
    intros.
    induction b1...
    simpl.
    rewrite -> union_nil_l...
    unfold union.
    induction b2...
    inversion H0; subst.
    simpl.
    remember (compare a a0) as cmp0.
    destruct cmp0...
    rewrite -> IHb2...
    assert (insert a b2 = a :: b2)...
    rewrite -> H1.
    simpl.
    remember (compare a0 a) as cmp1.
    destruct cmp1...
    assert (a0 = a)...
    rewrite -> insert_eq_head...
    inversion H0...
    rewrite -> IHb2...
    rewrite -> insert_eq_head...
    intros.
    apply In_insert in H1...
    destruct H1...
    (* case *)
    inversion H...
    rewrite -> union_cons_insert...
    rewrite -> insert_comm...
    remember (compare a a0) as cmp0.
    destruct cmp0...
    simpl.
    rewrite <- Heqcmp0...
    rewrite -> insert_comm...
    rewrite -> union_cons_insert...
    rewrite -> union_cons_insert...
    apply Ordered_cons...
    intros.
    simpl in H1.
    destruct H1...
    (* case *)
    simpl.
    rewrite <- Heqcmp0...
    rewrite -> union_cons_insert...
    rewrite -> IHb1...
    apply Ordered_cons...
    intros...
    apply In_insert in H1.
    destruct H1...
  Qed.

  Lemma union_insert_r_comm : forall a b1 b2,
    Ordered R b1 ->
    Ordered R b2 ->
    union b1 (insert a b2) = insert a (union b1 b2).
  Proof with eauto with datatypes.
    intros.
    induction b1...
    simpl.
    rewrite -> union_nil_l...
    rewrite -> union_nil_l...
    inversion H. subst.
    rewrite -> union_cons_insert...
    rewrite -> union_cons_insert...
    rewrite -> IHb1...
  Qed.

  Lemma union_assoc : forall b1 b2 b3, 
    Ordered R b1 ->
    Ordered R b2 ->
    Ordered R b3 ->
    union b1 (union b2 b3) = union (union b1 b2) b3.
  Proof with auto with datatypes.
    intros.
    induction b2...
    + rewrite -> union_nil_l...
    + simpl.
      inversion H0.
      subst.
      rewrite -> union_insert_l_comm...
      rewrite -> union_cons_insert...
      rewrite -> union_insert_r_comm...
      rewrite -> IHb2...
  Qed.

  Lemma union_comm : forall b1 b2, 
    Ordered R b1 ->
    Ordered R b2 ->
    union b1 b2 = union b2 b1.
  Proof with auto with datatypes.
    intros.
    induction b2...
    + simpl...
      rewrite -> union_nil_l...
    + simpl...
      rewrite -> union_cons_insert...
      rewrite -> IHb2...
      inversion H0...
  Qed.

  Lemma from_list_order : forall (lst : list A),
    Ordered R (from_list lst).
  Proof with eauto with datatypes.
    intros.
    induction lst...
    simpl...
    simpl...
  Qed.

  Hint Resolve from_list_order.

  Lemma unions_app : forall (lst lst0 : list (list A)),
    (forall (b : list A), In b lst -> Ordered R b) ->
    (forall (b : list A), In b lst0 -> Ordered R b) ->
    unions (lst ++ lst0) = union (unions lst) (unions lst0).
  Proof with eauto with datatypes.
    intros.
    induction lst...
    simpl.
    rewrite -> union_nil_l...
    simpl.
    rewrite <- union_assoc...
    rewrite -> IHlst...
  Qed.

  Lemma from_list_app : forall (lst lst0 : list A),
    from_list (lst ++ lst0) = union (from_list lst) (from_list lst0).
  Proof with auto.
    intros.
    simpl.
    induction lst.
    simpl.
    rewrite -> union_nil_l...
    simpl.
    rewrite -> IHlst.
    rewrite -> union_insert_l_comm...
  Qed.

  Lemma In_union : forall (x : A) (b1 b2 : list A),
    Ordered R b1 ->
    Ordered R b2 ->
    (In x (union b1 b2) <->
     In x b1 \/ In x b2).
  Proof with auto with datatypes.
    intros.
    split; intros.
    + induction b1...
      rewrite -> union_nil_l in H1...
      rewrite -> union_cons_insert in H1...
      apply In_insert in H1...
      destruct H1.
      * subst...
      * apply IHb1 in H1...
        destruct H1...
        inversion H...
    + destruct H1...
      * induction b2...
        simpl.
        rewrite -> In_insert.
        inversion H0; subst.
        apply IHb2 in H5...
      * induction b2...
        simpl in H1. inversion H1.
        simpl.
        apply In_insert.
        simpl in H1.
        destruct H1...
        apply IHb2 in H1...
        inversion H0...
  Qed.

  Lemma In_unions : forall (x : A) lst, 
    (forall (b : list A), In b lst -> Ordered R b) ->
    In x (unions lst) ->
    exists elt, In elt lst /\ In x elt.
  Proof with eauto with datatypes.
    intros.
    induction lst...
    simpl in H0.
    apply In_union in H0...
    destruct H0...
    apply IHlst in H0.
    destruct H0 as [elt [HeltIn HxIn]]...
    intros.
    apply H...
  Qed.

  Lemma union_singleton_l : forall x xs,
    Ordered R xs ->
    insert x xs = union [x] xs.
  Proof with auto with datatypes.
    intros.
    simpl...
    rewrite -> union_comm...
    apply Ordered_cons...
    intros.
    simpl in H0.
    inversion H0.
  Qed.

  Lemma from_list_cons : forall x xs,
    insert x (from_list xs) = union [x] (from_list xs).
  Proof with auto with datatypes.
    intros.
    apply union_singleton_l.
    apply from_list_order.
  Qed.

  Lemma in_from_list_iff : forall x lst, In x lst <-> In x (from_list lst).
  Proof with auto with datatypes.
    split; intros.
    + induction lst...
      simpl in H.
      destruct H.
      - subst. simpl. apply In_insert...
      - simpl. apply In_insert...
    + induction lst...
      simpl in H.
      apply In_insert in H.
      destruct H...
      subst...
  Qed.

  Lemma from_list_id : forall lst,
    Ordered R lst ->
    from_list lst = lst.
  Proof with auto with datatypes.
    intros.
    induction lst...
    inversion H; subst.
    simpl.
    remember (from_list lst) as lst0.
    destruct lst0...
    + simpl. rewrite -> IHlst...
    + simpl. 
      destruct (compare a a0).
      rewrite -> IHlst...
      assert (a = a0).
      apply antisymmetry...
      apply H2.
      apply in_from_list_iff.
      rewrite <- Heqlst0...
      subst...
      f_equal.
      rewrite <- IHlst...
      assert (Ordered R (a0 :: lst0)).
      { rewrite -> Heqlst0... }
      inversion H0...
  Qed.

  Lemma union_cons : forall x xs,
    Ordered R xs ->
    (forall y, In y xs -> R x y) ->
    union [x] xs = x :: xs.
  Proof with auto with datatypes.
    intros.
    induction xs...
    simpl.
    inversion H; subst.
    rewrite -> IHxs...
    simpl.
    destruct (compare a x)...
    + assert (a = x).
      apply antisymmetry...
      subst...
    + f_equal...
  Qed.

  Lemma in_inserted : forall x lst, In x (insert x lst).
  Proof with auto with datatypes.
    intros.
    induction lst...
    simpl...
    simpl.
    destruct (compare x a)...
  Qed.

  Lemma in_inserted_tail : forall x y lst,
    In x lst ->
    In x (insert y lst).
  Proof with auto with datatypes.
    intros.
    induction lst...
    inversion H.
    simpl in H.
    destruct H; subst.
    + simpl.
      destruct (compare y x)...
    + simpl.
      destruct (compare y a)...
  Qed.

  Lemma in_split : forall x lst,
    Ordered R lst ->
    In x lst ->
    exists rest,
      Ordered R rest /\
      lst = union [x] rest.
  Proof with auto with datatypes.
    intros.
    induction lst...
    simpl in H0.
    inversion H0.
    simpl in H0.
    destruct H0; subst...
    + exists lst.
      inversion H; subst.
      rewrite <- union_singleton_l...
      simpl.
      split...
      symmetry.
      apply insert_eq_head...
    + inversion H; subst.
      apply IHlst in H0...
      destruct H0 as [rest [HOrderedRest Heq]].
      exists (union [a] rest).
      split...
      - apply union_order_pres...
        apply Ordered_cons... intros. simpl in H0. inversion H0.
      - rewrite <- insert_eq_head...
        rewrite -> Heq.
        rewrite <- union_singleton_l...
        rewrite <- union_singleton_l...
        rewrite <- union_singleton_l...
        apply union_order_pres...
        apply Ordered_cons... intros. simpl in H0. inversion H0.
  Qed.

End Lemmas.

Section BinaryLemmas.

  Variable A : Type.
  Variable R : relation A.
  Variable Order : TotalOrder R.

  Variable B: Type.
  Variable S : relation B.
  Variable P : TotalOrder S.

  Lemma from_list_map_cons : forall (f : A -> B) x xs,
    Ordered R xs ->
    from_list (map f (insert x xs)) =
    from_list (map f (x :: xs)).
  Proof with auto with datatypes.
    intros.
    induction xs...
    simpl.
    destruct (compare x a)...
    simpl.
    rewrite -> insert_comm.
    2: apply from_list_order.
    inversion H; subst.
    apply IHxs in H3; clear IHxs.
    rewrite -> H3.
    simpl...
  Qed.

  Lemma map_union : forall (f : A -> B) (xs ys : list A),
    Ordered R xs ->
    Ordered R ys ->
    from_list (map f (union xs ys)) =
    union (from_list (map f xs)) (from_list (map f ys)).
  Proof with auto with datatypes.
    intros.
    induction ys...
    simpl.
    inversion H0.
    subst.
    apply IHys in H4; clear IHys.
    rewrite -> union_insert_r_comm.
    rewrite <- H4.
    rewrite -> from_list_map_cons.
    reflexivity.
    apply union_order_pres...
    inversion H0...
    apply from_list_order.
    apply from_list_order.
  Qed.

  Lemma in_unions_map : forall (b : B) (lst: list A) (f : A -> list B),
    (forall x, Ordered S (f x)) ->
    In b (unions (map f lst)) ->
    exists (a : A), In a lst /\ In b (f a).
  Proof with eauto with datatypes.
    intros.
    induction lst...
    simpl in H0.
    inversion H0.
    simpl in H0.
    apply In_union in H0...
    destruct H0...
    apply IHlst in H0; clear IHlst.
    destruct H0 as [a0 [HIna HInb]]...
    apply unions_order_pres.
    intros.
    rewrite -> in_map_iff in H1.
    destruct H1 as [a0 [HEq HIn]].
    remember (H a0); clear Heqo.
    rewrite -> HEq in o...
  Qed.


  Section AllDiff.
    
    Require Import Common.AllDiff.


    Lemma AllDiff_insert : forall (f : A -> B) x lst,
      Ordered R lst ->
      AllDiff f (insert x lst) ->
      AllDiff f (x :: lst).
    Proof with auto.
      intros.
      induction lst...
      simpl in H0.
      destruct (compare x a)...
      simpl in H0.
      destruct H0 as [H0 H1].
      apply IHlst in H1.
      2: solve [inversion H;trivial].
      simpl in H1.
      destruct H1 as [H1 H2].
      simpl.
      repeat split; intros...
      + destruct H3...
        subst.
        assert (f y <> f x).
        { apply H0. apply in_inserted. }
        unfold not; intros.
        unfold not in H3.
        apply H3...
      + apply H0.
        apply in_inserted_tail...
    Qed.

    Lemma AllDiff_insert_2 : forall (f : A -> B) x lst,
      AllDiff f (x :: lst) ->
      AllDiff f (insert x lst).
    Proof with auto.
      intros.
      induction lst...
      simpl...
      destruct (compare x a)...
      simpl.
      split.
      + intros.
        simpl in H.
        destruct H as [J [J0 J1]]...
        apply In_insert in H0.
        destruct H0; subst.
        - assert (f x <> f a).
          apply J...
          unfold not. unfold not in H. intros. symmetry in H0. 
          apply H in H0...
        - apply J0...
      + apply IHlst...
        simpl in H.
        destruct H as [J [J0 J1]]...
        simpl...
    Qed.

    Lemma AllDiff_preservation : forall (f : A -> B) x y lst,
      Ordered R lst ->
      Ordered R lst ->
      AllDiff f (union [x] lst) ->
      f x = f y ->
      AllDiff f (union [y] lst).
    Proof with auto with datatypes.
      intros.
      induction lst...
      + simpl...
      + simpl in H1.
        simpl.
        inversion H; subst.
        remember H1 as X eqn:Y; clear Y.
        apply AllDiff_insert in H1.
        inversion H1; subst...
        apply IHlst in H4...
        clear IHlst.
        rewrite <- union_singleton_l in *...
        apply AllDiff_insert_2.
        simpl. split...
        intros.
        apply In_insert in H7.
        { destruct H7.
          - subst...
            rewrite <- H2.
            apply H3.
            apply in_inserted.
          - apply H3...
            apply in_inserted_tail... }
        apply union_order_pres...
        apply Ordered_cons... intros. simpl in H3. inversion H3.
        apply Ordered_nil.
    Qed.

  End AllDiff.
End BinaryLemmas.

  
    
    
