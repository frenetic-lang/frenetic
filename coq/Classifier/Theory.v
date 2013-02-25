Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.CpdtTactics.
Require Import Common.Types.
Require Import Network.Packet.
Require Import Pattern.Pattern.
Require Import Classifier.Classifier.

Local Open Scope list_scope.
Local Open Scope equiv_scope.

Section Equivalence.

  Definition Classifier_equiv {A : Type} (cf1 cf2 : Classifier A) :=
    forall pt pk def, scan def cf1 pt pk = scan def cf2 pt pk.

  Lemma Classifier_equiv_is_Equivalence : forall {A : Type},
    Equivalence (@Classifier_equiv A).
  Proof with auto.
    intros.
    split.
    unfold Reflexive.
    unfold Classifier_equiv...
    unfold Symmetric.
    unfold Classifier_equiv...
    unfold Transitive.
    unfold Classifier_equiv.
    intros.
    rewrite -> H...
  Qed.

End Equivalence.

Instance Classifier_Equivalance `(A : Type) : 
  Equivalence (@Classifier_equiv A).
Proof.
  apply Classifier_equiv_is_Equivalence.
Qed.

Class ClassifierAction `(A : Type) := {
  action_eqdec : forall (x y : A), { x = y } + { x <> y };
  action_unit : A
}.

Definition has_unit {A : Type} {Act : ClassifierAction A} 
  (f : A -> A -> A) : Prop :=
  (forall a, f a action_unit = a) /\ forall a, f action_unit a = a.


Inductive total (A : Type) : Classifier A -> Prop :=
| total_tail : forall a cf,
  total (cf ++ [(Pattern.all, a)]).

Section Lemmas.

  Hint Constructors total.

  Variable A B : Type.

  Lemma scan_map_comm : forall (f : A -> B) (defA : A) (defB : B) cf pt pk,
    total cf ->
    scan defB (map (second f) cf) pt pk = f (scan defA cf pt pk).
  Proof with auto.
    intros f defA defB cf pt pk H.
    inversion H.
    generalize dependent cf0.
    induction cf; intros.
    simpl.
    destruct cf0. simpl in H0. inversion H0.
    rewrite <- app_comm_cons in H0. inversion H0.
    (* Inductive case *)
    intros.
    destruct cf0.
    simpl...
    destruct (Pattern.match_packet pt pk Pattern.all)...
      admit. (* how was this ever true? *)
    destruct a0 as [pat a0].
    destruct p as [pat' a'].
    simpl.
    remember (Pattern.match_packet pt pk pat') as Hm.
    destruct Hm...
    rewrite <- app_comm_cons in H0.
    inversion H0.
    apply IHcf...
    rewrite <- H4...
  Qed.
  
  Lemma scan_elim_unit_tail : forall (def : A) pk pt cf pat,
    scan def (cf ++ [(pat, def)]) pt pk = scan def cf pt pk.
  Proof with auto.
    intros.
    induction cf.
    simpl.
    destruct (Pattern.match_packet pt pk pat)...
    destruct a as [pat' a].
    simpl.
    destruct (Pattern.match_packet pt pk pat')...
  Qed.

  Lemma scan_inv : forall (def : A) pkt port
    (N1 : Classifier A),
    ((forall m a,  In (m,a) N1 -> Pattern.match_packet port pkt  m = false) /\
      scan def N1 port pkt = def) \/
    (exists N2 N3, exists m : pattern, exists a : A,
      N1 = N2 ++ (m,a)::N3 /\
      Pattern.match_packet port pkt m  = true /\
      scan def N1 port pkt = a /\
      (forall (m' : pattern) (a' : A), In (m',a') N2 -> 
        Pattern.match_packet port pkt  m' = false)).
  Proof with intros; simpl; auto with datatypes.
    intros.
    induction N1.
    (* Base case *)
    intros.
    left...
    split...
    contradiction.
    (* Inductive case *)
    destruct a.
    destruct IHN1.
    (* Case 1 *)
    remember (Pattern.match_packet port pkt p) as b.  destruct b.
    right. exists nil. exists N1. exists p. exists a.
    crush. rewrite <- Heqb...
    (* Case 2 *)
    left. crush. apply H0 in H2. crush. rewrite <- Heqb...
    (* Case 3 *)
    destruct H as [N2 [N3 [m [a' [Neq  [Hov [Ha'eq H]]]]]]].
    remember (Pattern.match_packet port pkt p) as b. destruct b. 
    right. exists nil. exists N1. exists p. exists a. crush. rewrite <- Heqb...
    right. exists ((p,a) :: N2). exists N3. exists m. exists a'.
    crush. rewrite <- Heqb... apply H in H1. crush.
  Qed.

  Hint Unfold union inter inter_entry.

  Variable f : A -> A -> A.
  Variable def : A.
  Variable cf : Classifier A.

  Lemma inter_nil_l : inter f nil cf = nil.
  Proof. intros. induction cf; crush. Qed.

  Lemma inter_nil_r : inter f cf nil = nil.
  Proof. intros. induction cf; crush. Qed.
  
  Hint Resolve inter_nil_l inter_nil_r.

  Lemma elim_scan_head : forall cf1 cf2 pkt pt,
    (forall m a, In (m,a) cf1 -> Pattern.match_packet pt pkt m = false) ->
    scan def (cf1 ++ cf2) pt pkt = scan def cf2 pt pkt.
  Proof with simpl; auto with datatypes.
    intros.
    induction cf1...
    destruct a as [m a].
    assert (forall m a', In (m,a') cf1 -> Pattern.match_packet pt pkt m = false).
      intros. apply H with (a0 := a')...
    apply IHcf1 in H0.
    assert (Pattern.match_packet pt pkt m = false).
      assert (In (m,a) ((m,a)::cf1))... 
      apply H in H1...
    rewrite -> H1...
  Qed.

  Hint Resolve elim_scan_head.

  Lemma elim_scan_middle : forall cf1 cf2 cf3 pkt pt,
    (forall m (a : A), In (m,a) cf2 -> Pattern.match_packet pt pkt m = false) ->
    scan def (cf1 ++ cf2 ++ cf3) pt pkt = scan def (cf1 ++ cf3) pt pkt.
  Proof.
    intros.
    generalize dependent cf2.
    induction cf1; crush.
  Qed.
    
  Hint Resolve elim_scan_middle.

  Lemma elim_scan_tail : forall cf1 cf2 cf3 pat a pt pk,
    Pattern.match_packet pt pk pat = true ->
    scan def (cf1 ++ (pat, a) :: cf2 ++ cf3) pt pk = 
    scan def (cf1 ++ (pat, a) :: cf2) pt pk.
  Proof with auto.
    intros.
    induction cf1.
    simpl.
    rewrite -> H...
    destruct a0 as [pat0 a0].
    simpl.
    remember (Pattern.match_packet pt pk pat0) as b.
    destruct b...
  Qed.

  Lemma elim_inter_head : forall cf1 cf2 pt pkt m a,
    Pattern.match_packet pt pkt m = false ->
    scan def
    (fold_right
      (fun (v' : pattern * A) (acc : list (pattern * A)) =>
        let (pat', act') := v' in (Pattern.inter m pat', f a act') :: acc) 
      nil cf1 ++ cf2) pt pkt = scan def cf2 pt pkt.
  Proof with auto.
    intros.
    induction cf1...
    destruct a0 as [p0 a0].
    simpl.
    rewrite -> Pattern.is_match_false_inter_l...
  Qed.

  Hint Resolve elim_inter_head.

  Lemma elim_inter_head_aux : forall cf1 cf2 pkt pt m (a : A),
    Pattern.match_packet pt pkt m = false ->
    scan def (inter_entry f cf1 (m, a) ++ cf2) pt pkt = scan def cf2 pt pkt.
  Proof with auto.
    intros.
    induction cf1.
    crush.
    destruct a0 as [p0 a0].
    simpl.
    rewrite -> Pattern.is_match_false_inter_l...
  Qed.
    
  Lemma inter_empty_aux : forall N1 m m0 pkt pt (a a0 : A),
    (forall m  (a : A), In (m,a) N1 -> Pattern.match_packet pt pkt m = false) ->
    In (m,a) (inter_entry f N1 (m0,a0)) ->
    Pattern.match_packet pt pkt m = false.
  Proof with auto with datatypes.
    intros.
    induction N1.
    crush.
    destruct a1.
    simpl in H0.
    destruct H0.
    (* contra *)
    unfold inter_entry in H0. inversion H0. subst.
    assert (Pattern.match_packet pt pkt t = false). apply H with (a := a1)...
    apply Pattern.no_match_subset_r...
  (* inductive *)
    assert (forall m' (a' : A), In (m',a') N1 ->
      Pattern.match_packet pt pkt m' = false).
    intros. 
    assert (In (m',a') ((t,a1)::N1))...
    apply H in H2...
    apply IHN1 in H1...
  Qed.

  Lemma inter_empty : forall N2 pkt pt,
    (forall m (a : A), In (m,a) N2 -> Pattern.match_packet pt pkt m = false) ->
    (forall N1 m (a : A), In (m,a) (inter f N1 N2) -> 
      Pattern.match_packet pt pkt m = false).
  Proof with auto with datatypes.
    intros N2 pkt pt.
    intros Hlap.
    intros.
    generalize dependent N2.
    induction N1.
    crush.
    (* Inductive *)
    destruct a0.
    intros.
    simpl in H.
    rewrite -> in_app_iff in H.
    destruct H.
    apply inter_empty_aux with (N1 := N2) (m0 := p)  (a := a) (a0 := a0)...
    apply IHN1 in Hlap...
  Qed.

  Hint Resolve inter_empty scan_inv.
  Hint Rewrite in_app_iff.
  
End Lemmas.

Section Optimizer.

  Lemma elim_shadowed_equiv : forall {A : Type} 
    pat1 pat2 act1 act2 (cf1 cf2 cf3 : Classifier A),
    Pattern.equiv pat1 pat2 ->
    Classifier_equiv 
      (cf1 ++ (pat1,act1) :: cf2 ++ (pat2,act2) :: cf3)
      (cf1 ++ (pat1,act1) :: cf2 ++ cf3).
  Proof with auto.
    intros.
    unfold Classifier_equiv.
    intros.
    remember (Pattern.match_packet pt pk pat1) as Hmatched.
    destruct Hmatched.
    assert (scan def (cf1 ++ (pat1,act1) :: cf2 ++ (pat2,act2) :: cf3) pt pk =
      scan def (cf1 ++ (pat1,act1) :: cf2) pt pk).
    apply elim_scan_tail...
    rewrite -> H0.
    assert (scan def (cf1 ++ (pat1,act1) :: cf2 ++ cf3) pt pk =
      scan def (cf1 ++ (pat1,act1) :: cf2) pt pk).
    apply elim_scan_tail...
    rewrite -> H1...
    (* Did not match *)
    assert (false = Pattern.match_packet pt pk pat2) as Hpat2Unmatched.
      rewrite -> HeqHmatched.
      unfold equiv in H...
    assert ((pat2,act2) :: cf3 = [(pat2,act2)] ++ cf3) as J0 by auto.
    rewrite -> J0.
    assert (cf1 ++ (pat1,act1) :: cf2 ++ [(pat2,act2)] ++ cf3 = 
      (cf1 ++ (pat1,act1) :: cf2) ++ [(pat2,act2)] ++ cf3) as J1.
    rewrite <- app_assoc...
    rewrite -> J1.
    assert (cf1 ++ (pat1,act1) :: cf2 ++ cf3 = 
      (cf1 ++ (pat1,act1) :: cf2) ++ cf3) as J2.
    rewrite <- app_assoc...
    rewrite -> J2.
    apply elim_scan_middle.
    exact (fun x y => x). (* TODO(arjun): stupid stupid *)
    intros.
    inversion H0.
    inversion H1.
    subst...
    inversion H1.
  Qed.

  Lemma elim_shadowed_helper_ok : forall {A : Type} 
    (prefix postfix : Classifier A),
    Classifier_equiv
      (prefix ++ postfix) (elim_shadowed_helper prefix postfix).
  Proof with auto.
    intros.
    unfold Classifier_equiv.
    generalize dependent prefix.
    induction postfix; intros.
    simpl.
    rewrite -> app_nil_r...
    (* Inductive case *)
    destruct a as [pat act].
    simpl.
    match goal with
      | [ |- context[if ?b then _ else _] ] => remember b
    end.
    destruct b.
    Focus 2.
    assert ((pat,act) :: postfix = [(pat,act)] ++ postfix) as Hfoo by auto.
    rewrite -> Hfoo.
    rewrite -> app_assoc.
    apply IHpostfix.
    (* Hard case *)
    symmetry in Heqb.
    rewrite -> existsb_exists in Heqb.
    destruct Heqb as [[pat' act'] [HIn Heq]].
    assert (scan def (prefix ++ (pat,act) :: postfix) pt pk =
      scan def (prefix ++ postfix) pt pk) as Hit.
    apply In_split in HIn.
    destruct HIn as [l1 [l2 HIn]].
    rewrite -> HIn.
    rewrite <- app_assoc.
    rewrite <- app_assoc.
    simpl.
    apply elim_shadowed_equiv.
    remember (Pattern.beq pat pat') as b.
    destruct b.
    symmetry in Heqb.
    apply Pattern.beq_true_spec in Heqb.
    unfold Coq.Classes.Equivalence.equiv in Heqb.
    apply symmetry...
    inversion Heq.
    rewrite -> Hit.
    apply IHpostfix.
  Qed.

  Theorem elim_shadowed_ok : forall {A : Type} (cf : Classifier A),
    cf === elim_shadowed cf.
  Proof with auto.
    intros.     
    unfold elim_shadowed.
    assert (nil ++ cf = cf) as J0...
    rewrite <- J0.
    apply elim_shadowed_helper_ok.
  Qed.

End Optimizer.

Section Action.
  
  Variable A : Type.
  Variable A_as_Action : ClassifierAction A.
  
  Implicit Arguments A.
  Implicit Arguments A_as_Action.
  
  Definition left_biased (a b : A) := 
    match action_eqdec a action_unit with
      | left _ => b
      | right _ => a
    end.
  
  Lemma left_biased_has_unit : has_unit left_biased.
  Proof with auto.
    unfold left_biased.
    split; intros.
    remember (action_eqdec a action_unit) as b.
    destruct b...
    remember (action_eqdec action_unit action_unit) as b.
    destruct b...
    contradiction n...
  Qed.

  Hint Resolve left_biased_has_unit.
  
  Hint Constructors total.
  
  
  Lemma inter_entry_app : forall cf1 cf2 m (a : A) (f : A -> A -> A),
    inter_entry f (cf1 ++ cf2) (m,a) = 
    inter_entry f cf1 (m,a) ++ inter_entry f cf2 (m,a).
  Proof with auto.
    intros.
    induction cf1...
    destruct a0.
    simpl. f_equal...
  Qed.

  Lemma union_scan_comm : forall (f : A -> A -> A) pt pk cf1 cf2,
    has_unit f ->
    scan action_unit (union f cf1 cf2) pt pk = 
    f (scan action_unit cf1 pt pk) (scan action_unit cf2 pt pk).
  Proof with simpl; eauto with datatypes.
    intros f pt pk cf1 cf2 H.
    remember H as Hwb.
    destruct H as [H H0].
    induction cf1.
      (* Base case *)
    rewrite -> H0... 
    (* Inductive case *)
    unfold union.
    destruct a as [m a].
    remember (Pattern.match_packet pt pk m).
    remember (scan_inv action_unit pk pt (inter f ((m, a) :: cf1) cf2 ++ ((m, a) :: cf1) ++ cf2)) as H1. clear HeqH1.
    destruct H1.
    destruct H1 as [H1 H2].
    (* Case: scan falls off the table. *)
    rewrite -> H2.
    assert (Pattern.match_packet pt pk m = false) as HnotA.
    apply H1 with (a0 := a)...
    simpl in H2.
    rewrite <- app_assoc in H2.
    rewrite -> elim_inter_head in H2...
    assert ((m,a) :: cf1 ++ cf2 = [(m,a)] ++ cf1 ++ cf2) as Hcf. auto.
    rewrite -> Hcf in H2.
    rewrite -> elim_scan_middle in H2.
    rewrite -> HnotA.
    rewrite <- IHcf1.
    unfold union...
    exact f.
    intros. inversion H3. inversion H4. subst... inversion H4.
    (* Case: scan does not fall off. *)
    destruct H1 as [cf3 [cf4 [m0 [a0 [H1 [H2 [H3 H4]]]]]]]. 
    destruct b.
    clear IHcf1.
    rewrite <- app_comm_cons.
    (* Case where pkt is in m *)
    remember (scan_inv action_unit pk pt cf2) as Hinv. clear HeqHinv.
    destruct Hinv as [[H5 H6]|Hinv].
    rewrite -> H6.
    assert (forall m'  (a' : A), In (m',a') (inter f ((m, a) :: cf1) cf2) ->
      Pattern.match_packet pt pk m' = false) as H7.
    apply inter_empty; auto.
    assert (scan action_unit (inter f ((m, a) :: cf1) cf2 ++ 
      (m, a) :: cf1 ++ cf2) pt pk =
    scan action_unit ((m,a) :: cf1 ++ cf2) pt pk) as HelimHd.
    apply elim_scan_head; auto.
    rewrite -> HelimHd.
    assert ((m,a) :: cf1 ++ cf2 = 
      nil ++ (m,a) :: cf1 ++ cf2) as HNilHd by auto.
    rewrite -> HNilHd.
    rewrite -> elim_scan_tail.
    rewrite -> app_nil_l.
    rewrite -> H.
    reflexivity.
    auto.
    destruct Hinv as [N2' [N3' [m' [a' [Heq' [Hlap' [Hscan' Hlap2']]]]]]].
    match goal with
      | [ |- ?X = ?Y ] => remember Y as RHS end.
    assert (RHS = f a a') as HRHS.
    rewrite -> HeqRHS.
    simpl.
    rewrite <- Heqb.
    rewrite -> Hscan'...
    simpl.
    match goal with
      | [ |- context[fold_right ?f ?acc ?lst]] => remember (fold_right f acc lst) as F
    end.
    rewrite <- app_assoc.
    remember (inter f cf1 cf2 ++ (m,a) :: cf1 ++ cf2) as Trash.
    assert (forall m5 (a5 : A), 
      In (m5,a5) (fold_right
        (fun (v' : pattern * A) (acc : list (pattern * A)) =>
          let (pat', act') := v' in (Pattern.inter m pat', f a act') :: acc) 
        nil N2')  ->
      Pattern.match_packet pt pk m5  = false) as HOMG.
    match goal with
      | [ |- context[fold_right ?f ?acc ?lst]] => remember (fold_right f acc lst) as F1
    end.
    assert (F1 = inter f [(m,a)] N2') as HF1.
    simpl. rewrite -> app_nil_r. rewrite -> HeqF1...
    rewrite -> HF1.
    apply inter_empty; auto.
    rewrite -> Heq' in HeqF.
    assert (F = inter_entry f (N2' ++ (m',a') :: N3') (m,a)).
    rewrite -> HeqF. simpl. auto.
    assert ( (fold_right
      (fun (v' : pattern * A) (acc : list (pattern * A)) =>
        let (pat', act') := v' in (Pattern.inter m pat', f a act') :: acc)
      nil N2') = inter_entry f N2' (m,a)). simpl. auto.
    rewrite -> H6 in HOMG.
    rewrite -> H5.
    rewrite -> inter_entry_app.
    rewrite <- app_assoc.
    rewrite -> elim_scan_head.
    simpl.
    rewrite -> Pattern.is_match_true_inter...
    auto.
    
    assert ((m,a) :: cf1 = [(m,a)] ++ cf1) as Hsimpl. auto.
    rewrite -> Hsimpl. 
    rewrite <- app_assoc.  
    rewrite -> elim_scan_middle with (cf2 := [(m,a)]).
    rewrite <- Hsimpl. clear Hsimpl.
    simpl.
    rewrite <- app_assoc.  
    rewrite -> elim_inter_head.
    rewrite <- Heqb.
    unfold union in IHcf1.
    trivial.
    auto.
    exact f.
    intros. inversion H5. inversion H6. subst... inversion H6.
  Qed.

  Lemma prefix_equivalence : forall cf1 cf2 pt pk,
    scan unit cf1 pt pk = scan unit (cf1 ++ cf2) pt pk \/
    scan unit cf1 pt pk = unit.
  Proof with auto.
    intros cf1 cf2 pt pk.
    induction cf1.
    right...
    destruct a as [pat a].
    simpl.
    remember (Pattern.match_packet pt pk pat) as b.
    destruct b.
    left...
    exact IHcf1.
  Qed.

End Action.
