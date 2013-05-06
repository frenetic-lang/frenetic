Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.CpdtTactics.
Require Import Common.Types.
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import Classifier.ClassifierSignatures.
Require Classifier.ClassifierImpl.

Import ListNotations.
Local Open Scope list_scope.
Local Open Scope equiv_scope.

Module Make 
       (Action_ : ACTION) 
       (MakeActionSpec : MAKE_ACTION_SPEC).

  Module Action := Action_.
  Module ActionSpec := MakeActionSpec (Action).
  Module Classifier := Classifier.ClassifierImpl.Make (Action).

  Import Classifier.
  Import ActionSpec.

  Definition action := Action.t.

  Definition Classifier_equiv (cf1 cf2 : t) :=
    forall pt pk def, scan' def cf1 pt pk = scan' def cf2 pt pk.
  
  Lemma Classifier_equiv_is_Equivalence : Equivalence Classifier_equiv.
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

  Instance Classifier_Equivalance : Equivalence Classifier_equiv := Classifier_equiv_is_Equivalence.

  Inductive total : t -> Prop :=
  | total_tail : forall a cf, total (cf ++ [(Pattern.all, a)]).

  Section Lemmas.

    Hint Constructors total.

    Lemma scan_elim_unit_tail : forall (def : action) pk pt cf pat,
                                  scan' def (cf ++ [(pat, def)]) pt pk = scan' def cf pt pk.
    Proof with auto.
      intros.
      induction cf.
      simpl.
      destruct (Pattern.match_packet pt pk pat)...
      destruct a as [pat' a].
      simpl.
      destruct (Pattern.match_packet pt pk pat')...
    Qed.

    Lemma scan_inv :
      forall (def : action) pkt port
             (N1 : t),
        ((forall m a,  In (m,a) N1 -> Pattern.match_packet port pkt  m = false) /\
         scan' def N1 port pkt = def) \/
        (exists N2 N3, 
           exists m : pattern, 
             exists a : action,
               N1 = N2 ++ (m,a)::N3 /\
               Pattern.match_packet port pkt m  = true /\
               scan' def N1 port pkt = a /\
               (forall (m' : pattern) (a' : action), 
                  In (m',a') N2 -> 
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

    Hint Unfold union_no_opt inter_no_opt inter_entry.

    Variable def : action.
    Variable cf : t.

    Lemma inter_nil_l : inter_no_opt nil cf = nil.
    Proof. intros. induction cf; crush. Qed.

    Lemma inter_nil_r : inter_no_opt cf nil = nil.
    Proof. intros. induction cf; crush. Qed.
    
    Hint Resolve inter_nil_l inter_nil_r.

    Lemma elim_scan_head : 
      forall cf1 cf2 pkt pt,
        (forall m a, In (m,a) cf1 -> Pattern.match_packet pt pkt m = false) ->
        scan' def (cf1 ++ cf2) pt pkt = scan' def cf2 pt pkt.
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

    Lemma elim_scan_middle : 
      forall cf1 cf2 cf3 pkt pt,
        (forall m (a : action), In (m,a) cf2 -> Pattern.match_packet pt pkt m = false) ->
        scan' def (cf1 ++ cf2 ++ cf3) pt pkt = scan' def (cf1 ++ cf3) pt pkt.
    Proof.
      intros.
      generalize dependent cf2.
      induction cf1; crush.
    Qed.
    
    Hint Resolve elim_scan_middle.

    Lemma elim_scan_tail : 
      forall cf1 cf2 cf3 pat a pt pk,
        Pattern.match_packet pt pk pat = true ->
        scan' def (cf1 ++ (pat, a) :: cf2 ++ cf3) pt pk = 
        scan' def (cf1 ++ (pat, a) :: cf2) pt pk.
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

    Lemma elim_inter_head : 
      forall cf1 cf2 pt pkt m a,
        Pattern.match_packet pt pkt m = false ->
        scan' def
              (fold_right
                 (fun (v' : pattern * action) (acc : list (pattern * action)) =>
                    let (pat', act') := v' in (Pattern.inter m pat', Action.par_action a act') :: acc) 
                 nil cf1 ++ cf2) pt pkt = scan' def cf2 pt pkt.
    Proof with auto.
      intros.
      induction cf1...
      destruct a0 as [p0 a0].
      simpl.
      rewrite -> Pattern.is_match_false_inter_l...
    Qed.

    Hint Resolve elim_inter_head.

    Lemma elim_inter_head_aux :
      forall cf1 cf2 pkt pt m (a : action),
        Pattern.match_packet pt pkt m = false ->
        scan' def (inter_entry cf1 (m, a) ++ cf2) pt pkt = scan' def cf2 pt pkt.
    Proof with auto.
      intros.
      induction cf1.
      crush.
      destruct a0 as [p0 a0].
      simpl.
      rewrite -> Pattern.is_match_false_inter_l...
    Qed.
    
    Lemma inter_empty_aux :
      forall N1 m m0 pkt pt (a a0 : action),
        (forall m  (a : action), In (m,a) N1 -> Pattern.match_packet pt pkt m = false) ->
        In (m,a) (inter_entry N1 (m0,a0)) ->
        Pattern.match_packet pt pkt m = false.
    Proof with auto with datatypes.
      intros.
      induction N1.
      + crush.
      + destruct a1.
        simpl in H0.
        destruct H0.
      - inversion H0; subst; clear H0.
        apply Pattern.no_match_subset_r...
        eapply H...
      - apply IHN1...
        intros. eapply H... simpl. right. exact H1.
    Qed.

    Lemma inter_empty : 
      forall N2 pkt pt,
        (forall m (a : action), In (m,a) N2 -> Pattern.match_packet pt pkt m = false) ->
        (forall N1 m (a : action), In (m,a) (inter_no_opt N1 N2) -> 
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

  End Lemmas.

  Section Optimizer.

    Lemma elim_shadowed_equiv : 
      forall pat1 pat2 act1 act2 (cf1 cf2 cf3 : t),
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
      assert (scan' def (cf1 ++ (pat1,act1) :: cf2 ++ (pat2,act2) :: cf3) pt pk =
              scan' def (cf1 ++ (pat1,act1) :: cf2) pt pk).
      apply elim_scan_tail...
      rewrite -> H0.
      assert (scan' def (cf1 ++ (pat1,act1) :: cf2 ++ cf3) pt pk =
              scan' def (cf1 ++ (pat1,act1) :: cf2) pt pk).
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
      intros.
      inversion H0.
      inversion H1.
      subst...
      inversion H1.
    Qed.

    Lemma elim_shadowed_helper_ok : 
      forall (prefix postfix : t),
        Classifier_equiv (prefix ++ postfix) (elim_shadowed_helper prefix postfix).
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
      + (* Hard case *)
        symmetry in Heqb.
        rewrite -> existsb_exists in Heqb.
        destruct Heqb as [[pat' act'] [HIn Heq]].
        assert (scan' def (prefix ++ (pat,act) :: postfix) pt pk =
                scan' def (prefix ++ postfix) pt pk) as Hit.
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
      + assert ((pat,act) :: postfix = [(pat,act)] ++ postfix) as Hfoo by auto.
        rewrite -> Hfoo.
        rewrite -> app_assoc.
        apply IHpostfix.

    Qed.

    Theorem elim_shadowed_ok : forall (cf : t), cf === elim_shadowed cf.
    Proof with auto.
      intros.     
      unfold elim_shadowed.
      assert (nil ++ cf = cf) as J0...
      rewrite <- J0.
      apply elim_shadowed_helper_ok.
    Qed.

    Lemma opt_spec : forall tbl pt pk, scan (opt tbl) pt pk = scan tbl pt pk.
    Proof with auto.
      intros.
      unfold opt.
      remember (elim_shadowed_ok tbl) as H eqn:X; clear X.
      unfold equiv in H.
      unfold Classifier_equiv in H.
      unfold scan...
    Qed.

  End Optimizer.

  Lemma scan_app_compose : 
    forall pt pk (z : action) lst1 lst2,
      scan' z (lst1 ++ lst2) pt pk = scan' (scan' z lst2 pt pk) lst1 pt pk.
  Proof with auto with datatypes.
    intros.
    induction lst1...
    destruct a.
    simpl.
    rewrite -> IHlst1...
  Qed.

  Hint Constructors total.
  
  Lemma inter_entry_app : 
    forall cf1 cf2 m (a : action) ,
      inter_entry (cf1 ++ cf2) (m,a) = 
      inter_entry cf1 (m,a) ++ inter_entry cf2 (m,a).
  Proof with auto.
    intros.
    induction cf1...
    destruct a0.
    simpl. f_equal...
  Qed.

  Lemma union_no_opt_spec : 
    forall pt pk cf1 cf2,
      scan (union_no_opt cf1 cf2) pt pk = 
      Action.par_action (scan cf1 pt pk) (scan cf2 pt pk).
  Proof with simpl; eauto with datatypes.
    intros pt pk cf1 cf2.
    unfold scan.
    induction cf1.
    (* Base case *)
    simpl.
    rewrite -> ActionSpec.par_drop_l...
    (* Inductive case *)
    unfold union_no_opt.
    destruct a as [m a].
    remember (Pattern.match_packet pt pk m).
    remember 
      (scan_inv 
         Action.drop pk pt 
         (inter_no_opt ((m, a) :: cf1) cf2 ++ ((m, a) :: cf1) ++ cf2)) as H1; clear HeqH1.
    destruct H1 as [H1| H1].
    + destruct H1 as [H1 H2].
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
      intros.
      simpl in H. inversion H. inversion H0; subst... inversion H0.
    (* Case: scan does not fall off. *)
    + destruct H1 as [cf3 [cf4 [m0 [a0 [H1 [H2 [H3 H4]]]]]]]. 
      destruct b.
      clear IHcf1.
      rewrite <- app_comm_cons.
      (* Case where pkt is in m *)
      remember (scan_inv Action.drop pk pt cf2) as Hinv eqn:X; clear X.
      destruct Hinv as [[H5 H6]|Hinv].
      rewrite -> H6.
      assert (forall m'  (a' : action),
                In (m',a') (inter_no_opt ((m, a) :: cf1) cf2) ->
                Pattern.match_packet pt pk m' = false) as H7.
      apply inter_empty; auto.
      assert (scan' Action.drop (inter_no_opt ((m, a) :: cf1) cf2 ++ 
                                       (m, a) :: cf1 ++ cf2) pt pk =
              scan' Action.drop ((m,a) :: cf1 ++ cf2) pt pk) as HelimHd.
      apply elim_scan_head; auto.
      rewrite -> HelimHd.
      assert ((m,a) :: cf1 ++ cf2 = 
              nil ++ (m,a) :: cf1 ++ cf2) as HNilHd by auto.
      rewrite -> HNilHd.
      rewrite -> elim_scan_tail.
      rewrite -> app_nil_l.
      rewrite -> ActionSpec.par_drop_r...
      auto.
      destruct Hinv as [N2' [N3' [m' [a' [Heq' [Hlap' [Hscan' Hlap2']]]]]]].
      match goal with
        | [ |- ?X = ?Y ] => remember Y as RHS end.
      assert (RHS = Action.par_action a a') as HRHS.
      rewrite -> HeqRHS.
      simpl.
      rewrite <- Heqb.
      rewrite -> Hscan'...
      simpl.
      match goal with
        | [ |- context[fold_right ?f ?acc ?lst]] => remember (fold_right f acc lst) as F
      end.
      rewrite <- app_assoc.
      remember (inter_no_opt cf1 cf2 ++ (m,a) :: cf1 ++ cf2) as Trash.
      assert
        (forall m5 (a5 : action), 
           In (m5,a5) (fold_right
                         (fun (v' : pattern * action) (acc : list (pattern * action)) =>
                            let (pat', act') := v' in (Pattern.inter m pat', 
                                                       Action.par_action a act') :: acc) 
                         nil N2')  ->
           Pattern.match_packet pt pk m5  = false) as HOMG.
      match goal with
        | [ |- context[fold_right ?f ?acc ?lst]] => remember (fold_right f acc lst) as F1
      end.
      assert (F1 = inter_no_opt [(m,a)] N2') as HF1.
      simpl. rewrite -> app_nil_r. rewrite -> HeqF1...
      rewrite -> HF1.
      apply inter_empty; auto.
      rewrite -> Heq' in HeqF.
      assert (F = inter_entry (N2' ++ (m',a') :: N3') (m,a)).
      rewrite -> HeqF. simpl. auto.
      assert ( (fold_right
                  (fun (v' : pattern * action) (acc : list (pattern * action)) =>
                     let (pat', act') := v' in (Pattern.inter m pat', Action.par_action a act') :: acc)
                  nil N2') = inter_entry N2' (m,a)).
      { simpl. auto. }
      rewrite -> H0 in HOMG.
      rewrite -> H.
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
      { intros.
        simpl in H. inversion H. inversion H0. subst... inversion H0. }
  Qed.

  Lemma union_spec : 
    forall pt pk cf1 cf2,
      scan (union cf1 cf2) pt pk = 
      Action.par_action (scan cf1 pt pk) (scan cf2 pt pk).
  Proof.
    intros.
    unfold union.
    rewrite -> opt_spec.
    apply union_no_opt_spec.
  Qed.

  Lemma prefix_equivalence : 
    forall cf1 cf2 pt pk,
      scan' Action.drop cf1 pt pk = scan' Action.drop (cf1 ++ cf2) pt pk \/
      scan' Action.drop cf1 pt pk = Action.drop.
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

  Import Action.
  Import ActionSpec.

  Section Scan.

    Lemma match_inter_false : 
      forall pt pk tbl1 tbl2,
        (forall pat act, In (pat,act) tbl1 -> Pattern.match_packet pt pk pat = false) ->
        (forall pat act, In (pat,act) tbl2 -> Pattern.match_packet pt pk pat = false) ->
        forall pat act, In (pat,act) (inter_no_opt tbl1 tbl2) -> Pattern.match_packet pt pk pat = false.
    Proof with auto with datatypes.
      intros.
      simpl in H1.
      induction tbl1...
      eauto.
      destruct a as [pat0 act0].
      simpl in H1.
      simpl in H.
      rewrite -> in_app_iff in H1.
      destruct H1 as [H1|H1].
      + clear IHtbl1.
        induction tbl2...
        * eauto.
        * destruct a as [pat1 act1].
          simpl in H1.
          { destruct H1.
            + inversion H1; subst; clear H1.
              rewrite -> Pattern.no_match_subset_r...
              eapply H0.
              simpl.
              left...
            + apply IHtbl2...
              intros.
              eapply H0.
              simpl.
              right.
              exact H2. }
      + apply IHtbl1...
        intros.
        eapply H. right. exact H2.
    Qed.

    Lemma match_union_false : 
      forall pt pk tbl1 tbl2,
        (forall pat act, In (pat,act) tbl1 -> Pattern.match_packet pt pk pat = false) ->
        (forall pat act, In (pat,act) tbl2 -> Pattern.match_packet pt pk pat = false) ->
        forall pat act, In (pat,act) (union_no_opt tbl1 tbl2) -> Pattern.match_packet pt pk pat = false.
    Proof with eauto with datatypes.
      intros.
      unfold union_no_opt in H1.
      rewrite -> in_app_iff in H1.
      rewrite -> in_app_iff in H1.
      destruct H1 as [H1 | [H1 | H1]]...
      eapply match_inter_false with (tbl1 := tbl1) (tbl2 := tbl2)...
    Qed.

  End Scan.


    Lemma apply_drop_spec : forall pt pk, apply_action drop (pt,pk) = nil.
    Proof with auto with datatypes.
      intros. unfold apply_action. rewrite -> atoms_drop...
    Qed.


    Lemma seq_nil_l : forall tbl2 pt pk, seq nil tbl2 pt pk = drop.
    Proof with auto with datatypes.
      intros.
      unfold seq.
      induction tbl2; simpl...
      + rewrite -> apply_drop_spec. simpl. rewrite -> seq_drop_r...
      + rewrite -> apply_drop_spec. simpl. rewrite -> seq_drop_r...
    Qed.

    Lemma Pick_false : 
      forall pt pk pat act atom tbl,
        false = Pattern.match_packet pt pk pat ->
        forall pat0 act0,
          In (pat0, act0) (Pick pat act atom tbl) ->
          false = Pattern.match_packet pt pk pat0.
    Proof with auto with datatypes.
      intros.
      induction tbl.
      + simpl in H0. inversion H0.
      + destruct a as [p a]. 
        simpl in H0.
        destruct H0 as [H0|H0]...
        inversion H0; subst; clear H0.
        rewrite -> Pattern.is_match_false_inter_l...
    Qed.

    Lemma total_singleton : forall pat act, total [(pat,act)] -> pat = Pattern.all.
    Proof with auto with datatypes.
      intros.
      inversion H; subst.
      destruct cf.
      + simpl in H1; inversion H1...
      + simpl in H1. inversion H1; subst.
        destruct cf; simpl in H3; inversion H3.
    Qed.

    Hint Constructors total.

    Lemma total_pop : forall a1 a2 tbl, total (a1 :: a2 :: tbl) -> total (a2 :: tbl).
    Proof with auto with datatypes.
      intros.
      destruct a1 as [p1 a1].
      destruct a2 as [p2 a2].
      induction tbl...
      + inversion H; subst.
        destruct cf.
        * simpl in H1. inversion H1.
        * destruct cf.
          simpl in H1.
          destruct p as [p1' a1'].
          inversion H1; subst.
          rewrite <- app_nil_l...
          simpl in H1. inversion H1.
          destruct cf; simpl in H4; inversion H4.
      + 
    Admitted.

  Section Sequencing.

    Local Notation "x || y" := (par_action x y).
    Local Notation "x ; y" := (seq_action x y) (at level 51, right associativity).


    Lemma pick_unmasked :
      forall atom pat act tbl pt pk,
        None = apply_atom atom (pt, pk) ->
        scan (Pick pat act atom tbl) pt pk = drop.
    Proof with auto with datatypes.
      intros.
      induction tbl...
      destruct a as [p a].
      unfold scan.
      simpl.
      apply restrict_range_spec2  with (pat:=p) in H.
      rewrite -> Pattern.no_match_subset_r...
      rewrite -> Pattern.no_match_subset_r...
    Qed.

    Lemma unions_Pick_ok : 
      forall act pt pk pat tbl,
        Pattern.match_packet pt pk pat = true ->
        act; (par_actions
                (map
                   (fun ptpk : portId * packet =>
                   let (pt0, pk0) := ptpk in scan tbl pt0 pk0)
                   (apply_action act (pt, pk))))=
         scan
           (unions
              (map (fun atom => Pick pat act atom tbl)
                   (atoms act))) pt pk.
    Proof with auto with datatypes.
      intros.
      unfold apply_action.
      induction (atoms act).
      simpl. rewrite -> seq_drop_r...
      simpl.
      rewrite -> union_no_opt_spec.
      rewrite <- IHl; clear IHl.
      unfold filter_map_body.
      remember (apply_atom a (pt,pk)) as r.
      destruct r.
      + destruct p as [pt0 pk0].
        simpl.
        rewrite <- seq_distr.
        f_equal.
        generalize dependent act.
        { induction tbl; intros. 
          + unfold scan. simpl. rewrite -> seq_drop_r...
          + destruct a0 as [pat0 act0].
            unfold scan.
            simpl.
            rewrite <- IHtbl.
            remember (Pattern.match_packet pt pk (restrict_range a pat0))
              as b eqn:Hb.
            destruct b.
            * rewrite -> Pattern.is_match_true_inter...
              erewrite -> restrict_range_spec in Hb; eauto.
              rewrite <- Hb...
              rewrite -> Pattern.is_match_true_inter...
              apply restrict_domain_spec1 in Heqr...
            * rewrite -> Pattern.no_match_subset_r...
              erewrite -> restrict_range_spec in Hb; eauto.
              rewrite <- Hb...
              rewrite -> Pattern.no_match_subset_r... }
      + rewrite -> pick_unmasked...
        rewrite -> par_drop_l...
    Qed.

    Axiom union_assoc : 
      forall tbl1 tbl2 tbl3, 
        union_no_opt tbl1 (union_no_opt tbl2 tbl3) = union_no_opt (union_no_opt tbl1 tbl2) tbl3.

    Axiom union_comm : forall tbl1 tbl2, union_no_opt tbl1 tbl2 = union_no_opt tbl2 tbl1.

    Lemma union_nil_r : forall tbl, union_no_opt tbl nil = tbl.
    Proof with auto with datatypes.
      intros.
      unfold union_no_opt.
      rewrite -> inter_nil_r.
      simpl...
    Qed.

    Lemma sequence_no_opt_spec :
      forall pt pk tbl1 tbl2,
        seq tbl1 tbl2 pt pk = scan (sequence_no_opt tbl1 tbl2) pt pk.
    Proof with auto with datatypes.
      intros.
      induction tbl1...
      + simpl. rewrite -> seq_nil_l...
      + destruct a as [p a].
        unfold seq.
        unfold scan.
        simpl.
        remember (Pattern.match_packet pt pk p) as b eqn:Hb.
        { destruct b.
          + remember (atoms a) as ats.
            destruct ats.
            * simpl. 
              assert (atoms a = nil -> a = drop) as X. admit.
              assert (a = drop)... subst.
              clear X. 
              rewrite <- Hb.
              rewrite -> seq_drop_l...
            * fold scan.
              assert (unions (map (fun atom => Pick p a atom tbl2) (e0 :: ats)) =
                      union_no_opt (Pick p a e0 tbl2)
                            (unions (map (fun atom => Pick p a atom tbl2) ats))) as J...
              rewrite <- J; clear J.
              assert
                (scan
                   (unions
                      (map (fun atom : Classifier.Action.e => Pick p a atom tbl2)
                           (e0 :: ats)) ++ sequence_no_opt tbl1 tbl2) pt pk =
                 scan
                   (unions
                      (map (fun atom : Classifier.Action.e => Pick p a atom tbl2)
                           (e0 :: ats))) pt pk) as X. admit.
              (* Use pick_in with something about unions to remove the admit above. *)
              rewrite -> X; clear X.
              rewrite -> Heqats.
              apply unions_Pick_ok... 
          + remember (atoms a) as ats.
            destruct ats.
            * simpl. rewrite <- Hb...
            * fold scan.
              fold (seq tbl1 tbl2 pt pk).
              unfold scan.
              rewrite -> elim_scan_head...
              intros.
              clear Heqats.
              generalize dependent m.
              generalize dependent a0.
              induction ats; intros.
              - simpl in H.
                rewrite -> union_nil_r in H. 
                clear IHtbl1.
                induction tbl2...
                simpl in H. inversion H.
                simpl in H. destruct a1 as [p1 a1]. simpl in H.
                { destruct H.
                  + inversion H; subst; clear H.
                    rewrite -> Pattern.is_match_false_inter_l...
                  + apply IHtbl2... }
              - simpl in H.
                rewrite -> union_assoc in H.
                rewrite -> (union_comm (Pick p a e0 tbl2)) in H.
                rewrite <- union_assoc in H.
                refine (@match_union_false 
                          pt pk (Pick p a a0 tbl2)
                          (union_no_opt (Pick p a e0 tbl2)
                                 (unions (map (fun atom => Pick p a atom tbl2) ats))) _ _ m a1 _);
                  eauto.
                intros. symmetry. eapply Pick_false; eauto. }
    Qed.

  End Sequencing.

End Make.

