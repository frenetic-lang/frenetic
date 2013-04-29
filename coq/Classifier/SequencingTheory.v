Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.CpdtTactics.
Require Import Common.Types.
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import Classifier.Classifier.
Require Import Classifier.Theory.

Local Open Scope list_scope.

Section Sequencing.

  Variable A : Type.

  Variable interp_action : A -> portId -> packet -> list (portId * packet).
  Variable mask : A -> pattern.

  Variable ActionInstance : Action A.
  Existing Instance ActionInstance.

  Inductive Total : Classifier A -> Prop :=
  | total_tail : forall cf,  Total (cf ++ [(Pattern.all, zero)]).

  Axiom zero_spec : forall pt pk, interp_action zero pt pk = nil.

  Axiom mask_relax :
    forall pt pk pat1 pat2, 
      Pattern.match_packet pt pk pat1 = 
      Pattern.match_packet pt pk (Pattern.mask pat1 pat2).

  Lemma sequence_nil_r : forall tbl, sequence mask seq_action tbl nil = nil.
  Proof with auto with datatypes.
    intros.
    induction tbl.
    simpl...
    simpl.
    destruct a...
  Qed.

  Lemma sequence_zero_r : 
    forall pt pk tbl, 
      scan zero (sequence mask seq_action tbl [(Pattern.all, zero)]) pt pk = zero.
  Proof with auto with datatypes.
    intros.
    induction tbl...
    destruct a as [p a].
    simpl.
    remember (Pattern.match_packet pt pk p) as b eqn:Hb.
    destruct b.
    + remember (Pattern.match_packet pt pk (mask zero)) as b eqn:Hb0.
      destruct b.
      - rewrite -> Pattern.is_match_true_inter...
        rewrite -> seq_zero_r...
        rewrite <- mask_relax.
        rewrite -> Pattern.is_match_true_inter...
        rewrite -> Pattern.all_spec...
      - rewrite -> Pattern.no_match_subset_r...
        rewrite <- mask_relax...
        rewrite -> Pattern.no_match_subset_r...
    + rewrite -> Pattern.is_match_false_inter_l...
  Qed.

  Lemma sequence_cons_zero_r : 
    forall pt pk tbl pat act,
      false = Pattern.match_packet pt pk pat ->
      scan zero
           (sequence mask seq_action tbl [(pat, act); (Pattern.all, zero)]) pt pk =
      zero.
  Proof with auto with datatypes.
    intros.
    induction tbl...
    destruct a as [p a].
    simpl.
    rewrite -> Pattern.no_match_subset_r...
    { remember (Pattern.match_packet pt pk p) as b eqn:Hb.
      destruct b.
      + remember (Pattern.match_packet pt pk (mask zero)) as b eqn:Hb0.
        destruct b.
      - rewrite -> Pattern.is_match_true_inter...
        rewrite -> seq_zero_r...
        rewrite <- mask_relax.
        rewrite -> Pattern.is_match_true_inter...
        rewrite -> Pattern.all_spec...
      - rewrite -> Pattern.no_match_subset_r...
        rewrite <- mask_relax...
        rewrite -> Pattern.no_match_subset_r... 
      + rewrite -> Pattern.is_match_false_inter_l... }
    rewrite <- mask_relax.
    rewrite -> Pattern.is_match_false_inter_l...
  Qed.

  Lemma silly : forall (A : Type) (x : A) xs, x :: xs = [x] ++ xs.
  Proof. auto. Qed.

  Lemma scan_sequence_helper_false : 
    forall pat act z tbl pt pk,
      false = Pattern.match_packet pt pk pat ->
      scan z (sequence_helper mask seq_action pat act tbl) pt pk = z.
  Proof with auto with datatypes.
    intros.
    induction tbl...
    destruct a as [p a].
    simpl.
    rewrite -> Pattern.is_match_false_inter_l...
  Qed.

  Lemma total_singleton : 
    forall pat act, 
      Total ([(pat,act)]) -> pat = Pattern.all /\ act = zero.
  Proof with auto with datatypes.
    intros.
    inversion H.
    destruct cf.
    + simpl in H1. inversion H1...
      + simpl in H1. inversion H1.
        destruct cf; simpl in H3; inversion H3. 
  Qed.

  Lemma total_pop : forall x y lst, Total (x :: y :: lst) -> Total (y :: lst).
  Proof with auto.
    intros.
    inversion H.
    destruct lst.
  Admitted.

  Lemma scan_sequence_helper_elim_tail :
    forall pat act z tbl1 tbl2 pt pk,
      Total tbl1 ->
      true = Pattern.match_packet pt pk pat ->
      scan z
           (sequence_helper mask seq_action pat act tbl1 ++ tbl2) pt pk =
      scan z
           (sequence_helper mask seq_action pat act tbl1) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl1...
    + inversion H.
      destruct cf. simpl. inversion H2. simpl in H2. inversion H2.
    + destruct a as [p a].
      destruct tbl1.
      - inversion H.
        destruct cf.
        simpl in H2.
        inversion H2; subst; clear H2.
        simpl.
        rewrite -> Pattern.is_match_true_inter...
        rewrite <- mask_relax.
        rewrite -> Pattern.is_match_true_inter...
        apply Pattern.all_spec.
        admit. (* mask zero = all *)
        simpl in H2. destruct cf; simpl in H2; inversion H2.
      - simpl.
        simpl in IHtbl1.
        assert (Total (p0 :: tbl1)) as J.
        { eapply total_pop; eauto. }
        apply IHtbl1 in J; clear IHtbl1.
        remember (Pattern.match_packet pt pk p) as b eqn:Hb.
        destruct b.
        * remember (Pattern.match_packet pt pk (mask a)) as b eqn:Hb0.
          { destruct b.
            + rewrite -> Pattern.is_match_true_inter...
              rewrite <- mask_relax.
              rewrite -> Pattern.is_match_true_inter...
            + rewrite -> Pattern.no_match_subset_r...
              rewrite <- mask_relax.
              rewrite -> Pattern.no_match_subset_r... }
        * rewrite -> Pattern.no_match_subset_r...
          rewrite <- mask_relax.
          rewrite -> Pattern.is_match_false_inter_l...
  Qed.

  Lemma seq_elim_hd : 
    forall pt pk pat act tbl1 tbl2,
      Total tbl2 ->
      false = Pattern.match_packet pt pk pat ->
      scan zero (sequence mask seq_action tbl1 ((pat, act) :: tbl2)) pt pk =
      scan zero (sequence mask seq_action tbl1 tbl2) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl2...
    + induction tbl1...
      rewrite -> sequence_nil_r.
      destruct a. unfold sequence_atom. simpl.
      rewrite -> Pattern.no_match_subset_r...
      rewrite -> IHtbl1. rewrite -> sequence_nil_r...
      rewrite <- mask_relax.
      rewrite -> Pattern.is_match_false_inter_l...
    + destruct a as [p a].
      induction tbl1...
      destruct a0 as [p0 a0].
      simpl.
      rewrite -> Pattern.no_match_subset_r...
      simpl in IHtbl2.
      rewrite -> Pattern.no_match_subset_r in IHtbl2...
      do 2 rewrite -> scan_app_compose in IHtbl2...
      remember (Pattern.match_packet pt pk p0) as b eqn:Hb.
      { destruct b.
        - (* clear IHtbl1. *)
          remember (Pattern.match_packet pt pk p) as b eqn:Hb0.
          destruct b.
          + remember (Pattern.match_packet pt pk (mask a)) as b eqn:Hb1.
            destruct b.
            * rewrite -> Pattern.is_match_true_inter...
              rewrite <- mask_relax.
              rewrite -> Pattern.is_match_true_inter...
            * rewrite -> Pattern.no_match_subset_r...
              { destruct tbl2.
                + destruct (total_singleton H); subst; clear H.
                  simpl.
                  rewrite -> sequence_zero_r.
                  rewrite -> sequence_cons_zero_r...
                + rewrite -> scan_sequence_helper_elim_tail 
                  with (pt:=pt) (pk:=pk)...
                  rewrite -> scan_sequence_helper_elim_tail 
                  with (pt:=pt) (pk:=pk)...
                  eapply total_pop; eauto.  
                  eapply total_pop; eauto. }
              rewrite <- mask_relax...
              rewrite -> Pattern.no_match_subset_r...
          + rewrite -> Pattern.no_match_subset_r...
              { destruct tbl2.
                + destruct (total_singleton H); subst; clear H.
                  simpl.
                  rewrite -> sequence_zero_r.
                  rewrite -> sequence_cons_zero_r...
                + rewrite -> scan_sequence_helper_elim_tail 
                  with (pt:=pt) (pk:=pk)...
                  rewrite -> scan_sequence_helper_elim_tail 
                  with (pt:=pt) (pk:=pk)...
                  eapply total_pop; eauto.  
                  eapply total_pop; eauto. }
              rewrite <- mask_relax.
              rewrite -> Pattern.is_match_false_inter_l...
        - rewrite -> Pattern.is_match_false_inter_l...
          simpl in IHtbl2.
          do 2 rewrite -> scan_sequence_helper_false in IHtbl2...
          do 2 rewrite -> scan_app_compose...
          rewrite -> scan_sequence_helper_false...
          rewrite -> scan_sequence_helper_false... }
      rewrite <- mask_relax...
      rewrite -> Pattern.is_match_false_inter_l...
      rewrite <- mask_relax...
      rewrite -> Pattern.is_match_false_inter_l...
  Qed.

  Lemma seq_elim_hd_2 : 
    forall pt pk pat act tbl1 tbl2,
      Total tbl2 ->
      false = Pattern.match_packet pt pk (mask act) ->
      scan zero (sequence mask seq_action tbl1 ((pat, act) :: tbl2)) pt pk =
      scan zero (sequence mask seq_action tbl1 tbl2) pt pk.
  Admitted.

  Hint Resolve total_pop.

  Definition scan_list (tbl : Classifier A) (lst : list (portId * packet)) :=
    fold_right
      (fun ptpk act =>
         match ptpk with
           | (pt, pk) => par_action (scan zero tbl pt pk) act
         end)
      zero
      lst.

  Lemma scan_list_nil_tbl : forall lst, scan_list nil lst = zero.
  Proof with auto with datatypes.
    intros.
    induction lst...
    simpl.
    destruct a.
    rewrite -> IHlst.
    rewrite -> par_zero_l...
  Qed.

  Lemma scan_list_elim_hdrule :
    forall pat act tbl pks pt pk,
      Pattern.match_packet pt pk pat = false ->
      scan_list ((pat, act) :: tbl) pks = scan_list tbl pks.
  Proof with auto with datatypes.
    intros.
    induction tbl...
    + rewrite -> scan_list_nil_tbl.
      induction pks...
      destruct a...
      simpl.
      simpl.
    simpl.
  Admitted.

  Axiom interp_zero : forall pt pk, interp_action zero pt pk = nil.

(*
  Lemma scan_list_ok : 
    forall p a tbl a0 pt pk,
      true = Pattern.match_packet pt pk p ->
      true = Pattern.match_packet pt pk (mask a) ->
      scan_list ((p, a) :: tbl) (interp_action a0 pt pk) = seq_action a0 a.
  Proof with auto with datatypes.
    intros.
    induction (interp_action a0 pt pk).
    + admit.
    + simpl. destruct a1 as [pt1 pk1].
      rewrite -> IHl.
    remember (interp_action a0 pt pk) as lst.
    induction lst...
    + simpl.
      (* err... need interp_action to be a maybe? *)
      admit.
    + 
*)

  Lemma sequence_helper_correctness :
    forall pat act tbl pt pk,
      true = Pattern.match_packet pt pk pat ->
      Total tbl ->
      scan_list tbl (interp_action act pt pk) =
      scan zero (sequence_helper mask seq_action pat act tbl) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl.
    + simpl. rewrite -> scan_list_nil_tbl...
    + destruct a as [p a].
      simpl.
      remember (Pattern.match_packet pt pk p) as b eqn:Hb.
      destruct b.
      - remember (Pattern.match_packet pt pk (mask a)) as b eqn:Hb0.
        destruct b.
        * simpl.
          rewrite -> Pattern.is_match_true_inter...
          
  Admitted.

  Lemma sequencing_correctness :
    forall (tbl1 tbl2 : Classifier A) (pt : portId) (pk : packet),
      Total tbl2 ->
      scan_list tbl2 (interp_action (scan zero tbl1 pt pk) pt pk) =
      scan zero (sequence mask seq_action tbl1 tbl2) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl1.
    + simpl.
      rewrite -> interp_zero...
    + destruct a as [p1 a1].
      simpl.
      remember (Pattern.match_packet pt pk p1) as b.
      destruct b.
    - 
      
      fold sequence.
      remember (Pattern.match_packet pt pk p1) as b.
      destruct b.
    - induction tbl2...
      * simpl. 
        rewrite -> scan_list_nil_tbl.
        rewrite -> sequence_nil_r...
      * simpl.
        destruct a as [pat a].
        simpl.
        remember (Pattern.match_packet pt pk pat) as b eqn: Hb.
        { destruct b.
          + simpl.
            (* Here, it is important we don't fall off the end of segment. *)
            remember (Pattern.match_packet pt pk (mask a)) as b eqn: Hb0.
            destruct b.
            - rewrite -> Pattern.is_match_true_inter...
              admit. (* important lemma! *)
              rewrite <- mask_relax.
              rewrite -> Pattern.is_match_true_inter...
            - admit. 
          + destruct tbl2.
            - destruct (total_singleton H).
              subst.
              simpl.
              rewrite -> scan_app_compose in *.
              rewrite -> seq_zero_r.
              rewrite -> sequence_zero_r.
              match goal with
                | [ |- context[if ?c then _ else _] ] => destruct c
              end.
              * admit. 
              * admit.
            - rewrite -> Pattern.no_match_subset_r...
              
              admit. (* hard *)
(*
              rewrite -> elim_scan_head.
              rewrite -> seq_elim_hd; eauto.

rewrite -> scan_app_compose.
              rewrite -> seq_elim_hd; eauto.
              rewrite <- scan_app_compose.
              rewrite <- app_comm_cons.
              rewrite -> (silly (sequence_atom mask seq_action p1 a1 pat a0)).
              rewrite -> elim_scan_head.
              apply IHtbl2.
              eauto.
              intros.
              simpl in H0.
              unfold sequence_atom in H0.
              { destruct H0.
                + inversion H0; subst; clear H0.
                  rewrite -> Pattern.no_match_subset_r...
                  rewrite <- mask_relax.
                  rewrite -> Pattern.is_match_false_inter_l...
                + inversion H0. } }
*)
              idtac.
              admit. }
    - rewrite -> IHtbl1...
      unfold sequence_helper.
      rewrite -> elim_scan_head...
      intros.
      clear IHtbl1.
      induction tbl2...
      * simpl in H0.
        inversion H0.
      * destruct a0.
        simpl in H0.
        destruct H0 as [H0 | H0]...
        unfold sequence_atom in H0.
        inversion H0; subst.
        rewrite -> Pattern.is_match_false_inter_l...
        { destruct tbl2.
          + simpl in *. inversion H0.
          + rewrite -> IHtbl2; eauto. }
  Qed.