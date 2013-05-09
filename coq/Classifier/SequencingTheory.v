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

  Variable A : Type.

  Variable interp_action : A -> portId -> packet -> list (portId * packet).
  Variable par_action : A -> A -> A.
  Variable seq_action : A -> A -> A.
  Variable mask : A -> pattern.

  Variable Action : ClassifierAction A.
  Axiom zero_spec : forall pt pk, interp_action zero pt pk = nil.
  Axiom par_zero_l : forall a, par_action zero a = a.
  Axiom par_zero_r : forall a, par_action a zero = a.
  Axiom seq_zero_l : forall a, seq_action zero a = zero.
  Axiom seq_zero_r : forall a, seq_action a zero = zero.

  Axiom mask_relax :
    forall pt pk pat1 pat2, 
      Pattern.match_packet pt pk pat1 = Pattern.match_packet pt pk (Pattern.mask pat1 pat2).


  Lemma sequence_nil_r : forall tbl, sequence mask seq_action tbl nil = nil.
  Proof with auto with datatypes.
    intros.
    induction tbl.
    simpl...
    simpl.
    destruct a...
  Qed.

  Lemma silly : forall (A : Type) (x : A) xs, x :: xs = [x] ++ xs.
  Proof. auto. Qed.


  Lemma seq_elim_hd : 
    forall pt pk pat act tbl1 tbl2,
      false = Pattern.match_packet pt pk pat ->
      scan zero (sequence mask seq_action tbl1 ((pat, act) :: tbl2)) pt pk =
      scan zero (sequence mask seq_action tbl1 tbl2) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl1...
    induction tbl2.
    + rewrite -> sequence_nil_r.
      simpl.
      destruct a.
      rewrite -> silly.
      rewrite -> elim_scan_head.
      rewrite -> IHtbl1.
      rewrite -> sequence_nil_r...
      intros.
      simpl in H0.
      destruct H0. 2: inversion H0.
      inversion H0; subst.
      rewrite -> Pattern.no_match_subset_r...
      erewrite <- mask_relax...
      rewrite -> Pattern.is_match_false_inter_l...
    + admit. (* irrtating but prolly right. *)
  Qed.

  Lemma seq_elim_hd_2 : 
    forall pt pk pat act tbl1 tbl2,
      false = Pattern.match_packet pt pk (mask act) ->
      scan zero (sequence mask seq_action tbl1 ((pat, act) :: tbl2)) pt pk =
      scan zero (sequence mask seq_action tbl1 tbl2) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl1...
    induction tbl2.
    + rewrite -> sequence_nil_r.
      simpl.
      destruct a.
      rewrite -> silly.
      rewrite -> elim_scan_head.
      rewrite -> IHtbl1.
      rewrite -> sequence_nil_r...
      intros.
      simpl in H0.
      destruct H0. 2: inversion H0.
      inversion H0; subst.
      rewrite -> Pattern.no_match_subset_r...
      erewrite <- mask_relax...
      rewrite -> Pattern.no_match_subset_r...
    + admit. (* irrtating but prolly right. *)
  Qed.

  Lemma seq_ok : 
    forall (tbl1 tbl2 : Classifier A) (pt : portId) (pk : packet),
      total tbl2 ->
      seq_action (scan zero tbl1 pt pk) (scan zero tbl2 pt pk) =
      scan zero (sequence mask seq_action tbl1 tbl2) pt pk.
  Proof with auto with datatypes.
    intros.
    induction tbl1.
    + simpl.
      rewrite -> seq_zero_l...
    + simpl.
      destruct a as [p1 a1].
      remember (Pattern.match_packet pt pk p1) as b.
      destruct b.
    - unfold sequence_helper.
      clear IHtbl1.
      induction tbl2...
      * simpl. rewrite -> seq_zero_r. rewrite -> sequence_nil_r...
      * simpl.
        destruct a as [pat a0].
        remember (Pattern.match_packet pt pk pat) as b eqn: Hb.
        { destruct b.
          + simpl.
            (* Here, it is important we don't fall off the end of segment. *)
            remember (Pattern.match_packet pt pk (mask a0)) as b eqn: Hb0.
            destruct b.
            - rewrite -> Pattern.is_match_true_inter...
              rewrite <- mask_relax.
              rewrite -> Pattern.is_match_true_inter...
            - match goal with
                | [ |- context[if ?c then ?t else ?f ] ] => destruct c
              end.
              reflexivity.
              (* TODO(arjun): total needs to imply that the action is zero too. *)
              admit.
          + rewrite -> scan_app_compose in *.
            rewrite -> seq_elim_hd...
            rewrite -> silly.
            rewrite -> scan_app_compose.
            simpl.
            rewrite -> Pattern.no_match_subset_r...
            rewrite -> IHtbl2...
            admit. (* tail is also total *)
            erewrite <- mask_relax.
            rewrite -> Pattern.is_match_false_inter_l... }
    - rewrite -> IHtbl1...
      unfold sequence_helper.
      rewrite -> elim_scan_head...
      intros.
      clear IHtbl1.
      induction tbl2...
      simpl in H0.
      inversion H0.
      destruct a0.
      simpl in H0.
      destruct H0 as [H0 | H0]...
      unfold sequence_atom in H0.
      inversion H0; subst.
      rewrite -> Pattern.is_match_false_inter_l...
      rewrite -> IHtbl2...
      admit. (* tail is also total *)
  Qed.