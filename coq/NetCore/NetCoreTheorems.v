Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Types.
Require Import Common.CpdtTactics.
Require Import Word.WordInterface.
Require Import Classifier.Classifier.
Require Import Classifier.Theory.
Require Import Network.NetworkPacket.
Require Import Pattern.Pattern.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import NetCore.NetCoreEval.
Require Import NetCore.NetCoreCompiler.
Require Import NetCore.Verifiable.

Local Open Scope list_scope.

Instance bool_as_Action : ClassifierAction bool := 
  {
    zero := false;
    action_eqdec := bool_dec
  }.

Hint Resolve zero.

Definition Equiv_Preserving (f : forall A, Classifier A -> Classifier A) :=
  forall (A : Type) (EA : ClassifierAction A) (pt : portId) (pk : packet) (cf : Classifier A),  
    scan zero (f A cf) pt pk = scan zero cf pt pk.

Hint Unfold Equiv_Preserving.

Theorem compile_pred_correct : 
  forall pr sw pt pk opt,
    Equiv_Preserving opt ->
    match_pred pr sw pt pk = scan false (compile_pred (opt bool) pr sw) pt pk.
Proof with auto.
  intros.
  assert (forall cf pt pk, scan false (opt bool cf) pt pk = scan false cf pt pk) as Heqp.
  unfold Equiv_Preserving in H.
  intros.
  assert (false = zero)...
  rewrite -> H0.
  rewrite -> H...
  clear H.
  induction pr.
  simpl.
  remember (Pattern.match_packet pt pk p).
  destruct b. trivial. trivial.
  (* On Switch *)
  simpl.
  remember (Word64.eq_dec sw s) as b.
  destruct b.
  simpl.
  rewrite -> Pattern.all_spec...
  simpl...
  (* PrOr *)
  assert (false = zero) as J...
  rewrite -> J in *.
  simpl.
  rewrite -> Heqp.
  rewrite -> union_scan_comm.
  rewrite -> IHpr1.
  rewrite -> IHpr2.
  trivial.
  (* showing that false is unit of orb. *)
  unfold has_unit.
  rewrite <- J.
  split; intros. 
  destruct a...
  destruct a...
  (* PrAnd *)
  { simpl.
    rewrite -> IHpr1.
    rewrite -> IHpr2.
    rewrite -> Heqp...
    rewrite -> inter_comm_bool_range... }
    (* PrNot *)
    simpl.
  rewrite -> Heqp.
  rewrite -> scan_map_comm with (defA := false)...
  remember (scan_inv false pk pt (compile_pred (opt bool) pr sw)) as Inv. 
  clear HeqInv.
  destruct Inv as [[H H0]| H].
  rewrite -> H0 in IHpr.
  rewrite -> IHpr.
  rewrite -> elim_scan_head...
  simpl.
  rewrite -> Pattern.all_spec...
  destruct H as [cf2 [cf3 [pat [a [H [H0 [H1 H2]]]]]]].
  rewrite -> H.
  rewrite <- app_assoc.
  rewrite <- app_comm_cons.
  rewrite -> elim_scan_tail...
  unfold pattern in *.
  rewrite <- H.
  f_equal...
  apply total_tail...
  (* PrAll *)
  simpl.
  rewrite -> Pattern.all_spec...
  (* PrNone *)
  simpl...
Qed.

Lemma A_eqdec : forall (a1 a2 : act), { a1 = a2 } + { a1 <> a2 }.
Proof. repeat decide equality.  Defined.

Instance A_as_Action : ClassifierAction act :=
  {
    zero := empty_action;
    action_eqdec := A_eqdec
  }.

Lemma eval_par_action : 
  forall sw pt pk bufId act1 act2, 
    eval_action (InPkt sw pt pk bufId) (par_action act1 act2) =
    eval_action (InPkt sw pt pk bufId) act1 ++
                eval_action (InPkt sw pt pk bufId) act2.
Proof with auto with datatypes.
  intros.
  destruct act1, act2.
  unfold eval_action.
  simpl.
  simpl.
  (* Requires semantics to produce a bag of outputs. *)
Admitted.

Lemma par_action_zero_l : forall a, par_action empty_action a = a.
Proof with auto.
  intros.
  destruct a.
  unfold par_action.
  unfold empty_action.
  simpl...
(* modifications are stupid *)
Admitted.

Lemma par_action_zero_r : forall a, par_action a empty_action = a.
Proof with auto.
  intros.
  destruct a.
  unfold par_action.
  unfold empty_action.
  simpl...
  do 2 rewrite -> app_nil_r...
Qed.

Lemma compile_pol_correct :
  forall opt po sw pt pk bufid,
    Vf_pol po ->
    Equiv_Preserving opt ->
    classify po (InPkt sw pt pk bufid) = 
    eval_action (InPkt sw pt pk bufid)
                (scan empty_action (compile_pol opt po sw) pt pk).
Proof with auto.
  intros.
  rename H into HVfPol.
  rename H0 into Heqp.
  induction po.
  + simpl.
    assert (forall cf pt pk, scan empty_action (opt act cf) pt pk = scan empty_action cf pt pk) as J0...
    intros.
    unfold Equiv_Preserving in Heqp.
    assert (empty_action = zero)...
    rewrite -> H.
    rewrite -> Heqp...
    rewrite -> J0.
    rewrite -> scan_map_comm with (defA := false)...
    rewrite -> scan_elim_unit_tail.
    assert (match_pred p sw pt pk = scan false (compile_pred (opt bool) p sw) pt pk).
    apply compile_pred_correct...
    rewrite -> H.
    destruct (scan false (compile_pred (opt bool) p sw) pt pk)...
    apply total_tail.
  + simpl.
    assert (empty_action = zero) as J...
    rewrite -> J in *.
    rewrite -> Heqp.
    inversion HVfPol; subst.
    rewrite -> union_scan_comm.
    rewrite -> IHpo1...
    rewrite -> IHpo2...
    symmetry.
    apply eval_par_action.
    (* Shows that app respects unit (nil) *)
    split; intros; rewrite <- J.
    apply par_action_zero_r.
    apply par_action_zero_l.
  + inversion HVfPol.
Qed.

Local Open Scope equiv_scope.

Lemma Equiv_Preserving_elim_shadowed : Equiv_Preserving (@elim_shadowed).
Proof.
  unfold Equiv_Preserving.
  intros.
  remember (elim_shadowed_ok cf) as H.
  clear HeqH.
  unfold equiv in H.
  unfold Classifier_equiv in H.
  rewrite -> H.
  trivial.
Qed.


Lemma Equiv_Preserving_id : Equiv_Preserving no_opt.
Proof.
  unfold Equiv_Preserving.
  intros.
  unfold Datatypes.id.
  reflexivity.
Qed.

Lemma Equiv_Preserving_composes : 
  forall f g,
    Equiv_Preserving f -> 
    Equiv_Preserving g ->
    Equiv_Preserving (fun A x  => g A (f A x)).
Proof.
  intros.
  unfold Equiv_Preserving in *.
  intros.
  specialize H0 with A EA pt pk (f A cf). 
  rewrite H0.
  apply H.
Qed.

Lemma scan_pat_none :
  forall A (def : A) cf pt pk a pat,
    Pattern.is_empty pat = true ->
    scan def ((pat, a) :: cf) pt pk = scan def cf pt pk.
Proof with auto.
  intros.
  simpl.
  rewrite -> Pattern.match_packet_spec.
  rewrite -> Pattern.is_empty_true_r...
Qed.

Lemma Equiv_Preserving_strip_empty : Equiv_Preserving strip_empty_rules.
Proof with auto.
  unfold Equiv_Preserving.
  intros.
  induction cf; auto.
  destruct a.
  simpl. 
  remember (Pattern.is_empty p) as b.
  destruct b.
  rewrite -> Pattern.match_packet_spec.
  rewrite -> Pattern.is_empty_true_r...
  simpl.
  destruct (Pattern.match_packet pt pk p)...
Qed.

Lemma compile_no_opt_ok :
  forall po sw pt pk bufid,
    Vf_pol po ->
    classify po (InPkt sw pt pk bufid) = 
    eval_action (InPkt sw pt pk bufid)
        (scan empty_action (compile_no_opt po sw) pt pk).
Proof.
  intros.
  unfold compile_no_opt.
  apply compile_pol_correct.
  trivial.
  apply Equiv_Preserving_id.
Qed.

Lemma compile_opt_ok : 
  forall po sw pt pk bufid,
    Vf_pol po ->
    classify po (InPkt sw pt pk bufid) = 
    eval_action (InPkt sw pt pk bufid)
                (scan empty_action (compile_opt po sw) pt pk).
Proof.
  intros.
  unfold compile_no_opt.
  apply compile_pol_correct.
  trivial.
  apply Equiv_Preserving_composes.
  apply Equiv_Preserving_elim_shadowed.
  apply Equiv_Preserving_strip_empty.
Qed.

Definition SemanticsPreserving opt := Equiv_Preserving opt.

Definition netcore_eval pol sw pt pk bufId :=
  classify pol (InPkt sw pt pk bufId).

Definition flowtable_eval ft sw pt pk (bufId : option bufferId) :=
  eval_action (InPkt sw pt pk bufId) (scan empty_action ft pt pk).

Definition compile := compile_pol.

Definition compose {A B C : Type} (f : B -> C) (g : A -> B) x := f (g x).

Theorem compile_correct : 
  forall pol sw pt pk bufId,
    Vf_pol pol ->
    netcore_eval pol sw pt pk bufId =
    flowtable_eval (compile_opt pol sw) sw pt pk bufId.
Proof.
  unfold compile.
  unfold SemanticsPreserving.
  unfold netcore_eval.
  unfold flowtable_eval.
  intros.
  apply compile_pol_correct.
  trivial.
  apply Equiv_Preserving_composes.
  apply Equiv_Preserving_elim_shadowed.
  apply Equiv_Preserving_strip_empty.
Qed.
