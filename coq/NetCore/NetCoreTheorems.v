Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.

Require Import Common.Types.
Require Import Common.CpdtTactics.
Require Import Word.WordInterface.
Require Import Classifier.Defs.
Require Import Classifier.Theory.
Require Import Network.Packet.
Require Import Pattern.Defs.
Require Import Pattern.Theory.
Require Import OpenFlow.MessagesDef.
Require Import NetCore.NetCore.

Local Open Scope list_scope.

Instance bool_as_Action : ClassifierAction bool := {
  action_unit := false;
  action_eqdec := bool_dec
}.

  Hint Resolve action_unit.

  Definition Equiv_Preserving (f : forall A, Classifier A -> Classifier A) :=
    forall (A : Type) (EA : ClassifierAction A) (pt : portId) (pk : packet) (cf : Classifier A),  
    scan action_unit (f A cf) pt pk = scan action_unit cf pt pk.

  Hint Unfold Equiv_Preserving.

  Theorem compile_pred_correct : forall pr sw pt pk opt,
    Equiv_Preserving opt ->
    match_pred pr sw pt pk = scan false (compile_pred (opt bool) pr sw) pt pk.
  Proof with auto.
    intros.
    assert (forall cf pt pk, scan false (opt bool cf) pt pk = scan false cf pt pk) as Heqp.
      unfold Equiv_Preserving in H.
      intros.
      assert (false = action_unit)...
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
    simpl...
    simpl...
    (* PrOr *)
    simpl.
    assert (false = action_unit) as J...
    rewrite -> J in *.
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
    destruct H as [cf2 [cf3 [pat [a [H [H0 [H1 H2]]]]]]].
    rewrite -> H.
    rewrite <- app_assoc.
    rewrite <- app_comm_cons.
    rewrite -> elim_scan_tail...
    rewrite <- H.
    f_equal...
    apply total_tail...
    (* PrAll *)
    simpl...
    (* PrNone *)
    simpl...
  Qed.

  Lemma A_eqdec : forall (a1 a2 : list Action), { a1 = a2 } + { a1 <> a2 }.
  Proof. repeat decide equality.  Defined.

  Instance A_as_Action : ClassifierAction (list Action) := {
  action_unit := @nil Action;
  action_eqdec := A_eqdec
}.


  Lemma compile_pol_correct : forall opt po sw pt pk bufid,
    Equiv_Preserving opt ->
    classify po (InPkt sw pt pk bufid) = 
      map (eval_action (InPkt sw pt pk bufid)) 
        (scan nil (compile_pol opt po sw) pt pk).
  Proof with auto.
    intros.
    rename H into Heqp.
    induction po.

    simpl.
    assert (forall cf pt pk, scan nil (opt (list Action) cf) pt pk = scan nil cf pt pk) as J0...
      intros.
      unfold Equiv_Preserving in Heqp.
      assert (nil = action_unit)...
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

    simpl.
    assert (nil = action_unit) as J...
    rewrite -> J in *.
    rewrite -> Heqp.
    rewrite -> union_scan_comm.
    rewrite -> IHpo1.
    rewrite -> IHpo2.
    rewrite -> map_app.
    trivial.
    (* Shows that app respects unit (nil) *)
    split... intros. rewrite <- J.  apply app_nil_r.
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
    unfold id.
    reflexivity.
  Qed.

  Lemma Equiv_Preserving_composes : forall f g,
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
  
  Lemma scan_pat_none : forall A (def : A) cf pt pk a pat,
    Pattern.is_empty pat = true ->
    scan def ((pat, a) :: cf) pt pk = scan def cf pt pk.
  Proof with auto.
    intros.
    simpl.
    unfold Pattern.match_packet.
    rewrite -> PatMatchable.is_empty_true_r...
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
    unfold Pattern.match_packet.
    rewrite -> PatMatchable.is_empty_true_r...
    simpl.
    destruct (Pattern.match_packet pt pk p)...
  Qed.

  Lemma compile_no_opt_ok : forall po sw pt pk bufid,
    classify po (InPkt sw pt pk bufid) = 
      map (eval_action (InPkt sw pt pk bufid)) 
        (scan nil (compile_no_opt po sw) pt pk).
  Proof.
    intros.
    unfold compile_no_opt.
    apply compile_pol_correct.
    apply Equiv_Preserving_id.
  Qed.

  Lemma compile_opt_ok : forall po sw pt pk bufid,
    classify po (InPkt sw pt pk bufid) = 
      map (eval_action (InPkt sw pt pk bufid)) 
        (scan nil (compile_opt po sw) pt pk).
  Proof.
    intros.
    unfold compile_no_opt.
    apply compile_pol_correct.
    apply Equiv_Preserving_composes.
    apply Equiv_Preserving_elim_shadowed.
    apply Equiv_Preserving_strip_empty.
  Qed.