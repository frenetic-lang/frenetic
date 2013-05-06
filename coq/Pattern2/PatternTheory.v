Set Implicit Arguments.

Require Import Coq.Arith.EqNat.
Require Import NPeano.
Require Import Arith.Peano_dec.
Require Import Bool.Bool.
Require Import Coq.Classes.Equivalence.
Require Import Lists.List.

Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import Common.Types.
Require Import Pattern2.PatternSignatures.
Require Pattern2.PatternImpl.
Require Import Wildcard.Wildcard.
Require Import Wildcard.Theory.
Require Import OpenFlow.OpenFlowSemantics.

Open Scope bool_scope.
Open Scope list_scope.
Open Scope equiv_scope.

Module Make (Import Port : PORT) <: PATTERN_SPEC.

  Module Pattern := Pattern2.PatternImpl.Make (Port).
  Import Pattern.

  Create HintDb pattern.

  Lemma IP_ARP_frametyp_neq : Const_0x800 <> Const_0x806.
  Proof with auto.
    assert ({ Const_0x800 = Const_0x806 } + { Const_0x800 <> Const_0x806 }).
    apply Word16.eq_dec.
    unfold Const_0x800 in *. 
    unfold Const_0x806 in *.
    destruct H.
    inversion e.
    trivial.
  Qed.

  Hint Unfold inter empty is_empty.
  Hint Resolve Word8.eq_dec Word16.eq_dec Word32.eq_dec Word48.eq_dec.

  Lemma inter_comm : forall p p', inter p p' = inter p' p.
  Proof with auto.
    intros.
    destruct p.
    destruct p'. 
    unfold inter.
    simpl.
    rewrite -> (inter_comm _ ptrnDlSrc0).
    rewrite -> (inter_comm _ ptrnDlDst0).
    rewrite -> (inter_comm _ ptrnDlType0).
    rewrite -> (inter_comm _ ptrnDlVlan0).
    rewrite -> (inter_comm _ ptrnDlVlanPcp0).
    rewrite -> (inter_comm _ ptrnNwSrc0).
    rewrite -> (inter_comm _ ptrnNwDst0).
    rewrite -> (inter_comm _ ptrnNwTos0).
    rewrite -> (inter_comm _ ptrnTpSrc0).
    rewrite -> (inter_comm _ ptrnTpDst0).
    rewrite -> (inter_comm _ ptrnInPort0).
    rewrite -> (inter_comm _ ptrnNwProto0).
    reflexivity.
  Qed.

  Lemma inter_assoc : forall p p' p'',
                        inter p (inter p' p'') = inter (inter p p') p''.
  Proof with simpl; auto.
    intros.
    unfold inter.
    simpl.
    repeat rewrite -> inter_assoc...
  Qed.

  Lemma is_empty_false_distr_l : forall x y,
                                   is_empty (inter x y) = false -> 
                                   is_empty x = false .
  Proof with simpl; eauto.
    intros.
    unfold inter in H.
    simpl in H.
    repeat rewrite -> orb_false_iff in H.
    do 11 (destruct H).
    unfold is_empty.
    destruct x.
    destruct y.
    simpl in *.
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    erewrite -> is_empty_false_distr_l; [ idtac | eauto ].
    reflexivity.
  Qed.

  Lemma is_empty_false_distr_r : forall x y,
                                   is_empty (inter x y) = false -> 
                                   is_empty y = false .
  Proof. 
    intros.
    rewrite -> inter_comm in H.
    eapply is_empty_false_distr_l.
    exact H.
  Qed.

  Lemma is_empty_true_l : forall x y,
                            is_empty x = true ->
                            is_empty (inter x y) = true.
  Proof with auto.
    intros.
    destruct x.
    destruct y.
    simpl in *.
    repeat rewrite -> orb_true_iff in H.
    repeat rewrite -> or_assoc in H.
    repeat rewrite -> orb_true_iff.
    repeat rewrite -> or_assoc.
    repeat (destruct H; auto 13 with wildcard).
  Qed.

  Lemma is_empty_true_r : forall x y,
                            is_empty y = true ->
                            is_empty (inter x y) = true.
  Proof with auto.
    intros.
    rewrite inter_comm.
    apply is_empty_true_l...
  Qed.

  Lemma is_match_false_inter_l :
    forall pt (pkt : packet) pat1 pat2,
      match_packet pt pkt pat1 = false ->
      match_packet pt pkt (inter pat1 pat2) = false.
  Proof with auto.
    intros.
    unfold match_packet in *.
    rewrite -> negb_false_iff in H.
    rewrite -> negb_false_iff.
    rewrite -> inter_assoc.
    apply is_empty_true_l...
  Qed.

  Lemma is_match_false_inter_r :
    forall pt (pkt : packet) pat1 pat2,
      match_packet pt pkt pat2 = false ->
      match_packet pt pkt (inter pat1 pat2) = false.
  Proof with auto.
    intros.
    unfold match_packet in *.
    rewrite -> negb_false_iff in H.
    rewrite -> negb_false_iff.
    rewrite inter_comm with (p := pat1). 
    rewrite -> inter_assoc.
    apply is_empty_true_l...
  Qed.

  Lemma no_match_subset_r :
    forall k n t t',
      match_packet n k t' = false -> 
      match_packet n k (inter t t') = false.
  Proof with auto.
    intros.
    rewrite -> inter_comm.
    apply is_match_false_inter_l...
  Qed.

  Lemma exact_match_inter : 
    forall x y,
      is_exact x = true ->
      is_empty (inter x y) = false ->
      inter x y = x.
  Proof with auto.
    intros.
    destruct x. destruct y. simpl in *.
    repeat rewrite -> andb_true_iff in H.
    do 11 (destruct H).
    repeat rewrite -> orb_false_iff in H0.
    do 11 (destruct H0).
    unfold inter.
    unfold inter.
    simpl.
    repeat rewrite -> exact_match_inter_l...
  Qed.

  Lemma all_spec :
    forall pt pk,
      match_packet pt pk all = true.
  Proof with auto.
    intros.
    unfold match_packet.
    rewrite -> negb_true_iff.
    unfold all.
    simpl.
    reflexivity.
  Qed.

  Lemma exact_match_is_exact : 
    forall pk pt,
      is_exact (exact_pattern pk pt) = true.
  Proof with auto.
    intros.
    unfold exact_pattern.
    unfold is_exact.
    unfold Wildcard.is_exact.
    simpl...
  Qed.

  Lemma exact_intersect :
    forall k n t,
      match_packet k n t = true ->
      inter (exact_pattern n k) t = exact_pattern n k.
  Proof with auto.
    intros.
    unfold match_packet in H.
    rewrite -> negb_true_iff in H.
    apply exact_match_inter...
  Qed.

  Lemma is_match_true_inter :
    forall pat1 pat2 pt pk,
      match_packet pt pk pat1 = true ->
      match_packet pt pk pat2 = true ->
      match_packet pt pk (inter pat1 pat2) = true.
  Proof with auto.
    intros.
    apply exact_intersect in H.
    apply exact_intersect in H0.
    unfold match_packet.
    rewrite -> negb_true_iff.
    rewrite -> inter_assoc.
    rewrite -> H.
    rewrite -> H0.
    unfold exact_pattern.
    unfold is_empty.
    simpl.
    reflexivity.
  Qed.

  Hint Rewrite inter_assoc : pattern.
  Hint Resolve is_empty_false_distr_l is_empty_false_distr_r : pattern.
  Hint Resolve is_empty_true_l is_empty_true_r : pattern.
  Hint Resolve is_match_false_inter_l : pattern.
  Hint Resolve all_spec : pattern.
  Hint Resolve exact_match_is_exact : pattern.
  Hint Resolve exact_intersect : pattern.
  Hint Resolve is_match_true_inter : pattern.

  Hint Resolve is_empty_true_l is_empty_true_r.

  Definition equiv (pat1 pat2 : pattern) : Prop :=
    forall pt pk, 
      match_packet pt pk pat1 = match_packet pt pk pat2.
  
  Lemma equiv_is_Equivalence : Equivalence equiv.
  Proof with auto.
    unfold equiv.
    unfold match_packet.
    split.
    unfold Reflexive...
    unfold Symmetric...
    unfold Transitive...
    intros.
    rewrite -> H...
  Qed.
  
  Instance Pattern_Equivalence : Equivalence equiv := equiv_is_Equivalence.

  Lemma beq_true_spec : forall p p',
    beq p p' = true ->
    equiv p p'.
  Proof with auto.
    intros.
    unfold equiv.
    unfold match_packet.
    intros.
    unfold beq in H.
    destruct (eq_dec p p'); subst...
    inversion H.
  Qed.

  Lemma match_packet_spec : forall pt pk pat,
    match_packet pt pk pat = 
    negb (is_empty (inter (exact_pattern pk pt) pat)).
  Proof.
    intros.
    unfold match_packet.
    unfold is_empty.
    unfold inter.
    unfold exact_pattern.
    unfold match_packet.
    reflexivity.
  Qed.

  Lemma all_is_not_empty : is_empty all = false.
  Proof.
    reflexivity.
  Qed.

End Make.