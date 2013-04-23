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
Require Import Pattern.Pattern.
Require Import Pattern.Valid.
Require Import Wildcard.Wildcard.
Require Import Wildcard.Theory.

Open Scope bool_scope.
Open Scope list_scope.
Open Scope equiv_scope.

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

Module PatMatchable.

  Definition t := pattern.

  Definition inter := Pattern.inter.
  Definition empty := Pattern.empty.
  Definition is_empty := Pattern.is_empty.
  Definition is_exact := Pattern.is_exact.

  Hint Unfold inter empty is_empty 
    Pattern.inter Pattern.empty Pattern.is_empty.

  Hint Resolve Word8.eq_dec Word16.eq_dec Word32.eq_dec Word48.eq_dec.
 
  Lemma inter_comm : forall p p', inter p p' = inter p' p.
  Proof with auto.
    intros.
    destruct p.
    destruct p'. 
    unfold inter.
    unfold Pattern.inter.
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
    unfold Pattern.inter.
    simpl.
    repeat rewrite -> inter_assoc...
  Qed.


  Lemma is_empty_false_distr_l : forall x y,
    is_empty (inter x y) = false -> 
    is_empty x = false .
  Proof with simpl; eauto.
    intros.
    unfold inter in H.
    unfold Pattern.inter in H.
    simpl in H.
    repeat rewrite -> orb_false_iff in H.
    do 11 (destruct H).
    unfold is_empty.
    unfold Pattern.is_empty.
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
    forall (pt : portId) (pkt : packet) pat1 pat2,
      Pattern.match_packet pt pkt pat1 = false ->
      Pattern.match_packet pt pkt (inter pat1 pat2) = false.
  Proof with auto.
    intros.
    unfold Pattern.match_packet in *.
    rewrite -> negb_false_iff in H.
    rewrite -> negb_false_iff.
    rewrite -> inter_assoc.
    apply is_empty_true_l...
  Qed.

  Lemma no_match_subset_r : forall k n t t',
    Pattern.match_packet n k t' = false -> 
    Pattern.match_packet n k (inter t t') = false.
  Proof with auto.
    intros.
    rewrite -> inter_comm.
    apply is_match_false_inter_l...
  Qed.

  Lemma exact_match_inter : forall x y,
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
    unfold Pattern.inter.
    simpl.
    repeat rewrite -> exact_match_inter_l...
  Qed.

  Lemma all_spec : forall pt pk,
    Pattern.match_packet pt pk Pattern.all = true.
  Proof with auto.
    intros.
    unfold Pattern.match_packet.
    rewrite -> negb_true_iff.
    unfold Pattern.all.
    destruct pk.
    unfold Pattern.exact_pattern.
    unfold Pattern.inter.
    unfold Pattern.Wildcard_of_option in *.
    destruct pktTpSrc; destruct pktTpDst; simpl...
  Qed.

  Lemma exact_match_is_exact : forall pk pt,
    Pattern.is_exact (Pattern.exact_pattern pk pt) = true.
  Proof with auto.
    intros.
    unfold Pattern.exact_pattern.
    unfold Pattern.is_exact.
    unfold Wildcard.is_exact.
    simpl.
    unfold Pattern.Wildcard_of_option.
    simpl.
    destruct (pktTpSrc pk); destruct (pktTpDst pk)...
  Qed.

  Lemma exact_intersect : forall k n t,
    Pattern.match_packet k n t = true ->
    Pattern.inter (Pattern.exact_pattern n k) t = Pattern.exact_pattern n k.
  Proof with auto.
    intros.
    unfold Pattern.match_packet in H.
    rewrite -> negb_true_iff in H.
    apply exact_match_inter...
  Qed.

  Lemma is_match_true_inter : forall pat1 pat2 pt pk,
    Pattern.match_packet pt pk pat1 = true ->
    Pattern.match_packet pt pk pat2 = true ->
    Pattern.match_packet pt pk (Pattern.inter pat1 pat2) = true.
  Proof with auto.
    intros.
    apply exact_intersect in H.
    apply exact_intersect in H0.
    unfold Pattern.match_packet.
    rewrite -> negb_true_iff.
    rewrite -> inter_assoc.
    unfold inter.
    rewrite -> H.
    rewrite -> H0.
    unfold Pattern.exact_pattern.
    unfold Pattern.is_empty.
    simpl.
    unfold Pattern.Wildcard_of_option.
    destruct (pktTpSrc pk); destruct (pktTpDst pk)...
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

  Hint Constructors ValidPattern.

  Lemma pres0 : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
    nwProto nwProto' nwTos tpSrc tpDst inPort,
    nwProto <> nwProto' ->
    ValidPattern (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                            (Wildcard.inter Word8.eq_dec 
                                (WildcardExact nwProto)
                                (WildcardExact nwProto'))
                            nwTos tpSrc tpDst inPort).
  Proof with auto with wildcard.
    intros.
    apply ValidPat_None.
    simpl.
    repeat rewrite -> orb_true_iff.
    repeat rewrite -> or_assoc.
    rewrite -> inter_exact_neq...
    simpl.
    auto 13.
  Qed.

  Hint Immediate pres0.

  Lemma pres1 : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
    nwProto nwProto' nwTos tpSrc tpDst inPort,
    In nwProto SupportedNwProto ->
    ~ In nwProto' SupportedNwProto ->
    ValidPattern (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                            (Wildcard.inter Word8.eq_dec 
                                (WildcardExact nwProto)
                                (WildcardExact nwProto'))
                            nwTos tpSrc tpDst inPort).
  Proof with auto.
    intros.
    pose (X := Word8.eq_dec nwProto nwProto').
    destruct X; subst...
  Qed.
  
  Hint Immediate pres1.

  Lemma pres1' : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
    nwProto nwProto' nwTos tpSrc tpDst inPort,
    ~ In nwProto SupportedNwProto ->
    In nwProto' SupportedNwProto ->
    ValidPattern (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                            (Wildcard.inter Word8.eq_dec 
                                (WildcardExact nwProto)
                                (WildcardExact nwProto'))
                            nwTos tpSrc tpDst inPort).
  Proof with auto.
    intros.
    pose (X := Word8.eq_dec nwProto nwProto').
    destruct X; subst...
  Qed.
  
  Hint Immediate pres1'.

  Lemma pres2 : forall dlSrc dlDst dlTyp dlTyp' dlVlan dlVlanPcp nwSrc nwDst
    nwProto nwTos tpSrc tpDst inPort,
    dlTyp <> dlTyp' ->
    ValidPattern (Pattern dlSrc dlDst 
                    (Wildcard.inter Word16.eq_dec 
                      (WildcardExact dlTyp)
                      (WildcardExact dlTyp'))
                    dlVlan dlVlanPcp nwSrc nwDst nwProto
                    nwTos tpSrc tpDst inPort).
  Proof with auto.
    intros.
    apply ValidPat_None.
    simpl.
    repeat rewrite -> orb_true_iff.
    repeat rewrite -> or_assoc.
    rewrite -> inter_exact_neq; auto 13.
  Qed.

  Hint Immediate pres2.

  Lemma pres3 : forall dlSrc dlDst dlTyp dlTyp' dlVlan dlVlanPcp nwSrc nwDst
    nwProto nwTos tpSrc tpDst inPort,
    In dlTyp SupportedDlTyp ->
    ~ In dlTyp' SupportedDlTyp ->
    ValidPattern (Pattern dlSrc dlDst 
                    (Wildcard.inter Word16.eq_dec 
                      (WildcardExact dlTyp)
                      (WildcardExact dlTyp'))
                    dlVlan dlVlanPcp nwSrc nwDst nwProto
                    nwTos tpSrc tpDst inPort).
    Proof with auto.
      intros.
      pose (X := Word16.eq_dec dlTyp dlTyp').
      destruct X; subst...
    Qed.

    Hint Resolve pres3.

  Lemma pres3' : forall dlSrc dlDst dlTyp dlTyp' dlVlan dlVlanPcp nwSrc nwDst
    nwProto nwTos tpSrc tpDst inPort,
    ~ In dlTyp SupportedDlTyp ->
    In dlTyp' SupportedDlTyp ->
    ValidPattern (Pattern dlSrc dlDst 
                    (Wildcard.inter Word16.eq_dec 
                      (WildcardExact dlTyp)
                      (WildcardExact dlTyp'))
                    dlVlan dlVlanPcp nwSrc nwDst nwProto
                    nwTos tpSrc tpDst inPort).
    Proof with auto.
      intros.
      pose (X := Word16.eq_dec dlTyp dlTyp').
      destruct X; subst...
    Qed.

    Hint Resolve pres3'.

    Lemma pres4 : In Const_0x800 SupportedDlTyp.
    Proof with auto with datatypes.
      intros.
      unfold SupportedDlTyp...
    Qed.

    Hint Resolve pres4.

    Lemma pres4' : In Const_0x806 SupportedDlTyp.
    Proof with auto with datatypes.
      intros.
      unfold SupportedDlTyp...
    Qed.

    Hint Resolve pres4'.

    Lemma pres5 : forall dlSrc dlDst  dlVlan dlVlanPcp nwSrc nwDst
                         nwProto nwTos tpSrc tpDst inPort,
    ValidPattern (Pattern dlSrc dlDst 
                    (Wildcard.inter Word16.eq_dec 
                      (WildcardExact Const_0x800)
                      (WildcardExact Const_0x806))
                    dlVlan dlVlanPcp nwSrc nwDst nwProto
                    nwTos tpSrc tpDst inPort).
    Proof with auto.
      intros.
      apply pres2.
      exact IP_ARP_frametyp_neq.
    Qed.

    Hint Immediate pres5.

    Lemma pres5' : forall dlSrc dlDst  dlVlan dlVlanPcp nwSrc nwDst
                          nwProto nwTos tpSrc tpDst inPort,
    ValidPattern (Pattern dlSrc dlDst 
                    (Wildcard.inter Word16.eq_dec 
                      (WildcardExact Const_0x806)
                      (WildcardExact Const_0x800))
                    dlVlan dlVlanPcp nwSrc nwDst nwProto
                    nwTos tpSrc tpDst inPort).
    Proof with auto.
      intros.
      apply pres2.
      unfold not.
      intros.
      symmetry in H.
      apply IP_ARP_frametyp_neq...
    Qed.

    Hint Immediate pres5'.

    Lemma empty_valid_l : forall pat pat',
      is_empty pat = true ->
      ValidPattern (Pattern.inter pat pat').
    Proof with auto.
      intros.
      apply ValidPat_None.
      apply is_empty_true_l...
    Qed.

    Lemma empty_valid_r : forall pat pat',
      is_empty pat' = true ->
      ValidPattern (Pattern.inter pat pat').
    Proof with auto.
      intros.
      apply ValidPat_None.
      apply is_empty_true_r...
    Qed.

  Ltac inter_solve :=
    unfold Pattern.inter; simpl; autorewrite with wildcard using auto.

  Lemma inter_preserves_valid : forall pat1 pat2,
    ValidPattern pat1 ->
    ValidPattern pat2 ->
    ValidPattern (Pattern.inter pat1 pat2).
  Proof with auto.
    intros pat1 pat2 H H0.

    destruct H; destruct H0; 
      try solve [ auto using empty_valid_l, empty_valid_r ].
  
    pose (X := Word8.eq_dec nwProto nwProto0);  destruct X; subst; inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    pose (X := Word8.eq_dec nwProto nwProto0);  destruct X; subst; inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    pose (X := Word16.eq_dec frameTyp frameTyp0); 
      destruct X; subst; inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
    inter_solve.
  Qed.

End PatMatchable.

Section Equivalence.
  
  Inductive Pattern_equiv : pattern -> pattern -> Prop :=
  | Pattern_equiv_match : forall pat1 pat2,
    (forall pt pk, 
      Pattern.match_packet pt pk pat1 = Pattern.match_packet pt pk pat2) ->
    Pattern_equiv pat1 pat2.

  Hint Constructors Pattern_equiv.
  
  Lemma Pattern_equiv_is_Equivalence : Equivalence Pattern_equiv.
  Proof with auto.
    split.
    unfold Reflexive...
    unfold Symmetric. intros. inversion H...
    unfold Transitive. intros. inversion H. inversion H0. subst.
    split. intros. rewrite -> H1...
  Qed.
  
End Equivalence.

Instance Pattern_Equivalance : Equivalence Pattern_equiv.
  apply Pattern_equiv_is_Equivalence.
Qed.

