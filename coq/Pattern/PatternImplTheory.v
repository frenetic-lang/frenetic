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
Require Import Pattern.PatternImplDef.
Require Import Wildcard.Wildcard.
Require Import Wildcard.Theory.
Require Import OpenFlow.OpenFlowSemantics.

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
  forall (pt : portId) (pkt : packet) pat1 pat2,
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
  forall (pt : portId) (pkt : packet) pat1 pat2,
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

Lemma no_match_subset_r : forall k n t t',
  match_packet n k t' = false -> 
  match_packet n k (inter t t') = false.
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
  unfold inter.
  simpl.
  repeat rewrite -> exact_match_inter_l...
Qed.

Lemma all_spec : forall pt pk,
  match_packet pt pk all = true.
Proof with auto.
  intros.
  unfold match_packet.
  rewrite -> negb_true_iff.
  unfold all.
  simpl.
  reflexivity.
Qed.

Lemma exact_match_is_exact : forall pk pt,
  is_exact (exact_pattern pk pt) = true.
Proof with auto.
  intros.
  unfold exact_pattern.
  unfold is_exact.
  unfold Wildcard.is_exact.
  simpl.
  unfold Wildcard_of_option.
  simpl.
  destruct (pktTpSrc pk); destruct (pktTpDst pk)...
Qed.

Lemma exact_intersect : forall k n t,
  match_packet k n t = true ->
  inter (exact_pattern n k) t = exact_pattern n k.
Proof with auto.
  intros.
  unfold match_packet in H.
  rewrite -> negb_true_iff in H.
  apply exact_match_inter...
Qed.

Lemma is_match_true_inter : forall pat1 pat2 pt pk,
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

Hint Constructors ValidPattern.

Lemma exact_is_valid : forall pt pk, ValidPattern (exact_pattern pk pt).
Proof with auto with datatypes.
  intros.
  unfold exact_pattern.
  destruct pk; simpl.
  destruct pktNwHeader; simpl...
  destruct i; simpl.
  destruct pktTpHeader; simpl...
Admitted.
(*  apply ValidPat_TCPUDP. unfold SupportedNwProto...
  destruct a...
Qed.
*)

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

Lemma dlTyp_None_Valid : forall dlSrc dlDst dlVlan dlVlanPcp
  nwSrc nwDst nwProto nwTos tpSrc tpDst inPort,
  ValidPattern (Pattern dlSrc dlDst WildcardNone dlVlan dlVlanPcp
                        nwSrc nwDst nwProto nwTos
                        tpSrc tpDst inPort).
Proof with auto with bool.
  intros.
  apply ValidPat_None.
  unfold is_empty.
  simpl.
  rewrite -> orb_true_r...
Qed.

Lemma nwProto_None_Valid : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp
  nwSrc nwDst nwTos tpSrc tpDst inPort,
  ValidPattern (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp
                        nwSrc nwDst WildcardNone nwTos
                        tpSrc tpDst inPort).
Proof with auto with bool.
  intros.
  apply ValidPat_None.
  unfold is_empty...
Qed.

Lemma tpSrc_None_Valid : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp
  nwSrc nwDst nwProto nwTos tpDst inPort,
  ValidPattern (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp
                        nwSrc nwDst nwProto nwTos
                        WildcardNone tpDst inPort).
Proof with auto with bool.
  intros.
  apply ValidPat_None.
  unfold is_empty...
Qed.

Lemma tpDst_None_Valid : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp
  nwSrc nwDst nwProto nwTos tpSrc inPort,
  ValidPattern (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp
                        nwSrc nwDst nwProto nwTos
                        tpSrc WildcardNone inPort).
Proof with auto with bool.
  intros.
  apply ValidPat_None.
  unfold is_empty...
Qed.

Hint Immediate dlTyp_None_Valid nwProto_None_Valid tpSrc_None_Valid 
  tpDst_None_Valid.

Lemma empty_valid_l : forall pat pat',
  is_empty pat = true ->
  ValidPattern (inter pat pat').
Proof with auto.
  intros.
  apply ValidPat_None.
  apply is_empty_true_l...
Qed.

Lemma empty_valid_r : forall pat pat',
  is_empty pat' = true ->
  ValidPattern (inter pat pat').
Proof with auto.
  intros.
  apply ValidPat_None.
  apply is_empty_true_r...
Qed.

Ltac inter_solve :=
  unfold inter; simpl; autorewrite with wildcard using auto.

Lemma dlTyp_inter_exact_r : forall dlSrc dlDst dlTyp k dlVlan dlVlanPcp 
  nwSrc nwDst nwProto nwTos tpSrc tpDst inPort,
  ValidPattern 
    (Pattern dlSrc dlDst (WildcardExact k)
      dlVlan dlVlanPcp nwSrc nwDst nwProto nwTos tpSrc tpDst inPort) ->
  ValidPattern 
    (Pattern dlSrc dlDst WildcardNone
      dlVlan dlVlanPcp nwSrc nwDst nwProto nwTos tpSrc tpDst inPort) ->
  ValidPattern
    (Pattern dlSrc dlDst 
      (Wildcard.inter Word16.eq_dec dlTyp (WildcardExact k))
      dlVlan dlVlanPcp nwSrc nwDst nwProto nwTos tpSrc tpDst inPort).
Proof with auto.
  intros.
  destruct (is_exact_split_r Word16.eq_dec dlTyp k).
  assert (Wildcard.inter Word16.eq_dec dlTyp (WildcardExact k) = 
         WildcardExact k) as J...
  rewrite -> J...
  assert (Wildcard.inter Word16.eq_dec dlTyp (WildcardExact k) = 
          WildcardNone) as J...
  rewrite -> J...
Qed.

Lemma dlTyp_inter_exact_l : forall dlSrc dlDst dlTyp k dlVlan dlVlanPcp 
  nwSrc nwDst nwProto nwTos tpSrc tpDst inPort,
  ValidPattern 
    (Pattern dlSrc dlDst (WildcardExact k)
      dlVlan dlVlanPcp nwSrc nwDst nwProto nwTos tpSrc tpDst inPort) ->
  ValidPattern 
    (Pattern dlSrc dlDst WildcardNone
      dlVlan dlVlanPcp nwSrc nwDst nwProto nwTos tpSrc tpDst inPort) ->
  ValidPattern
    (Pattern dlSrc dlDst 
      (Wildcard.inter Word16.eq_dec (WildcardExact k) dlTyp)
      dlVlan dlVlanPcp nwSrc nwDst nwProto nwTos tpSrc tpDst inPort).
Proof with auto.
  intros.
  destruct (is_exact_split_l Word16.eq_dec k dlTyp).
  assert (Wildcard.inter Word16.eq_dec (WildcardExact k) dlTyp = 
         WildcardExact k) as J...
  rewrite -> J...
  assert (Wildcard.inter Word16.eq_dec (WildcardExact k) dlTyp = 
          WildcardNone) as J...
  rewrite -> J...
Qed.

Hint Resolve dlTyp_inter_exact_l dlTyp_inter_exact_r.

Axiom zero_not_supportedProto : In Word8.zero SupportedNwProto -> False.

Hint Resolve zero_not_supportedProto.

Lemma inter_preserves_valid : forall pat1 pat2,
  ValidPattern pat1 ->
  ValidPattern pat2 ->
  ValidPattern (inter pat1 pat2).
Proof with auto.
  intros pat1 pat2 H H0.

  destruct H; destruct H0;
    try solve [ auto using empty_valid_l, empty_valid_r |
                inter_solve; auto ].

  pose (X := Word8.eq_dec nwProto nwProto0);  destruct X; subst; inter_solve.

  inter_solve.
  pose (J0 := is_exact_split_r Word16.eq_dec tpSrc Word16.zero).
  pose (J1 := is_exact_split_r Word16.eq_dec tpDst Word16.zero).
  destruct J0. destruct J1.
  rewrite -> H0.
  rewrite -> H1...
  rewrite -> H1...
  rewrite -> H0...

  inter_solve.
  apply dlTyp_inter_exact_l...
  remember (Word8.eq_dec nwProto Word8.zero) as J0.
  destruct J0; subst...

  inter_solve.
  destruct (is_exact_split_l Word8.eq_dec nwProto nwProto0).
  assert (Wildcard.inter Word8.eq_dec (WildcardExact nwProto)
    nwProto0 = WildcardExact nwProto)...
  rewrite -> H1...
  assert (Wildcard.inter Word8.eq_dec (WildcardExact nwProto)
    nwProto0 = WildcardNone)...
  rewrite -> H1...

  inter_solve.
  pose (J := is_exact_split_r Word8.eq_dec nwProto nwProto0).
  destruct J.
  inter_solve.
  assert (Wildcard.inter Word8.eq_dec nwProto
                      (WildcardExact nwProto0) = WildcardExact nwProto0)...
  rewrite -> H1.
  pose (J0 := is_exact_split_l Word16.eq_dec Word16.zero tpSrc).
  pose (J1 := is_exact_split_l Word16.eq_dec Word16.zero tpDst).
  destruct J0.
  destruct J1.
  rewrite -> H2.
  rewrite -> H3...
  rewrite -> H3...
  rewrite -> H2...
  inter_solve.
  assert (Wildcard.inter Word8.eq_dec nwProto
                      (WildcardExact nwProto0) = WildcardNone)...
  rewrite -> H1...

  inter_solve.
  apply dlTyp_inter_exact_r...
  remember (Word8.eq_dec Word8.zero nwProto) as J0.
  destruct J0; subst...

  inter_solve.
  destruct (is_exact_split_r Word8.eq_dec nwProto nwProto0).
  assert (Wildcard.inter Word8.eq_dec nwProto (WildcardExact nwProto0)
    = WildcardExact nwProto0)...
  rewrite -> H1...
  assert (Wildcard.inter Word8.eq_dec nwProto (WildcardExact nwProto0)
    = WildcardNone)...
  rewrite -> H1...
Qed.

Section Equivalence.
  
  Definition Pattern_equiv (pat1 pat2 : pattern) : Prop :=
    forall pt pk, 
      match_packet pt pk pat1 = match_packet pt pk pat2.

  Hint Unfold Pattern_equiv.
  
  Lemma Pattern_equiv_is_Equivalence : Equivalence Pattern_equiv.
  Proof with auto.
    split.
    unfold Reflexive...
    unfold Symmetric...
    unfold Transitive.
      unfold Pattern_equiv.
      intros.
      rewrite -> H...
    Qed.
  
End Equivalence.

Instance Pattern_Equivalance : Equivalence Pattern_equiv.
apply Pattern_equiv_is_Equivalence.
Qed.

(*
Lemma match_opt_wildcard_equiv : forall (A : Type)
    (eq_dec : Eqdec A) (val : A) (opt : option A),
  negb (Wildcard.is_empty
          (Wildcard.inter eq_dec (WildcardExact val) (to_wild opt))) =     
  match_opt eq_dec val opt.
Proof with auto.
  intros.
  destruct opt...
  simpl.
  unfold Wildcard.inter.
  destruct (eq_dec val a)...
Qed.
*)

Lemma match_opt_const_equiv : forall (A : Type)
    (eq_dec : Eqdec A) (val : A) (opt : A),
  negb (Wildcard.is_empty
          (Wildcard.inter eq_dec (WildcardExact val) (WildcardExact opt))) =
  if eq_dec val opt then true else false.
Proof with auto.
  intros.
  unfold Wildcard.inter.
  destruct (eq_dec opt val); subst...
  destruct (eq_dec val val)...
  destruct (eq_dec val opt); subst...
Qed.

Lemma trans : forall (A : Type) (eq : Eqdec A) (x : A) 
  (w : Wildcard A) (H : w <> WildcardNone),
  match_opt eq x (Wildcard.to_option H) = 
  negb (Wildcard.is_empty (Wildcard.inter eq (WildcardExact x) w)).
Proof with auto.
  intros.
  destruct w.
  simpl.
  remember (eq x a) as b.
  destruct b.
  subst...
  rewrite -> inter_exact_eq.
  unfold Wildcard.is_empty...
  rewrite -> inter_exact_neq...
  simpl...
  contradiction H...
Qed.

  Lemma icmp_tpSrc : forall pat,
    ValidPattern pat ->
    is_empty pat = false ->
    ptrnNwProto pat = WildcardExact Const_0x1 ->
    ptrnTpSrc pat = WildcardAll \/ ptrnTpSrc pat = WildcardExact Word16.zero.
  Proof with auto.
    intros.
    destruct pat.
    simpl in *.
    subst.
    inversion H; subst...
    unfold SupportedNwProto in H2.
    destruct H2. inversion H1. destruct H1. inversion H1. inversion H1.
    simpl in H1.
    simpl in H0.
    rewrite -> H0 in H1.
    inversion H1.
  Qed.

  Lemma icmp_tpDst : forall pat,
    ValidPattern pat ->
    is_empty pat = false ->
    ptrnNwProto pat = WildcardExact Const_0x1 ->
    ptrnTpDst pat = WildcardAll \/ ptrnTpDst pat = WildcardExact Word16.zero.
  Proof with auto.
    intros.
    destruct pat.
    simpl in *.
    subst.
    inversion H; subst...
    unfold SupportedNwProto in H2.
    destruct H2. inversion H1. destruct H1. inversion H1. inversion H1.
    simpl in H1.
    simpl in H0.
    rewrite -> H0 in H1.
    inversion H1.
  Qed.

Theorem match_equiv : forall pt pk pat (Hempty : is_empty pat = false),
  ValidPattern pat ->
  match_ethFrame pk pt (to_match pat Hempty) =
  match_packet pt pk pat.
Proof with auto.
  intros.
  destruct pat.
  destruct pk.
  unfold match_packet.
  inversion H.
  (* TCP / UDP packet *)
  subst.
  simpl.
  repeat rewrite -> negb_orb.
  destruct (Word16.eq_dec pktDlTyp Const_0x800).
  destruct pktNwHeader.
  destruct i.
  destruct (Word8.eq_dec pktIPProto nwProto).
  destruct pktTpHeader.
  destruct t.
  subst.
  repeat rewrite -> trans.
  repeat rewrite -> andb_assoc.
  simpl.
  repeat rewrite -> inter_exact_eq.
  simpl.
  rewrite -> andb_true_r.
  rewrite -> andb_true_r.
  reflexivity.
  destruct i.
  subst.
  repeat rewrite -> trans.
  repeat rewrite -> andb_assoc.
  simpl.
  repeat rewrite -> inter_exact_eq.
  simpl.
  rewrite -> andb_true_r.
  rewrite -> andb_true_r.
  rewrite -> andb_true_r.
  (* Need to show that ptrnTpSrc and ptrnTpDst are zero *)
  assert (
   negb
     (Wildcard.is_empty
        (Wildcard.inter Word16.eq_dec (WildcardExact Word16.zero) ptrnTpSrc)) =
     true).
    destruct (icmp_tpSrc H Hempty)...
Admitted.
(*
    simpl in H0.
    rewrite -> H0...
    simpl in H0.
    rewrite -> H0...
    rewrite -> inter_exact_eq...
  admit. (* TODO(arjun): aliases *)
  (* next case: unparsable *)
  admit.
  (* Case: pattern is for TCP/UDP but packet is not. *)
  unfold SupportedNwProto in H1.
  simpl in H1.
  destruct H1 as [H1 | [ H1 | H1]]; subst.
  repeat rewrite -> trans.
  repeat rewrite -> andb_assoc.
  simpl.
  destruct pktTpHeader.
    contradiction n...
    rewrite -> (inter_exact_neq _ n)...
    simpl.
    rewrite -> andb_false_r.
    rewrite -> andb_false_r.
    rewrite -> andb_false_l.
    reflexivity.
    rewrite -> andb_false_r.
    rewrite -> (inter_exact_neq _ n)...
    simpl.
    rewrite -> andb_false_r.
    rewrite -> andb_false_l.
    reflexivity.
  repeat rewrite -> trans.
  repeat rewrite -> andb_assoc.
  simpl.
  destruct pktTpHeader;
    rewrite -> (inter_exact_neq _ n); auto;
    simpl;
    rewrite -> andb_false_r;
    rewrite -> andb_false_r;
    rewrite -> andb_false_l;
    reflexivity.
  inversion H1.

  assert (Const_0x806 <> Const_0x800).
    admit. (* TODO(arjun) isn't this somewhere? *)
  contradiction e.

  (* Next case: boring, we've seen a symmetric case alraedy. *)
  admit.

  (* Next case: Not an IP packet, but the pattern is for IP. *)
  repeat rewrite -> trans.
  repeat rewrite -> andb_assoc.
  destruct pktNwHeader.
  contradiction n...
  (* got an ARP packet *)
  destruct a;
  rewrite -> andb_false_r;
  rewrite -> (inter_exact_neq _ n); auto;
  rewrite -> andb_false_r;
  rewrite -> andb_false_l;
  reflexivity.
  (* got a non-ARP, non-IP packet *)
  rewrite -> andb_false_r.
  rewrite -> (inter_exact_neq _ n); auto.
  rewrite -> andb_false_r.
  rewrite -> andb_false_l.
  reflexivity.

  (* Next case *)
  subst.
  simpl.
  repeat rewrite -> trans.
  destruct (Word16.eq_dec pktDlTyp Const_0x806).
  (* ARP pattern and ARP packet! *)
  destruct pktNwHeader.
  admit. (* 800 <> 806 contradiction *)
  repeat rewrite -> trans.
  repeat rewrite -> negb_orb.
  destruct a; repeat rewrite -> trans;
  repeat rewrite -> inter_exact_eq; auto;
  simpl;
  repeat rewrite -> andb_true_r;
  repeat rewrite -> andb_assoc;
  reflexivity.

  (* Case: unparsable *)
  admit.

  (* case: ARP Pattern, but not ARP packet *)
  repeat rewrite -> andb_assoc.
  rewrite -> andb_false_r.
  repeat rewrite -> negb_orb.
  rewrite -> (inter_exact_neq _ n)...
  simpl.
  rewrite -> andb_false_r.
  rewrite -> andb_false_l.
  reflexivity.

  (* case: unparsable *)
  subst.
  destruct pktNwHeader.
  destruct i.
  simpl.
  destruct (Word16.eq_dec Const_0x800 Const_0x800).
  repeat rewrite -> trans.
  destruct ptrnNwProto.
  subst.
  simpl.
  destruct (Word8.eq_dec pktIPProto n).
  subst.
  destruct pktTpHeader.
  destruct t.
  simpl.
  repeat rewrite -> negb_orb.
  repeat rewrite -> andb_assoc.
  rewrite -> inter_exact_eq...
  rewrite -> inter_exact_eq...
  destruct (Word16.eq_dec tcpSrc Word16.zero).
  destruct (Word16.eq_dec tcpDst Word16.zero).
  subst.
  repeat rewrite -> inter_exact_eq...
  simpl.
  repeat rewrite -> andb_true_r.
  reflexivity.
  rewrite -> andb_false_r.
  rewrite -> (inter_exact_neq _ n)...
  simpl.
  rewrite -> andb_false_r.
  reflexivity.
  rewrite -> andb_false_r.
  rewrite -> (inter_exact_neq _ n)...
  simpl.
  rewrite -> andb_false_r.
  reflexivity.
  destruct i.
  repeat rewrite -> negb_orb.
  repeat rewrite -> andb_assoc.
  repeat rewrite -> inter_exact_eq...
  simpl.
  repeat rewrite -> andb_true_r.
  reflexivity.
  repeat rewrite -> negb_orb.
  repeat rewrite -> andb_assoc.
  repeat rewrite -> inter_exact_eq...
  simpl.
  repeat rewrite -> andb_true_r.
  reflexivity.

Admitted.
*)
