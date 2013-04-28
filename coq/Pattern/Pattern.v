Set Implicit Arguments.

Require Import Pattern.PatternImplDef.
Require Import Pattern.PatternImplTheory.
Require Import Pattern.PatternInterface.
Require Import Coq.Lists.List.
Require Import Wildcard.Wildcard.
Require Import Network.NetworkPacket.
Require Import Coq.Classes.Equivalence.

Local Open Scope equiv_scope.

Module Pattern : PATTERN.

  Record pat := Pat {
    raw : pattern;
    valid : ValidPattern raw
  }.

  Definition t := pat.

  Definition beq (p1 p2 : t) :=
    match eq_dec (raw p1) (raw p2) with
      | left _ => true
      | right _ => false
    end.

  Definition inter (p1 p2 : t) := 
    Pat (inter_preserves_valid (valid p1) (valid p2)).


  Lemma all_is_Valid : ValidPattern all.
  Proof.
    apply ValidPat_any.
  Qed.

  Definition all : t := Pat all_is_Valid.

  Lemma empty_is_valid : ValidPattern empty.
  Proof.
    apply ValidPat_None.
    reflexivity.
  Qed.

  Definition empty : t := Pat empty_is_valid.

  Definition exact_pattern pk pt : t :=
    Pat (exact_is_valid pt pk).

  Definition is_empty pat : bool := is_empty (raw pat).

  Definition match_packet pt pk pat : bool :=
    match_packet pt pk (raw pat).

  Definition is_exact pat : bool := is_exact (raw pat).

  Definition to_match pat (H : is_empty pat = false) :=
    to_match (raw pat) H.

  Section Constructors.

    Definition inPort pt : t :=
      @Pat
        (Pattern 
           WildcardAll
           WildcardAll
           WildcardAll
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll
           (WildcardExact pt))
        (ValidPat_any _ _ _ _ _ _).

    Definition dlSrc dlAddr : t :=
      @Pat
        (Pattern 
          (WildcardExact dlAddr)
           WildcardAll
           WildcardAll
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll)
        (ValidPat_any (WildcardExact dlAddr) _ _ _ _ _).

    Definition dlDst dlAddr : t :=
      @Pat
        (Pattern 
           WildcardAll
          (WildcardExact dlAddr)
           WildcardAll
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll)
        (ValidPat_any _ (WildcardExact dlAddr) _ _ _ _).

    Definition dlTyp typ : t :=
      @Pat
        (Pattern 
           WildcardAll
           WildcardAll
           (WildcardExact typ)
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll)
        (ValidPat_any _ _ (WildcardExact typ) _ _ _).

    Definition dlVlan vlan : t :=
      @Pat
        (Pattern 
           WildcardAll
           WildcardAll
           WildcardAll
           (WildcardExact vlan)
           WildcardAll
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll)
        (ValidPat_any _ _ _ (WildcardExact vlan) _ _).

    Definition dlVlanPcp pcp : t :=
      @Pat
        (Pattern 
           WildcardAll
           WildcardAll
           WildcardAll
           WildcardAll
           (WildcardExact pcp)
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll 
           WildcardAll
           WildcardAll)
        (ValidPat_any _ _ _ _ (WildcardExact pcp) _).

    Definition ipSrc addr : t :=
      @Pat
        (Pattern
          WildcardAll
          WildcardAll
          (WildcardExact Const_0x800)
          WildcardAll
          WildcardAll
          (WildcardExact addr)
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll)
        (ValidPat_IP_any _ _ _ _ (WildcardExact addr) _ _ _ _).

    Definition ipDst addr : t :=
      @Pat
        (Pattern
          WildcardAll
          WildcardAll
          (WildcardExact Const_0x800)
          WildcardAll
          WildcardAll
          WildcardAll
          (WildcardExact addr)
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll)
        (ValidPat_IP_any _ _ _ _ _ (WildcardExact addr) _ _ _).

    Definition ipProto proto : t :=
      @Pat
        (Pattern
          WildcardAll
          WildcardAll
          (WildcardExact Const_0x800)
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll
          (WildcardExact proto)
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll)
        (ValidPat_IP_any _ _ _ _ _ _ _ _ (WildcardExact proto)).

    Definition tpSrcPort proto (H : In proto SupportedNwProto) tpPort : t :=
      @Pat
        (Pattern
          WildcardAll
          WildcardAll
          (WildcardExact Const_0x800)
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll
          (WildcardExact proto)
          WildcardAll
          (WildcardExact tpPort)
          WildcardAll
          WildcardAll)
        (@ValidPat_TCPUDP _ _ _ _ _ _ _ (WildcardExact tpPort) _ _ _ H).

    Definition tpDstPort proto (H : In proto SupportedNwProto) tpPort : t :=
      @Pat
        (Pattern
          WildcardAll
          WildcardAll
          (WildcardExact Const_0x800)
          WildcardAll
          WildcardAll
          WildcardAll
          WildcardAll
          (WildcardExact proto)
          WildcardAll
          WildcardAll
          (WildcardExact tpPort)
          WildcardAll)
        (@ValidPat_TCPUDP _ _ _ _ _ _ _ _ (WildcardExact tpPort) _ _ H).

    Lemma TCP_is_supported : In Const_0x6 SupportedNwProto.
    Proof with auto with datatypes.
      unfold SupportedNwProto...
    Qed.

    Lemma UDP_is_supported : In Const_0x7 SupportedNwProto.
    Proof with auto with datatypes.
      unfold SupportedNwProto...
    Qed.

    Definition tcpSrcPort := tpSrcPort TCP_is_supported.

    Definition tcpDstPort := tpDstPort TCP_is_supported.

    Definition udpSrcPort := tpSrcPort UDP_is_supported.

    Definition udpDstPort := tpDstPort UDP_is_supported.

  End Constructors.


    Definition equiv (pat1 pat2 : t) : Prop :=
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


Instance Pattern_Equivalence : Equivalence equiv.
  apply equiv_is_Equivalence.
Qed.

Section Lemmas.

  Lemma inter_comm : forall (p p0 : pat),  equiv (inter p p0) (inter p0 p).
  Proof with auto.
    unfold equiv.
    unfold match_packet.
    unfold inter.
    intros.
    simpl.
    rewrite -> inter_comm...
  Qed.

  Lemma inter_assoc : forall (p p' p'' : pat),
    equiv (inter p (inter p' p'')) (inter (inter p p') p'').
  Proof with auto.
    unfold equiv.
    unfold match_packet.
    unfold inter.
    intros.
    simpl.
    rewrite -> inter_assoc...
  Qed.

  Hint Unfold inter is_empty is_exact equiv match_packet.

  Lemma is_empty_false_distr_l : forall x y,
    is_empty (inter x y) = false -> 
    is_empty x = false .
  Proof with eauto.
    intros.
    autounfold in *.
    eapply is_empty_false_distr_l...
  Qed.

  Lemma is_empty_false_distr_r : forall x y,
    is_empty (inter x y) = false -> 
    is_empty y = false.
  Proof with eauto.
    intros.
    autounfold in *.
    eapply is_empty_false_distr_r...
  Qed.

  Lemma is_empty_true_l : forall x y,
    is_empty x = true ->
    is_empty (inter x y) = true.
  Proof with eauto.
    intros.
    autounfold in *.
    eapply is_empty_true_l...
  Qed.

  Lemma is_empty_true_r : forall x y,
    is_empty y = true ->
    is_empty (inter x y) = true.
  Proof with eauto.
    intros.
    autounfold in *.
    eapply is_empty_true_r...
  Qed.

  Lemma is_match_false_inter_l :
    forall pt (pkt : packet) pat1 pat2,
      match_packet pt pkt pat1 = false ->
      match_packet pt pkt (inter pat1 pat2) = false.
  Proof with eauto.
    intros.
    autounfold in *.
    eapply is_match_false_inter_l...
  Qed.

  Lemma is_match_false_inter_r :
    forall pt (pkt : packet) pat1 pat2,
      match_packet pt pkt pat2 = false ->
      match_packet pt pkt (inter pat1 pat2) = false.
  Proof with eauto.
    intros.
    autounfold in *.
    eapply is_match_false_inter_r...
  Qed.

  Lemma no_match_subset_r : forall k n t t',
    match_packet n k t' = false -> 
    match_packet n k (inter t t') = false.
  Proof with eauto.
    intros.
    autounfold in *.
    eapply no_match_subset_r...
  Qed.

  Lemma exact_match_inter : forall x y,
    is_exact x = true ->
    is_empty (inter x y) = false ->
    equiv (inter x y) x.
  Proof with eauto.
    intros.
    unfold equiv.
    unfold match_packet.
    intros.
    destruct x.
    destruct y.
    unfold is_exact in *.
    unfold inter in *.
    unfold is_empty in *.
    simpl in H.
    simpl in H0.
    pose (J := PatternImplTheory.exact_match_inter _ _ H H0).
    simpl.
    rewrite -> J...
  Qed.

  Lemma all_spec : forall pt pk,
    match_packet pt pk all = true.
  Proof with auto.
    unfold all.
    unfold match_packet.
    simpl.
    exact all_spec.
  Qed.

  Lemma all_is_not_empty : is_empty all = false.
  Proof.
    reflexivity.
  Qed.

  Lemma exact_match_is_exact : forall pk pt,
    is_exact (exact_pattern pk pt) = true.
  Proof with auto.
    unfold exact_pattern.
    unfold is_exact.
    intros.
    apply exact_match_is_exact.
  Qed.

  Lemma exact_intersect : forall k n t,
    match_packet k n t = true ->
    equiv (inter (exact_pattern n k) t) (exact_pattern n k).
  Proof with auto.
    unfold equiv.
    unfold exact_pattern.
    unfold match_packet.
    unfold inter.
    intros.
    simpl.
    pose (J := exact_intersect k n (raw t0) H).
    rewrite -> J...
  Qed.  

  Lemma is_match_true_inter : forall pat1 pat2 pt pk,
    match_packet pt pk pat1 = true ->
    match_packet pt pk pat2 = true ->
    match_packet pt pk (inter pat1 pat2) = true.
  Proof with auto.
    intros.
    unfold match_packet in *.
    unfold inter.
    simpl.
    rewrite -> is_match_true_inter...
  Qed.

  Lemma beq_true_spec : forall p p',
    beq p p' = true ->
    equiv p p'.
  Proof with auto.
    intros.
    unfold equiv.
    unfold match_packet.
    destruct p.
    destruct p'.
    unfold beq in H.
    simpl in H.
    destruct (eq_dec raw0 raw1); subst...
    inversion H.
  Qed.

  Lemma match_packet_spec : forall pt pk pat,
    match_packet pt pk pat = 
    negb (is_empty (inter (exact_pattern pk pt) pat)).
  Proof.
    intros.
    destruct pat0.
    unfold match_packet.
    unfold is_empty.
    unfold inter.
    unfold exact_pattern.
    unfold PatternImplDef.match_packet.
    unfold raw.
    reflexivity.
  Qed.

End Lemmas.

End Pattern.

Definition pattern := Pattern.t.
