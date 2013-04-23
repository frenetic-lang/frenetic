(** WARNING(arjun): Does not compile in Proof General. Proof General freezes
    on Pattern.inter and ValidPattern. coqc does work.
 *)
Set Implicit Arguments.

Require Import Coq.Arith.EqNat.
Require Import NPeano.
Require Import Arith.Peano_dec.
Require Import Bool.Bool.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.

Require Import OpenFlow.OpenFlow0x01Types.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import Wildcard.Wildcard.

Local Open Scope bool_scope.
Local Open Scope list_scope.

Record pattern : Type := Pattern {
  ptrnDlSrc : Wildcard dlAddr;
  ptrnDlDst : Wildcard dlAddr;
  ptrnDlType : Wildcard dlTyp;
  ptrnDlVlan : Wildcard dlVlan;
  ptrnDlVlanPcp : Wildcard dlVlanPcp;
  ptrnNwSrc : Wildcard nwAddr;
  ptrnNwDst : Wildcard nwAddr;
  ptrnNwProto : Wildcard nwProto;
  ptrnNwTos : Wildcard nwTos;
  ptrnTpSrc : Wildcard tpPort;
  ptrnTpDst : Wildcard tpPort;
  ptrnInPort : Wildcard portId
}.

Lemma eq_dec : forall (x y : pattern), { x = y } + { x <> y }.
Proof.
  decide equality;
    try solve [ apply (Wildcard.eq_dec Word16.eq_dec) |
      apply (Wildcard.eq_dec Word32.eq_dec) |
        apply (Wildcard.eq_dec Word8.eq_dec) |
          apply (Wildcard.eq_dec Word48.eq_dec) ].
Defined.

Definition Wildcard_of_option { a : Type } (def : a) (v : option a) :=
  WildcardExact (match v with
                   | None => def
                   | Some v => v
                 end).

Definition all :=
  Pattern
    WildcardAll WildcardAll WildcardAll WildcardAll WildcardAll 
    WildcardAll WildcardAll WildcardAll WildcardAll WildcardAll
    WildcardAll WildcardAll.

Definition empty :=
  Pattern WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone
  WildcardNone.

(** Note that we do not have a unique representation for empty patterns! *)
Definition is_empty pat : bool :=
  match pat with
    | Pattern dlSrc dlDst typ vlan pcp nwSrc nwDst nwProto nwTos tpSrc tpDst
      inPort =>
      Wildcard.is_empty inPort ||
        Wildcard.is_empty dlSrc ||
          Wildcard.is_empty dlDst ||
            Wildcard.is_empty vlan ||
              Wildcard.is_empty pcp || 
                Wildcard.is_empty typ ||
                  Wildcard.is_empty nwSrc || 
                    Wildcard.is_empty nwDst || 
                      Wildcard.is_empty nwTos || 
                        Wildcard.is_empty nwProto ||
                          Wildcard.is_empty tpSrc || 
                            Wildcard.is_empty tpDst
  end.

Lemma is_empty_neq_None : forall {A : Type} (w : Wildcard A),
  Wildcard.is_empty w = false -> w <> WildcardNone.
Proof.
  unfold not.
  intros.
  destruct w.

  inversion H0.
  inversion H0.
  simpl in H.
  inversion H.
Qed.

Hint Resolve is_empty_neq_None.

Lemma is_empty_dlSrc : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  dlSrc <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_dlDst : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  dlDst <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_dlTyp : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  dlTyp <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_dlVlan : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  dlVlan <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_dlVlanPcp : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  dlVlanPcp <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_nwSrc : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  nwSrc <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_nwDst : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  nwDst <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_nwProto : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  nwProto <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_nwTos : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  nwTos <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_tpSrc : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  tpSrc <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_tpDst : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  tpDst <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.

Lemma is_empty_inPort : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
  nwProto nwTos tpSrc tpDst inPort,
  is_empty (Pattern dlSrc dlDst dlTyp dlVlan dlVlanPcp nwSrc nwDst
                    nwProto nwTos tpSrc tpDst inPort) = false ->
  inPort <> WildcardNone.
Proof with auto.
  intros.
  simpl in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H)...
Qed.



(** Extracts to shockingly good code. *)
Lemma to_match : forall pat (H : is_empty pat = false), of_match.
Proof.
  intros.
  destruct pat.
  exact (Match
    (Wildcard.to_option (is_empty_dlSrc _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_dlDst _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_dlTyp _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_dlVlan _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_dlVlanPcp _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_nwSrc _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_nwDst _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_nwProto _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_nwTos _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_tpSrc _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_tpDst _ _ _ _ _ _ _ _ _ _ _ H))
    (Wildcard.to_option (is_empty_inPort _ _ _ _ _ _ _ _ _ _ _ H))).
 Defined.

Definition inter p p' :=
  let dlSrc := Wildcard.inter Word48.eq_dec (ptrnDlSrc p) 
    (ptrnDlSrc p') in
  let dlDst := Wildcard.inter Word48.eq_dec (ptrnDlDst p) 
    (ptrnDlDst p') in
  let dlType := Wildcard.inter Word16.eq_dec (ptrnDlType p) (ptrnDlType p') in
  let dlVlan := Wildcard.inter Word16.eq_dec (ptrnDlVlan p) (ptrnDlVlan p') in
  let dlVlanPcp := Wildcard.inter Word8.eq_dec (ptrnDlVlanPcp p) 
    (ptrnDlVlanPcp p') in
   let nwSrc := Wildcard.inter Word32.eq_dec (ptrnNwSrc p) (ptrnNwSrc p') in
   let nwDst := Wildcard.inter Word32.eq_dec (ptrnNwDst p) (ptrnNwDst p') in
   let nwProto := Wildcard.inter Word8.eq_dec (ptrnNwProto p)
     (ptrnNwProto p') in
   let nwTos := Wildcard.inter Word8.eq_dec (ptrnNwTos p) (ptrnNwTos p') in
   let tpSrc := Wildcard.inter Word16.eq_dec (ptrnTpSrc p) (ptrnTpSrc p') in
   let tpDst := Wildcard.inter Word16.eq_dec (ptrnTpDst p) (ptrnTpDst p') in
   let inPort := Wildcard.inter Word16.eq_dec (ptrnInPort p) (ptrnInPort p') in
     Pattern dlSrc dlDst dlType dlVlan dlVlanPcp 
       nwSrc nwDst nwProto nwTos 
       tpSrc tpDst 
       inPort.

Definition exact_pattern (pk : packet) (pt : Word16.t) :=
  Pattern
  (WildcardExact (pktDlSrc pk))
  (WildcardExact (pktDlDst pk))
  (WildcardExact (pktDlTyp pk))
  (WildcardExact (pktDlVlan pk))
  (WildcardExact (pktDlVlanPcp pk))
  (WildcardExact (pktNwSrc pk))
  (WildcardExact (pktNwDst pk))
  (WildcardExact (pktNwProto pk))
  (WildcardExact (pktNwTos pk))
  (WildcardExact (pktTpSrc pk))
  (WildcardExact (pktTpDst pk))
  (WildcardExact pt).

Definition match_packet (pt : Word16.t) (pk : packet) pat :=
  negb (is_empty (inter (exact_pattern pk pt) pat)).

Definition is_exact pat := 
  match pat with
    | Pattern dlSrc dlDst typ vlan pcp nwSrc nwDst nwProto nwTos tpSrc tpDst
      inPort =>
      Wildcard.is_exact inPort &&
      Wildcard.is_exact dlSrc &&
      Wildcard.is_exact dlDst &&
      Wildcard.is_exact typ &&
      Wildcard.is_exact vlan &&
      Wildcard.is_exact pcp &&
      Wildcard.is_exact nwSrc &&
      Wildcard.is_exact nwDst &&
      Wildcard.is_exact nwProto &&
      Wildcard.is_exact nwTos &&
      Wildcard.is_exact tpSrc &&
      Wildcard.is_exact tpDst
  end.

(** TODO(arjun): ICMP is a little strange. Read spec to see how its fields
    are mapped. *)
Definition SupportedNwProto := 
  [ Const_0x6; 
    Const_0x7 ].

Definition SupportedDlTyp := 
  [ Const_0x800; Const_0x806 ].

  (** Based on the flow chart on Page 8 of OpenFlow 1.0 specification. In
      a ValidPattern, all exact-match fields are used to match packets. *)

Inductive ValidPattern : pattern -> Prop :=
| ValidPat_TCPUDP : forall dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst 
    nwTos tpSrc tpDst inPort nwProto,
    In nwProto SupportedNwProto ->
  ValidPattern (Pattern dlSrc dlDst (WildcardExact Const_0x800)
    dlVlan dlVlanPcp
    nwSrc nwDst (WildcardExact nwProto)
    nwTos tpSrc tpDst inPort)
| ValidPat_ARP : forall dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst 
    inPort,
  ValidPattern (Pattern dlSrc dlDst (WildcardExact Const_0x806)
    dlVlan dlVlanPcp
    nwSrc nwDst (WildcardExact Word8.zero)
    (WildcardExact Word8.zero)
    (WildcardExact Word16.zero)
    (WildcardExact Word16.zero)
    inPort)
| ValidPat_IP_generic : forall dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst
      nwTos inPort nwProto,
  ValidPattern (Pattern dlSrc dlDst (WildcardExact Const_0x800)
    dlVlan dlVlanPcp
    nwSrc nwDst nwProto
    nwTos
    (WildcardExact Word16.zero)
    (WildcardExact Word16.zero)
    inPort)
| ValidPat_generic : forall dlSrc dlDst dlVlan dlVlanPcp
    inPort frameTyp,
  ValidPattern (Pattern dlSrc dlDst (WildcardExact frameTyp)
    dlVlan dlVlanPcp
    (WildcardExact Word32.zero)
    (WildcardExact Word32.zero)
    (WildcardExact Word8.zero)
    (WildcardExact Word8.zero)
    (WildcardExact Word16.zero)
    (WildcardExact Word16.zero)
    inPort)
| ValidPat_any : forall dlSrc dlDst dlTyp dlVlan dlVlanPcp inPort,
    ValidPattern
      (Pattern dlSrc 
               dlDst 
               dlTyp
               dlVlan 
               dlVlanPcp
               WildcardAll 
               WildcardAll 
               WildcardAll
               WildcardAll 
               WildcardAll 
               WildcardAll 
               inPort)
| ValidPat_IP_any : forall dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst
      nwTos inPort nwProto,
  ValidPattern (Pattern dlSrc dlDst (WildcardExact Const_0x800)
    dlVlan dlVlanPcp
    nwSrc nwDst nwProto
    nwTos
    WildcardAll
    WildcardAll
    inPort)
| ValidPat_None : forall pat,
    is_empty pat = true ->
  ValidPattern pat.
