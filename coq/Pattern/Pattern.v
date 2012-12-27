(** WARNING(arjun): Does not compile in Proof General. Proof General freezes
    on Pattern.inter. coqc does work.
 *)
Set Implicit Arguments.

Require Import Coq.Arith.EqNat.
Require Import NPeano.
Require Import Arith.Peano_dec.
Require Import Bool.Bool.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.

Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Network.Packet.
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

Module Pattern.

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
    Pattern WildcardAll WildcardAll WildcardAll WildcardAll WildcardAll 
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

Require Import OpenFlow.MessagesDef.

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

(** Extracts to shockingly good code. *)
Lemma to_match : forall pat (H : is_empty pat = false),
                   of_match. 
Proof.
  intros.
  destruct pat.
  unfold is_empty in H.
  repeat rewrite -> orb_false_iff in H.
  do 11 (destruct H).
  exact (Match
           (Wildcard.to_option (is_empty_neq_None H10))
           (Wildcard.to_option (is_empty_neq_None H9))
           (Wildcard.to_option (is_empty_neq_None H8))
           (Wildcard.to_option (is_empty_neq_None H6))
           (Wildcard.to_option (is_empty_neq_None H7))
           (Wildcard.to_option (is_empty_neq_None H5))
           (Wildcard.to_option (is_empty_neq_None H4))
           (Wildcard.to_option (is_empty_neq_None H3))
           (Wildcard.to_option (is_empty_neq_None H2))
           (Wildcard.to_option (is_empty_neq_None H1))
           (Wildcard.to_option (is_empty_neq_None H0))
           (Wildcard.to_option (is_empty_neq_None H))).
Defined.

End Pattern.