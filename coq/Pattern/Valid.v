Set Implicit Arguments.

Require Import Coq.Arith.EqNat.
Require Import NPeano.
Require Import Arith.Peano_dec.
Require Import Bool.Bool.
Require Import Coq.Classes.Equivalence.
Require Import Lists.List.
Require Import PArith.BinPos.

Require Import Word.WordInterface.
Require Import Network.NetworkPacket.
Require Import Common.Types.
Require Import Pattern.Pattern.
Require Import Wildcard.Wildcard.
Require Import Wildcard.Theory.

Open Scope bool_scope.
Open Scope list_scope.
Open Scope equiv_scope.
Open Scope positive_scope.


Definition SupportedNwProto := 
  [ Const_0x6; 
    Const_0x7;
    Const_0x1 ].

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
                            nwSrc nwDst WildcardAll
                            WildcardAll WildcardAll WildcardAll inPort)
| ValidPat_IP_other : forall dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst
                             nwTos inPort nwProto,
    ~ In nwProto SupportedNwProto ->
    ValidPattern (Pattern dlSrc dlDst (WildcardExact Const_0x800)
                            dlVlan dlVlanPcp
                            nwSrc nwDst (WildcardExact nwProto)
                            nwTos WildcardAll WildcardAll inPort)
| ValidPat_IP_NoNwProto : forall dlSrc dlDst dlVlan dlVlanPcp nwSrc nwDst
                              nwTos inPort,
    ValidPattern (Pattern dlSrc dlDst (WildcardExact Const_0x800)
                            dlVlan dlVlanPcp
                            nwSrc nwDst WildcardAll
                            nwTos WildcardAll WildcardAll inPort)
| ValidPat_OtherFrameTyp : forall dlSrc dlDst dlVlan dlVlanPcp
                                  inPort frameTyp,
    ~ In frameTyp SupportedDlTyp ->
    ValidPattern (Pattern dlSrc dlDst (WildcardExact frameTyp)
                            dlVlan dlVlanPcp
                            WildcardAll WildcardAll WildcardAll
                            WildcardAll WildcardAll WildcardAll inPort)
| ValidPat_AnyFrameTyp : forall dlSrc dlDst dlVlan dlVlanPcp
                                  inPort,
    ValidPattern (Pattern dlSrc dlDst WildcardAll
                            dlVlan dlVlanPcp
                            WildcardAll WildcardAll WildcardAll
                            WildcardAll WildcardAll WildcardAll inPort)
| ValidPat_None : forall pat,
  Pattern.is_empty pat = true ->
  ValidPattern pat.
  
