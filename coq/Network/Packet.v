Set Implicit Arguments.

Require Import Coq.Structures.Equalities.
Require Import PArith.BinPos.

(* Require Import Common.List. *)
Require Import Common.Types.
Require Import Word.WordInterface.

Local Open Scope list_scope.
Local Open Scope positive_scope. 

Axiom bytes : Type.
Extract Constant bytes => "Cstruct.buf".

Section Constants.

  Definition Const_0x800 := Word16.Mk 2048 eq_refl.
  Definition Const_0x806 := Word16.Mk 2054 eq_refl.
  Definition Const_0x6 := Word8.Mk 6 eq_refl.
  Definition Const_0x7 := Word8.Mk 7 eq_refl.
  Definition Const_0x1 := Word8.Mk 1 eq_refl.

End Constants.

Extract Constant Const_0x800 => "0x800".
Extract Constant Const_0x806 => "0x806".
Extract Constant Const_0x6 => "0x6".
Extract Constant Const_0x7 => "0x7".
Extract Constant Const_0x1 => "0x1".

Definition portId := Word16.t.
Definition dlAddr := Word48.t.
Definition dlTyp := Word16.t.
Definition dlVlan := Word16.t.
Definition dlVlanPcp := Word8.t. (* 3 bits *)
Definition nwAddr := Word32.t.
Definition nwProto := Word8.t.
Definition nwTos := Word8.t. (** 6 bits *)
Definition tpPort := Word16.t.

Unset Elimination Schemes.

Record tcp : Type := Tcp {
  tcpSrc : tpPort;
  tcpDst : tpPort;
  tcpSeq : Word32.t;
  tcpAck : Word32.t;
  tcpOffset : Word8.t;
  tcpFlags : Word16.t; (** nine lower bits *)
  tcpWindow : Word16.t;
  tcpPayload : bytes
}.

Record icmp : Type := Icmp {
  icmpType : Word8.t;
  icmpCode : Word8.t;
  icmpChksum : Word16.t;
  icmpPayload : bytes
}.

Inductive tpPkt : nwProto -> Type :=
  | TpTCP : tcp -> tpPkt Const_0x6
  | TpICMP : icmp -> tpPkt Const_0x1
  | TpUnparsable : forall (proto : nwProto), tpPkt proto.

Record ip : Type := IP {
  pktIPIdent : Word16.t;
  pktIPFlags : Word8.t; (* 3 bits *)
  pktIPSrc :  nwAddr;
  pktIPDst : nwAddr;
  pktIPTos : nwTos;
  pktIPProto : nwProto;
  pktIPTTL : Word8.t;
  pktFrag : Word16.t; (** 13 bits *)
  pktChksum : Word16.t;
  pktTPHeader : tpPkt pktIPProto
}.

Inductive arp : Type :=
  | ARPQuery : dlAddr -> nwAddr -> nwAddr -> arp
  | ARPReply : dlAddr -> nwAddr -> dlAddr -> nwAddr -> arp.

Inductive nw : dlTyp -> Type :=
  | NwIP : ip -> nw Const_0x800
  | NwARP : arp -> nw Const_0x806
  | NwUnparsable : forall (typ : dlTyp), nw typ.

Record packet : Type := Packet {
  pktDlSrc : dlAddr;
  pktDlDst : dlAddr;
  pktDlTyp : dlTyp;
  pktDlVlan : dlVlan;
  pktDlVlanPcp : dlVlanPcp;
  pktNwHeader : nw pktDlTyp
}.

Section Accessors.
  (** These accessors return zero if a field does not exist. *)

  Definition pktNwSrc pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPSrc ip
          | NwARP (ARPQuery _ ip _) => ip
          | NwARP (ARPReply _ ip _ _) => ip
          | NwUnparsable _ => Word32.zero
        end
    end.

  Definition pktNwDst pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPDst ip
          | NwARP (ARPQuery _ _ ip) => ip
          | NwARP (ARPReply _ _ _ ip) => ip
          | NwUnparsable _ => Word32.zero
        end
    end.

  Definition pktNwProto pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPProto ip
          | NwARP (ARPQuery _ _ _) => Word8.zero
          | NwARP (ARPReply _ _ _ _) => Word8.zero
          | NwUnparsable _ => Word8.zero
        end
    end.

  Definition pktNwTos pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPTos ip
          | NwARP (ARPQuery _ _ _) => Word8.zero
          | NwARP (ARPReply _ _ _ _) => Word8.zero
          | NwUnparsable _ => Word8.zero
        end
    end.

  Definition pktTpSrc pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => 
            match pktTPHeader ip with
              | TpTCP frag => tcpSrc frag
              | TpICMP _ => Word16.zero
              | TpUnparsable _ => Word16.zero
            end
          | NwARP (ARPQuery _ _ _) => Word16.zero
          | NwARP (ARPReply _ _ _ _) => Word16.zero
          | NwUnparsable _ => Word16.zero
        end
    end.

  Definition pktTpDst pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => 
            match pktTPHeader ip with
              | TpTCP frag => tcpDst frag
              | TpICMP _ => Word16.zero
              | TpUnparsable _ => Word16.zero
            end
          | NwARP (ARPQuery _ _ _) => Word16.zero
          | NwARP (ARPReply _ _ _ _) => Word16.zero
          | NwUnparsable _ => Word16.zero
        end
    end.

End Accessors.