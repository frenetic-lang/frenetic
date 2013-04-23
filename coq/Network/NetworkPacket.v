Set Implicit Arguments.

Require Import Coq.Structures.Equalities.
Require Import NArith.BinNat.

Require Import Common.Types.
Require Import Word.WordInterface.

Local Open Scope list_scope.
Local Open Scope N_scope.

Parameter bytes : Type.

(* TODO(JNF): I wonder if we should extract this to something
else. Cstruct.t has funny equalities due to the behavior of
shift. Maybe just a string? *)
Extract Constant bytes => "Cstruct.t".

Section Constants.
  
  Definition Const_0x800 := @Word16.Mk 2048 eq_refl.
  Definition Const_0x806 := @Word16.Mk 2054 eq_refl.
  Definition Const_0x6 := @Word8.Mk 6 eq_refl.
  Definition Const_0x7 := @Word8.Mk 7 eq_refl.
  Definition Const_0x1 := @Word8.Mk 1 eq_refl.

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
  tcpChksum : Word8.t;
  tcpUrgent : Word8.t;
  tcpPayload : bytes
}.

Record icmp : Type := Icmp {
  icmpType : Word8.t;
  icmpCode : Word8.t;
  icmpChksum : Word16.t;
  icmpPayload : bytes
}.

Inductive tpPkt : Type :=
  | TpTCP : tcp -> tpPkt
  | TpICMP : icmp -> tpPkt
  | TpUnparsable : nwProto -> bytes -> tpPkt.

Record ip : Type := IP {
  pktIPVhl : Word8.t;
  pktIPTos : nwTos;
  pktIPLen : Word16.t;
  pktIPIdent : Word16.t;
  pktIPFlags : Word8.t; (* 3 bits *)
  pktIPFrag : Word16.t; (** 13 bits *)
  pktIPTtl : Word8.t;
  pktIPProto : nwProto;
  pktIPChksum : Word16.t;
  pktIPSrc :  nwAddr;
  pktIPDst : nwAddr;
  pktTpHeader : tpPkt
}.

Inductive arp : Type :=
  | ARPQuery : dlAddr -> nwAddr -> nwAddr -> arp
  | ARPReply : dlAddr -> nwAddr -> dlAddr -> nwAddr -> arp.

Inductive nw : Type :=
  | NwIP : ip -> nw
  | NwARP : arp -> nw
  | NwUnparsable : dlTyp -> bytes -> nw.

Record packet : Type := Packet {
  pktDlSrc : dlAddr;
  pktDlDst : dlAddr;
  pktDlTyp : dlTyp;
  pktDlVlan : dlVlan;
  pktDlVlanPcp : dlVlanPcp;
  pktNwHeader : nw
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
          | NwUnparsable _ _ => Word32.zero
        end
    end.

  Definition pktNwDst pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPDst ip
          | NwARP (ARPQuery _ _ ip) => ip
          | NwARP (ARPReply _ _ _ ip) => ip
          | NwUnparsable _ _ => Word32.zero
        end
    end.

  Definition pktNwProto pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPProto ip
          | NwARP (ARPQuery _ _ _) => Word8.zero
          | NwARP (ARPReply _ _ _ _) => Word8.zero
          | NwUnparsable _ _ => Word8.zero
        end
    end.

  Definition pktNwTos pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => pktIPTos ip
          | NwARP (ARPQuery _ _ _) => Word8.zero
          | NwARP (ARPReply _ _ _ _) => Word8.zero
          | NwUnparsable _ _ => Word8.zero
        end
    end.

  Definition pktTpSrc pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => 
            match pktTpHeader ip with
              | TpTCP frag => tcpSrc frag
              | TpICMP _ => Word16.zero
              | TpUnparsable _ _ => Word16.zero
            end
          | NwARP (ARPQuery _ _ _) => Word16.zero
          | NwARP (ARPReply _ _ _ _) => Word16.zero
          | NwUnparsable _ _ => Word16.zero
        end
    end.

  Definition pktTpDst pk :=
    match pk with
      | {| pktNwHeader := hdr |} => 
        match hdr with
          | NwIP ip => 
            match pktTpHeader ip with
              | TpTCP frag => tcpDst frag
              | TpICMP _ => Word16.zero
              | TpUnparsable _ _ => Word16.zero
            end
          | NwARP (ARPQuery _ _ _) => Word16.zero
          | NwARP (ARPReply _ _ _ _) => Word16.zero
          | NwUnparsable _ _ => Word16.zero
        end
    end.

End Accessors.

Section Setters.
  (** These fail silently if the field does not exist. *)

  Definition setDlSrc pk dlSrc :=
    match pk with
      | Packet _ dlDst dlTyp dlVlan dlVlanPcp nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw
    end.

  Definition setDlDst pk dlDst :=
    match pk with
      | Packet dlSrc _ dlTyp dlVlan dlVlanPcp nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw
    end.

  Definition setDlVlan pk dlVlan :=
    match pk with
      | Packet dlSrc dlDst dlTyp _ dlVlanPcp nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw
    end.

  Definition setDlVlanPcp pk dlVlanPcp :=
    match pk with
      | Packet dlSrc dlDst dlTyp dlVlan _ nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw
    end.

  Definition nw_setNwSrc (typ : dlTyp) (nwPkt : nw) src : nw :=
    match nwPkt with
      | NwIP (IP vhl tos len ident flags frag ttl proto chksum _ dst tp) =>
        NwIP (@IP vhl tos len ident flags frag ttl proto chksum src dst tp)
      | NwARP arp => NwARP arp
      | NwUnparsable typ b => NwUnparsable typ b
    end.

  Definition nw_setNwDst (typ : dlTyp)(nwPkt : nw) dst : nw :=
    match nwPkt with
      | NwIP (IP vhl tos len ident flags frag ttl proto chksum src _ tp) =>
        NwIP (@IP vhl tos len ident flags frag ttl proto chksum src dst tp)
      | NwARP arp => NwARP arp
      | NwUnparsable typ b => NwUnparsable typ b
    end.

  Definition nw_setNwTos (typ : dlTyp) (nwPkt : nw) tos :=
    match nwPkt with
      | NwIP (IP vhl _ len ident flags frag ttl proto chksum src dst tp) =>
        NwIP (@IP vhl tos len ident flags frag ttl proto chksum src dst tp)
      | NwARP arp => NwARP arp
      | NwUnparsable typ b => NwUnparsable typ b
    end.

  Definition setNwSrc pk nwSrc :=
    match pk with
      | Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp (nw_setNwSrc dlTyp nw nwSrc)
    end.

  Definition setNwDst pk nwDst :=
    match pk with
      | Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp (nw_setNwDst dlTyp nw nwDst)
    end.

  Definition setNwTos pk nwTos :=
    match pk with
      | Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw =>
        @Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp (nw_setNwTos dlTyp nw nwTos)
    end.

  Definition tp_setTpSrc tp src :=
    match tp with
      | TpTCP (Tcp _ dst seq ack off flags win chksum urgent payload) => 
        TpTCP (Tcp src dst seq ack off flags win chksum urgent payload)
      | TpICMP icmp => TpICMP icmp
      | TpUnparsable proto payload => TpUnparsable proto payload
    end.

  Definition tp_setTpDst tp dst :=
    match tp with
      | TpTCP (Tcp src _ seq ack off flags win chksum urgent payload) => 
        TpTCP (Tcp src dst seq ack off flags win chksum urgent payload)
      | TpICMP icmp => TpICMP icmp
      | TpUnparsable proto payload => TpUnparsable proto payload
    end.

  Definition nw_setTpSrc nwPkt tpSrc :=
    match nwPkt with
      | NwIP (IP vhl tos len ident flags frag ttl proto chksum src dst tp) =>
        NwIP (IP vhl tos len ident flags frag ttl proto chksum src dst 
                 (tp_setTpSrc tp tpSrc))
      | NwARP arp => NwARP arp
      | NwUnparsable typ b => NwUnparsable typ b
    end.

  Definition nw_setTpDst nwPkt tpDst :=
    match nwPkt with
      | NwIP (IP vhl tos len ident flags frag ttl proto chksum src dst tp) =>
        NwIP (IP vhl tos len ident flags frag ttl proto chksum src dst 
                 (tp_setTpDst tp tpDst))
      | NwARP arp => NwARP arp
      | NwUnparsable typ b => NwUnparsable typ b
    end.

  Definition setTpSrc pk tpSrc :=
    match pk with
      | Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp (nw_setTpSrc nw tpSrc)
    end.

  Definition setTpDst pk nwDst :=
    match pk with
      | Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp nw =>
        Packet dlSrc dlDst dlTyp dlVlan dlVlanPcp (nw_setTpDst nw nwDst)
    end.

End Setters.
