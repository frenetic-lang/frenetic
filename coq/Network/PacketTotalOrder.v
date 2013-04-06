Set Implicit Arguments.

Require Import Coq.Structures.Equalities.
Require Import NArith.BinNat.
Require Import Bag.TotalOrder.
Require Import Word.WordTheory.
Require Import Network.Packet.

Local Open Scope list_scope.
Local Open Scope N_scope.

Axiom bytes_le : bytes -> bytes -> Prop.
Axiom Instance TotalOrder_bytes : TotalOrder bytes_le.

Definition proj_tcp r := 
  match r with
    | Tcp src dst seq ack off flags win chk urg payload =>
      (src,dst,seq,ack,off,flags,win,chk,urg,payload)
  end.

Definition inj_tcp tup := 
  match tup with
    | (src,dst,seq,ack,off,flags,win,chk,urg,payload) =>
      Tcp src dst seq ack off flags win chk urg payload
  end.

Local Notation "x ** y" := (PairOrdering x y) (at level 71, left associativity).
Local Notation "x +++ y" := (SumOrdering x y) (at level 70, right associativity).

Definition tcp_le :=
  (Word16.le ** Word16.le ** Word32.le ** Word32.le ** Word8.le ** 
     Word16.le ** Word16.le ** Word8.le ** Word8.le **  bytes_le).

Hint Resolve TotalOrder_sum TotalOrder_pair TotalOrder_bytes 
  Word16.TotalOrder Word32.TotalOrder Word8.TotalOrder.

Instance TotalOrder_tcp : TotalOrder (ProjectOrdering proj_tcp tcp_le).
Proof.
  apply TotalOrder_Project with (g:=inj_tcp).
  + unfold tcp_le. auto 20.
  + unfold inverse. destruct x; auto.
Qed.

Definition proj_icmp r :=
  match r with
    | Icmp typ code chksum payload => (typ, code, chksum, payload)
  end.

Definition inj_icmp tup :=
  match tup with
    | (typ, code, chksum, payload) => Icmp typ code chksum payload
  end.

Definition icmp_le := Word8.le ** Word8.le ** Word16.le ** bytes_le.

Instance TotalOrder_icmp : TotalOrder (ProjectOrdering proj_icmp icmp_le).
Proof.
  apply TotalOrder_Project with (g:=inj_icmp).
  + unfold icmp_le. auto 20.
  + unfold inverse. destruct x; auto.
Qed.

Definition proj_tpPkt (proto : nwProto) (r : tpPkt proto) := 
  match r with
    | TpTCP tcp => inl (proj_tcp tcp)
    | TpICMP icmp => inr (inl (proj_icmp icmp))
    | TpUnparsable proto bytes => inr (inr (proto,bytes))
  end.

Definition tcpTup :=
  (Word16.Word.t * Word16.Word.t * Word32.Word.t * Word32.Word.t * Word8.Word.t * 
   Word16.Word.t * Word16.Word.t * Word8.Word.t * Word8.Word.t *  bytes) %type.

Definition icmpTup :=
  (Word8.Word.t * Word8.Word.t * Word16.Word.t * bytes) %type.

Definition inj_tpPkt tup := 
  match tup in sum _ _ return
        match tup with 
          | inl _ => tpPkt Const_0x6 
          | inr (inl _) => tpPkt Const_0x1
          | inr (inr (proto, _)) => tpPkt proto
        end with
    | inl tcp => TpTCP (inj_tcp tcp)
    | inr (inl icmp) => TpICMP (inj_icmp icmp)
    | inr (inr (proto,bytes)) => TpUnparsable proto bytes
  end.

Definition tpPkt_le := tcp_le +++ icmp_le +++ (Word8.le ** bytes_le).
  

(* This is getting stupid. Why are these dependent types? *)
Instance TotalOrder_tpPkt `(proto : Word8.Word.t) : TotalOrder (ProjectOrdering (@proj_tpPkt proto) tpPkt_le).
Proof.
Abort.

(*
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
  pktTpHeader : tpPkt pktIPProto
}.

Inductive arp : Type :=
  | ARPQuery : dlAddr -> nwAddr -> nwAddr -> arp
  | ARPReply : dlAddr -> nwAddr -> dlAddr -> nwAddr -> arp.

Inductive nw : dlTyp -> Type :=
  | NwIP : ip -> nw Const_0x800
  | NwARP : arp -> nw Const_0x806
  | NwUnparsable : forall (typ : dlTyp), bytes -> nw typ.

Record packet : Type := Packet {
  pktDlSrc : dlAddr;
  pktDlDst : dlAddr;
  pktDlTyp : dlTyp;
  pktDlVlan : dlVlan;
  pktDlVlanPcp : dlVlanPcp;
  pktNwHeader : nw pktDlTyp
}.
*)

