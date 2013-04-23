Set Implicit Arguments.

Require Import Coq.Structures.Equalities.
Require Import NArith.BinNat.
Require Import Bag.TotalOrder.
Require Import Word.WordTheory.
Require Import Network.NetworkPacket.

Local Open Scope list_scope.
Local Open Scope N_scope.

(* "TODO(arjun): br more organized about byte-buffer axioms?". *)
Parameter bytes_le : bytes -> bytes -> Prop.
Parameter Instance TotalOrder_bytes : TotalOrder bytes_le.

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
  Word16.TotalOrder Word32.TotalOrder Word8.TotalOrder Word48.TotalOrder.

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

Definition proj_tpPkt r :=
  match r with
    | TpTCP tcp => inl (proj_tcp tcp)
    | TpICMP icmp => inr (inl (proj_icmp icmp))
    | TpUnparsable proto bytes => inr (inr (proto,bytes))
  end.

Definition inj_tpPkt tup := 
  match tup with
    | inl tcp => TpTCP (inj_tcp tcp)
    | inr (inl icmp) => TpICMP (inj_icmp icmp)
    | inr (inr (proto,bytes)) => TpUnparsable proto bytes
  end.

Definition tpPkt_le := tcp_le +++ icmp_le +++ (Word8.le ** bytes_le).
  
Instance TotalOrder_tpPkt : TotalOrder (ProjectOrdering proj_tpPkt tpPkt_le).
Proof.
  apply TotalOrder_Project with (g:=inj_tpPkt).
  + unfold tpPkt_le. unfold tcp_le. unfold icmp_le.
    auto 20.
  + unfold inverse. 
    destruct x; auto.
    destruct t; auto.
    destruct i; auto.
Qed.

Definition proj_ip r :=
  match r with
    | IP vhl tos len ident flags frag ttl proto chksum src dst tp =>
      (vhl, tos, len, ident, flags, frag, ttl, proto, chksum, src, dst, proj_tpPkt tp)
  end.

Definition inj_ip tup :=
  match tup with
    | (vhl, tos, len, ident, flags, frag, ttl, proto, chksum, src, dst, tp) =>
      IP vhl tos len ident flags frag ttl proto chksum src dst (inj_tpPkt tp)
  end.

Definition ip_le :=
  Word8.le ** Word8.le ** Word16.le ** Word16.le ** Word8.le ** Word16.le ** Word8.le ** Word8.le **
  Word16.le ** Word32.le ** Word32.le ** tpPkt_le.

Lemma inverse_ip : inverse proj_ip inj_ip.
Proof with auto.
  unfold inverse.
    destruct x; auto.
    simpl.
    destruct pktTpHeader; auto. (* TODO(arjun): repeating this proof unnecessarily *)
    destruct t; auto.
    destruct i; auto.
Qed.

Instance TotalOrder_ip : TotalOrder (ProjectOrdering proj_ip ip_le).
Proof.
  apply TotalOrder_Project with (g:=inj_ip).
  + unfold ip_le. unfold tpPkt_le. unfold tcp_le. unfold icmp_le.
    auto 20.
  + exact inverse_ip.
Qed.

Definition proj_arp x :=
  match x with
    | ARPQuery x y z => inl (x,y,z)
    | ARPReply w x y z => inr (w,x,y,z)
  end.

Definition inj_arp x :=
  match x with
    | inl (x,y,z) => ARPQuery x y z
    | inr (w,x,y,z) => ARPReply w x y z
  end.

Definition arp_le := (Word48.le ** Word32.le ** Word32.le) +++  (Word48.le ** Word32.le ** Word48.le ** Word32.le).

Lemma inverse_arp : inverse proj_arp inj_arp.
Proof.  unfold inverse. destruct x; auto. Qed.

Instance TotalOrder_arp : TotalOrder (ProjectOrdering proj_arp arp_le).
Proof.
  apply TotalOrder_Project with (g:=inj_arp).
  + unfold arp_le. auto 20.
  + exact inverse_arp.
Qed.

Definition proj_nw x :=
  match x with
    | NwIP ip => inl ip
    | NwARP arp => inr (inl arp)
    | NwUnparsable typ bytes => inr (inr (typ, bytes))
  end.

Definition inj_nw x :=
  match x with
    | inl ip => NwIP ip 
    | inr (inl arp) => NwARP arp
    | inr (inr (typ, bytes)) => NwUnparsable typ bytes
  end.

Definition nw_le := ProjectOrdering proj_ip ip_le +++ (ProjectOrdering proj_arp arp_le +++ (Word16.le ** bytes_le)).

Definition inverse_nw : inverse proj_nw inj_nw.
Proof. unfold inverse. destruct x; auto. Qed.

Instance TotalOrder_nw : TotalOrder (ProjectOrdering proj_nw nw_le).
Proof.
  apply TotalOrder_Project with (g:=inj_nw).
  + unfold nw_le. repeat apply TotalOrder_sum. apply TotalOrder_ip. apply TotalOrder_arp. auto.
  + exact inverse_nw.
Qed.

Definition proj_packet x :=
  match x with
    | Packet src dst typ vlan pcp nw => 
      (src,dst,typ,vlan,pcp,nw)
  end.

Definition inj_packet x :=
  match x with
    | (src,dst,typ,vlan,pcp,nw) =>
      Packet src dst typ vlan pcp nw
  end.

Lemma inverse_packet : inverse proj_packet inj_packet.
Proof. unfold inverse. destruct x; auto. Qed.

Definition packet_le :=  ProjectOrdering proj_packet (Word48.le ** Word48.le ** Word16.le ** Word16.le ** Word8.le ** (ProjectOrdering proj_nw nw_le)).

Instance TotalOrder_packet : TotalOrder packet_le.
Proof.
  apply TotalOrder_Project with (g:=inj_packet).
  + unfold packet_le.  repeat apply TotalOrder_pair; auto. apply TotalOrder_nw.
  + exact inverse_packet.
Qed.
