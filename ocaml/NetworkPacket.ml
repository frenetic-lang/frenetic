open Word

type bytes = Cstruct.t

(** val coq_Const_0x800 : int **)

let coq_Const_0x800 = 0x800

(** val coq_Const_0x806 : int **)

let coq_Const_0x806 = 0x806

(** val coq_Const_0x6 : int **)

let coq_Const_0x6 = 0x6

(** val coq_Const_0x7 : int **)

let coq_Const_0x7 = 0x7

(** val coq_Const_0x1 : int **)

let coq_Const_0x1 = 0x1

type portId = Word16.t

type dlAddr = Word48.t

type dlTyp = Word16.t

type dlVlan = Word16.t

type dlVlanPcp = Word8.t

type nwAddr = Word32.t

type nwProto = Word8.t

type nwTos = Word8.t

type tpPort = Word16.t

type tcp = { tcpSrc : tpPort; tcpDst : tpPort; tcpSeq : Word32.t;
             tcpAck : Word32.t; tcpOffset : Word8.t; tcpFlags : Word16.t;
             tcpWindow : Word16.t; tcpChksum : Word8.t; tcpUrgent : Word8.t;
             tcpPayload : bytes }

(** val tcpSrc : tcp -> tpPort **)

let tcpSrc x = x.tcpSrc

(** val tcpDst : tcp -> tpPort **)

let tcpDst x = x.tcpDst

(** val tcpSeq : tcp -> Word32.t **)

let tcpSeq x = x.tcpSeq

(** val tcpAck : tcp -> Word32.t **)

let tcpAck x = x.tcpAck

(** val tcpOffset : tcp -> Word8.t **)

let tcpOffset x = x.tcpOffset

(** val tcpFlags : tcp -> Word16.t **)

let tcpFlags x = x.tcpFlags

(** val tcpWindow : tcp -> Word16.t **)

let tcpWindow x = x.tcpWindow

(** val tcpChksum : tcp -> Word8.t **)

let tcpChksum x = x.tcpChksum

(** val tcpUrgent : tcp -> Word8.t **)

let tcpUrgent x = x.tcpUrgent

(** val tcpPayload : tcp -> bytes **)

let tcpPayload x = x.tcpPayload

type icmp = { icmpType : Word8.t; icmpCode : Word8.t; icmpChksum : Word16.t;
              icmpPayload : bytes }

(** val icmpType : icmp -> Word8.t **)

let icmpType x = x.icmpType

(** val icmpCode : icmp -> Word8.t **)

let icmpCode x = x.icmpCode

(** val icmpChksum : icmp -> Word16.t **)

let icmpChksum x = x.icmpChksum

(** val icmpPayload : icmp -> bytes **)

let icmpPayload x = x.icmpPayload

type tpPkt =
| TpTCP of tcp
| TpICMP of icmp
| TpUnparsable of nwProto * bytes

type ip = { pktIPVhl : Word8.t; pktIPTos : nwTos; pktIPLen : Word16.t;
            pktIPIdent : Word16.t; pktIPFlags : Word8.t;
            pktIPFrag : Word16.t; pktIPTtl : Word8.t; pktIPProto : nwProto;
            pktIPChksum : Word16.t; pktIPSrc : nwAddr; pktIPDst : nwAddr;
            pktTpHeader : tpPkt }

(** val pktIPVhl : ip -> Word8.t **)

let pktIPVhl x = x.pktIPVhl

(** val pktIPTos : ip -> nwTos **)

let pktIPTos x = x.pktIPTos

(** val pktIPLen : ip -> Word16.t **)

let pktIPLen x = x.pktIPLen

(** val pktIPIdent : ip -> Word16.t **)

let pktIPIdent x = x.pktIPIdent

(** val pktIPFlags : ip -> Word8.t **)

let pktIPFlags x = x.pktIPFlags

(** val pktIPFrag : ip -> Word16.t **)

let pktIPFrag x = x.pktIPFrag

(** val pktIPTtl : ip -> Word8.t **)

let pktIPTtl x = x.pktIPTtl

(** val pktIPProto : ip -> nwProto **)

let pktIPProto x = x.pktIPProto

(** val pktIPChksum : ip -> Word16.t **)

let pktIPChksum x = x.pktIPChksum

(** val pktIPSrc : ip -> nwAddr **)

let pktIPSrc x = x.pktIPSrc

(** val pktIPDst : ip -> nwAddr **)

let pktIPDst x = x.pktIPDst

(** val pktTpHeader : ip -> tpPkt **)

let pktTpHeader x = x.pktTpHeader

type arp =
| ARPQuery of dlAddr * nwAddr * nwAddr
| ARPReply of dlAddr * nwAddr * dlAddr * nwAddr

type nw =
| NwIP of ip
| NwARP of arp
| NwUnparsable of dlTyp * bytes

type packet = { pktDlSrc : dlAddr; pktDlDst : dlAddr; pktDlTyp : dlTyp;
                pktDlVlan : dlVlan; pktDlVlanPcp : dlVlanPcp;
                pktNwHeader : nw }

(** val pktDlSrc : packet -> dlAddr **)

let pktDlSrc x = x.pktDlSrc

(** val pktDlDst : packet -> dlAddr **)

let pktDlDst x = x.pktDlDst

(** val pktDlTyp : packet -> dlTyp **)

let pktDlTyp x = x.pktDlTyp

(** val pktDlVlan : packet -> dlVlan **)

let pktDlVlan x = x.pktDlVlan

(** val pktDlVlanPcp : packet -> dlVlanPcp **)

let pktDlVlanPcp x = x.pktDlVlanPcp

(** val pktNwHeader : packet -> nw **)

let pktNwHeader x = x.pktNwHeader

(** val pktNwSrc : packet -> nwAddr **)

let pktNwSrc pk =
  let { pktDlSrc = pktDlSrc0; pktDlDst = pktDlDst0; pktDlTyp = pktDlTyp0;
    pktDlVlan = pktDlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader =
    hdr } = pk
  in
  (match hdr with
   | NwIP ip0 -> ip0.pktIPSrc
   | NwARP a ->
     (match a with
      | ARPQuery (d, ip0, n) -> ip0
      | ARPReply (d, ip0, d0, n) -> ip0)
   | NwUnparsable (d, b) -> Word32.zero)

(** val pktNwDst : packet -> nwAddr **)

let pktNwDst pk =
  let { pktDlSrc = pktDlSrc0; pktDlDst = pktDlDst0; pktDlTyp = pktDlTyp0;
    pktDlVlan = pktDlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader =
    hdr } = pk
  in
  (match hdr with
   | NwIP ip0 -> ip0.pktIPDst
   | NwARP a ->
     (match a with
      | ARPQuery (d, n, ip0) -> ip0
      | ARPReply (d, n, d0, ip0) -> ip0)
   | NwUnparsable (d, b) -> Word32.zero)

(** val pktNwProto : packet -> nwProto **)

let pktNwProto pk =
  let { pktDlSrc = pktDlSrc0; pktDlDst = pktDlDst0; pktDlTyp = pktDlTyp0;
    pktDlVlan = pktDlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader =
    hdr } = pk
  in
  (match hdr with
   | NwIP ip0 -> ip0.pktIPProto
   | _ -> Word8.zero)

(** val pktNwTos : packet -> nwTos **)

let pktNwTos pk =
  let { pktDlSrc = pktDlSrc0; pktDlDst = pktDlDst0; pktDlTyp = pktDlTyp0;
    pktDlVlan = pktDlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader =
    hdr } = pk
  in
  (match hdr with
   | NwIP ip0 -> ip0.pktIPTos
   | _ -> Word8.zero)

(** val pktTpSrc : packet -> tpPort **)

let pktTpSrc pk =
  let { pktDlSrc = pktDlSrc0; pktDlDst = pktDlDst0; pktDlTyp = pktDlTyp0;
    pktDlVlan = pktDlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader =
    hdr } = pk
  in
  (match hdr with
   | NwIP ip0 ->
     (match ip0.pktTpHeader with
      | TpTCP frag -> frag.tcpSrc
      | _ -> Word16.zero)
   | _ -> Word16.zero)

(** val pktTpDst : packet -> tpPort **)

let pktTpDst pk =
  let { pktDlSrc = pktDlSrc0; pktDlDst = pktDlDst0; pktDlTyp = pktDlTyp0;
    pktDlVlan = pktDlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader =
    hdr } = pk
  in
  (match hdr with
   | NwIP ip0 ->
     (match ip0.pktTpHeader with
      | TpTCP frag -> frag.tcpDst
      | _ -> Word16.zero)
   | _ -> Word16.zero)

(** val setDlSrc : packet -> dlAddr -> packet **)

let setDlSrc pk dlSrc =
  let { pktDlSrc = pktDlSrc0; pktDlDst = dlDst; pktDlTyp = dlTyp0;
    pktDlVlan = dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 }

(** val setDlDst : packet -> dlAddr -> packet **)

let setDlDst pk dlDst =
  let { pktDlSrc = dlSrc; pktDlDst = pktDlDst0; pktDlTyp = dlTyp0;
    pktDlVlan = dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 }

(** val setDlVlan : packet -> dlVlan -> packet **)

let setDlVlan pk dlVlan0 =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    pktDlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 }

(** val setDlVlanPcp : packet -> dlVlanPcp -> packet **)

let setDlVlanPcp pk dlVlanPcp0 =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    dlVlan0; pktDlVlanPcp = pktDlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 }

(** val nw_setNwSrc : dlTyp -> nw -> nwAddr -> nw **)

let nw_setNwSrc typ nwPkt src =
  match nwPkt with
  | NwIP i ->
    let { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent = ident;
      pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
      proto; pktIPChksum = chksum; pktIPSrc = pktIPSrc0; pktIPDst = dst;
      pktTpHeader = tp } = i
    in
    NwIP { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent =
    ident; pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
    proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
    pktTpHeader = tp }
  | x -> x

(** val nw_setNwDst : dlTyp -> nw -> nwAddr -> nw **)

let nw_setNwDst typ nwPkt dst =
  match nwPkt with
  | NwIP i ->
    let { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent = ident;
      pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
      proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = pktIPDst0;
      pktTpHeader = tp } = i
    in
    NwIP { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent =
    ident; pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
    proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
    pktTpHeader = tp }
  | x -> x

(** val nw_setNwTos : dlTyp -> nw -> nwTos -> nw **)

let nw_setNwTos typ nwPkt tos =
  match nwPkt with
  | NwIP i ->
    let { pktIPVhl = vhl; pktIPTos = pktIPTos0; pktIPLen = len; pktIPIdent =
      ident; pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl;
      pktIPProto = proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst =
      dst; pktTpHeader = tp } = i
    in
    NwIP { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent =
    ident; pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
    proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
    pktTpHeader = tp }
  | x -> x

(** val setNwSrc : packet -> nwAddr -> packet **)

let setNwSrc pk nwSrc =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader =
  (nw_setNwSrc dlTyp0 nw0 nwSrc) }

(** val setNwDst : packet -> nwAddr -> packet **)

let setNwDst pk nwDst =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader =
  (nw_setNwDst dlTyp0 nw0 nwDst) }

(** val setNwTos : packet -> nwTos -> packet **)

let setNwTos pk nwTos0 =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader =
  (nw_setNwTos dlTyp0 nw0 nwTos0) }

(** val tp_setTpSrc : tpPkt -> tpPort -> tpPkt **)

let tp_setTpSrc tp src =
  match tp with
  | TpTCP t0 ->
    let { tcpSrc = tcpSrc0; tcpDst = dst; tcpSeq = seq; tcpAck = ack;
      tcpOffset = off; tcpFlags = flags; tcpWindow = win; tcpChksum = chksum;
      tcpUrgent = urgent; tcpPayload = payload } = t0
    in
    TpTCP { tcpSrc = src; tcpDst = dst; tcpSeq = seq; tcpAck = ack;
    tcpOffset = off; tcpFlags = flags; tcpWindow = win; tcpChksum = chksum;
    tcpUrgent = urgent; tcpPayload = payload }
  | x -> x

(** val tp_setTpDst : tpPkt -> tpPort -> tpPkt **)

let tp_setTpDst tp dst =
  match tp with
  | TpTCP t0 ->
    let { tcpSrc = src; tcpDst = tcpDst0; tcpSeq = seq; tcpAck = ack;
      tcpOffset = off; tcpFlags = flags; tcpWindow = win; tcpChksum = chksum;
      tcpUrgent = urgent; tcpPayload = payload } = t0
    in
    TpTCP { tcpSrc = src; tcpDst = dst; tcpSeq = seq; tcpAck = ack;
    tcpOffset = off; tcpFlags = flags; tcpWindow = win; tcpChksum = chksum;
    tcpUrgent = urgent; tcpPayload = payload }
  | x -> x

(** val nw_setTpSrc : nw -> tpPort -> nw **)

let nw_setTpSrc nwPkt tpSrc =
  match nwPkt with
  | NwIP i ->
    let { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent = ident;
      pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
      proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
      pktTpHeader = tp } = i
    in
    NwIP { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent =
    ident; pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
    proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
    pktTpHeader = (tp_setTpSrc tp tpSrc) }
  | x -> x

(** val nw_setTpDst : nw -> tpPort -> nw **)

let nw_setTpDst nwPkt tpDst =
  match nwPkt with
  | NwIP i ->
    let { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent = ident;
      pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
      proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
      pktTpHeader = tp } = i
    in
    NwIP { pktIPVhl = vhl; pktIPTos = tos; pktIPLen = len; pktIPIdent =
    ident; pktIPFlags = flags; pktIPFrag = frag; pktIPTtl = ttl; pktIPProto =
    proto; pktIPChksum = chksum; pktIPSrc = src; pktIPDst = dst;
    pktTpHeader = (tp_setTpDst tp tpDst) }
  | x -> x

(** val setTpSrc : packet -> tpPort -> packet **)

let setTpSrc pk tpSrc =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = (nw_setTpSrc nw0 tpSrc) }

(** val setTpDst : packet -> tpPort -> packet **)

let setTpDst pk nwDst =
  let { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
    dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = nw0 } = pk
  in
  { pktDlSrc = dlSrc; pktDlDst = dlDst; pktDlTyp = dlTyp0; pktDlVlan =
  dlVlan0; pktDlVlanPcp = dlVlanPcp0; pktNwHeader = (nw_setTpDst nw0 nwDst) }

let portId_to_string = Word16.to_string

let dlAddr_to_string = Word48.to_string

let dlTyp_to_string = Word16.to_string

let dlVlan_to_string = Word16.to_string

let dlVlanPcp_to_string = Word8.to_string

let nwAddr_to_string = Word32.to_string

let nwProto_to_string = Word8.to_string

let nwTos_to_string = Word8.to_string

let tpPort_to_string = Word16.to_string

