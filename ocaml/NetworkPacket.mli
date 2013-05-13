open Word

type bytes = Cstruct.t

val coq_Const_0x800 : int

val coq_Const_0x806 : int

val coq_Const_0x6 : int

val coq_Const_0x7 : int

val coq_Const_0x1 : int

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

val tcpSrc : tcp -> tpPort

val tcpDst : tcp -> tpPort

val tcpSeq : tcp -> Word32.t

val tcpAck : tcp -> Word32.t

val tcpOffset : tcp -> Word8.t

val tcpFlags : tcp -> Word16.t

val tcpWindow : tcp -> Word16.t

val tcpChksum : tcp -> Word8.t

val tcpUrgent : tcp -> Word8.t

val tcpPayload : tcp -> bytes

type icmp = { icmpType : Word8.t; icmpCode : Word8.t; icmpChksum : Word16.t;
              icmpPayload : bytes }

val icmpType : icmp -> Word8.t

val icmpCode : icmp -> Word8.t

val icmpChksum : icmp -> Word16.t

val icmpPayload : icmp -> bytes

type tpPkt =
| TpTCP of tcp
| TpICMP of icmp
| TpUnparsable of nwProto * bytes

type ip = { pktIPVhl : Word8.t; pktIPTos : nwTos; pktIPLen : Word16.t;
            pktIPIdent : Word16.t; pktIPFlags : Word8.t;
            pktIPFrag : Word16.t; pktIPTtl : Word8.t; pktIPProto : nwProto;
            pktIPChksum : Word16.t; pktIPSrc : nwAddr; pktIPDst : nwAddr;
            pktTpHeader : tpPkt }

val pktIPVhl : ip -> Word8.t

val pktIPTos : ip -> nwTos

val pktIPLen : ip -> Word16.t

val pktIPIdent : ip -> Word16.t

val pktIPFlags : ip -> Word8.t

val pktIPFrag : ip -> Word16.t

val pktIPTtl : ip -> Word8.t

val pktIPProto : ip -> nwProto

val pktIPChksum : ip -> Word16.t

val pktIPSrc : ip -> nwAddr

val pktIPDst : ip -> nwAddr

val pktTpHeader : ip -> tpPkt

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

val pktDlSrc : packet -> dlAddr

val pktDlDst : packet -> dlAddr

val pktDlTyp : packet -> dlTyp

val pktDlVlan : packet -> dlVlan

val pktDlVlanPcp : packet -> dlVlanPcp

val pktNwHeader : packet -> nw

val pktNwSrc : packet -> nwAddr

val pktNwDst : packet -> nwAddr

val pktNwProto : packet -> nwProto

val pktNwTos : packet -> nwTos

val pktTpSrc : packet -> tpPort

val pktTpDst : packet -> tpPort

val setDlSrc : packet -> dlAddr -> packet

val setDlDst : packet -> dlAddr -> packet

val setDlVlan : packet -> dlVlan -> packet

val setDlVlanPcp : packet -> dlVlanPcp -> packet

val nw_setNwSrc : dlTyp -> nw -> nwAddr -> nw

val nw_setNwDst : dlTyp -> nw -> nwAddr -> nw

val nw_setNwTos : dlTyp -> nw -> nwTos -> nw

val setNwSrc : packet -> nwAddr -> packet

val setNwDst : packet -> nwAddr -> packet

val setNwTos : packet -> nwTos -> packet

val tp_setTpSrc : tpPkt -> tpPort -> tpPkt

val tp_setTpDst : tpPkt -> tpPort -> tpPkt

val nw_setTpSrc : nw -> tpPort -> nw

val nw_setTpDst : nw -> tpPort -> nw

val setTpSrc : packet -> tpPort -> packet

val setTpDst : packet -> tpPort -> packet

