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

type dlVlan = Word16.t option

type dlVlanPcp = Word8.t

type nwAddr = Word32.t

type nwProto = Word8.t

type nwTos = Word8.t

type tpPort = Word16.t

type tcp = 
  { tcpSrc : tpPort; 
    tcpDst : tpPort; 
    tcpSeq : Word32.t;
    tcpAck : Word32.t; 
    tcpOffset : Word8.t; 
    tcpFlags : Word16.t;
    tcpWindow : Word16.t; 
    tcpChksum : Word8.t; 
    tcpUrgent : Word8.t;
    tcpPayload : bytes }

type icmp = 
  { icmpType : Word8.t; 
    icmpCode : Word8.t; 
    icmpChksum : Word16.t;
    icmpPayload : bytes }

type tpPkt =
| TpTCP of tcp
| TpICMP of icmp
| TpUnparsable of nwProto * bytes

type ip = 
  { pktIPVhl : Word8.t; 
    pktIPTos : nwTos; 
    pktIPLen : Word16.t;
    pktIPIdent : Word16.t; 
    pktIPFlags : Word8.t;
    pktIPFrag : Word16.t; 
    pktIPTtl : Word8.t; 
    pktIPProto : nwProto;
    pktIPChksum : Word16.t; 
    pktIPSrc : nwAddr; 
    pktIPDst : nwAddr;
    pktTpHeader : tpPkt }

type arp =
| ARPQuery of dlAddr * nwAddr * nwAddr
| ARPReply of dlAddr * nwAddr * dlAddr * nwAddr

type nw =
| NwIP of ip
| NwARP of arp
| NwUnparsable of dlTyp * bytes

type packet = 
  { pktDlSrc : dlAddr; 
    pktDlDst : dlAddr; 
    pktDlTyp : dlTyp;
    pktDlVlan : dlVlan; 
    pktDlVlanPcp : dlVlanPcp;
    pktNwHeader : nw }

val pktNwSrc : packet -> nwAddr

val pktNwDst : packet -> nwAddr

val pktNwTos : packet -> nwTos

val pktNwProto : packet -> nwProto

val pktTpSrc : packet -> tpPort

val pktTpDst : packet -> tpPort
    
val setDlSrc : packet -> dlAddr -> packet

val setDlDst : packet -> dlAddr -> packet

val setDlVlan : packet -> dlVlan -> packet

val setDlVlanPcp : packet -> dlVlanPcp -> packet

val setNwSrc : packet -> nwAddr -> packet

val setNwDst : packet -> nwAddr -> packet

val setNwTos : packet -> nwTos -> packet

val setTpSrc : packet -> tpPort -> packet

val setTpDst : packet -> tpPort -> packet

val portId_to_string : Word16.t -> string

val dlAddr_to_string : Word48.t -> string

val dlTyp_to_string : Word16.t -> string

val dlVlan_to_string : Word16.t option -> string

val dlVlanPcp_to_string : Word8.t -> string

val nwAddr_to_string : Word32.t -> string

val nwProto_to_string : Word8.t -> string

val nwTos_to_string : Word8.t -> string

val tpPort_to_string : Word16.t -> string

val nw_to_string : nw -> string

val packet_to_string : packet -> string

