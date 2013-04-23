module Types : sig

  open WordInterface

  type bytes = Cstruct.t

  type portId = Word16.t
  type dlAddr = Word48.t
  type dlTyp = Word16.t
  type dlVlan = Word16.t
  type dlVlanPcp = Word8.t
  type nwAddr = Word32.t
  type nwProto = Word8.t
  type nwTos = Word8.t
  type tpPort = Word16.t

  type tcp = { 
    tcpSrc : tpPort; 
    tcpDst : tpPort;
    tcpSeq : Word32.t;
    tcpAck : Word32.t; 
    tcpOffset : Word8.t;
    tcpFlags : Word16.t;
    tcpWindow : Word16.t; 
    tcpChksum : Word8.t;
    tcpUrgent : Word8.t;
    tcpPayload : bytes 
  }

  type icmp = { 
    icmpType : Word8.t; 
    icmpCode : Word8.t;
    icmpChksum : Word16.t;
    icmpPayload : bytes }

  type tpPkt =
    | TpTCP of tcp
    | TpICMP of icmp
    | TpUnparsable of nwProto * bytes

  type ip = { 
    pktIPVhl : Word8.t;
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
    pktTpHeader : tpPkt 
  }

  type arp =
    | ARPQuery of dlAddr * nwAddr * nwAddr
    | ARPReply of dlAddr * nwAddr * dlAddr * nwAddr

  type nw =
    | NwIP of ip
    | NwARP of arp
    | NwUnparsable of dlTyp * bytes

  type packet = { 
    pktDlSrc : dlAddr; 
    pktDlDst : dlAddr; 
    pktDlTyp : dlTyp;
    pktDlVlan : dlVlan;
    pktDlVlanPcp : dlVlanPcp;
    pktNwHeader : nw 
  }
end
  with type packet = NetworkPacket.packet

module Parser : sig

  open Types

  val parse_packet : Cstruct.t -> packet option
  val serialize_packet : packet -> Cstruct.t
  val sizeof_packet : packet -> int
  val string_of_eth : packet -> string

end
