module Types : sig

  open WordInterface

  type bytes = Cstruct.t

  type portId = int
  type dlAddr = Int64.t
  type dlTyp = int
  type dlVlan = int
  type dlVlanPcp = int
  type nwAddr = Int32.t
  type nwProto = int
  type nwTos = int
  type tpPort = int

  type tcp = { 
    tcpSrc : tpPort; 
    tcpDst : tpPort;
    tcpSeq : Int32.t;
    tcpAck : Int32.t; 
    tcpOffset : int;
    tcpFlags : int;
    tcpWindow : int; 
    tcpChksum : int;
    tcpUrgent : int;
    tcpPayload : bytes 
  }

  type icmp = { 
    icmpType : int; 
    icmpCode : int;
    icmpChksum : int;
    icmpPayload : bytes }

  type tpPkt =
    | TpTCP of tcp
    | TpICMP of icmp
    | TpUnparsable of nwProto * bytes

  type ip = { 
    pktIPVhl : int;
    pktIPTos : nwTos;
    pktIPLen : int;
    pktIPIdent : int; 
    pktIPFlags : int;
    pktIPFrag : int;
    pktIPTtl : int;
    pktIPProto : nwProto;
    pktIPChksum : int;
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
  and type portId = int

module Parser : sig

  open Types

  val parse_packet : Cstruct.t -> packet option
  val serialize_packet : packet -> Cstruct.t
  val sizeof_packet : packet -> int
  val string_of_eth : packet -> string

end
