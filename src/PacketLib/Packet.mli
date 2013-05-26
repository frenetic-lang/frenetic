(** Packet serialization library. *)

(** {9 Packet types}

    Based on {{:
    https://openflow.stanford.edu/display/ONL/POX+Wiki#POXWiki-Workingwithpacketspoxlibpacket}
    the packet library from POX}.

    You can navigate a packet's structure directly from here. But, using
    {!accs} may be more convenient.

*)

type bytes = Cstruct.t

type int8 = int

type int16 = int

type int48 = int64

type portId = int16

type dlAddr = int48

type dlTyp = int16

type dlVlan = int16 option

type dlVlanPcp = int8

type nwAddr = int32

type nwProto = int8

type nwTos = int8

type tpPort = int16

module Tcp : sig
  type t = {
    src : tpPort; 
    dst : tpPort; 
    seq : int32;
    ack : int32; 
    offset : int8; 
    flags : int16;
    window : int16; 
    chksum : int8; 
    urgent : int8;
    payload : bytes 
  }

  val parse : Cstruct.t -> t option
  val len : t -> int
  val serialize : Cstruct.t -> t -> unit

end

type icmp = {
  icmpType : int8;
  icmpCode : int8;
  icmpChksum : int16;
  icmpPayload : bytes
}

type tpPkt =
  | TpTCP of Tcp.t
  | TpICMP of icmp
  | TpUnparsable of nwProto * bytes

type ip = {
  pktIPVhl : int8;
  pktIPTos : nwTos;
  pktIPLen : int16;
  pktIPIdent : int16;
  pktIPFlags : int8;
  pktIPFrag : int16;
  pktIPTtl : int8;
  pktIPProto : nwProto;
  pktIPChksum : int16;
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

(** {9:accs Accessors} *)

val pktNwSrc : packet -> nwAddr

val pktNwDst : packet -> nwAddr

val pktNwTos : packet -> nwTos

val pktNwProto : packet -> nwProto

val pktTpSrc : packet -> tpPort

val pktTpDst : packet -> tpPort

(** {9 Mutators} *)

val setDlSrc : packet -> dlAddr -> packet

val setDlDst : packet -> dlAddr -> packet

val setDlVlan : packet -> dlVlan -> packet

val setDlVlanPcp : packet -> dlVlanPcp -> packet

val setNwSrc : packet -> nwAddr -> packet

val setNwDst : packet -> nwAddr -> packet

val setNwTos : packet -> nwTos -> packet

val setTpSrc : packet -> tpPort -> packet

val setTpDst : packet -> tpPort -> packet

(** {9 Pretty Printing} *)

val string_of_mac : int48 -> string

val portId_to_string : int16 -> string

val dlAddr_to_string : int48 -> string

val dlTyp_to_string : int16 -> string

val dlVlan_to_string : int16 option -> string

val dlVlanPcp_to_string : int8 -> string

val nwAddr_to_string : int32 -> string

val nwProto_to_string : int8 -> string

val nwTos_to_string : int8 -> string

val tpPort_to_string : int16 -> string

val nw_to_string : nw -> string

val packet_to_string : packet -> string

val string_of_mac : int48 -> string

val bytes_of_mac : int48 -> string

val mac_of_bytes : string -> int48

val string_of_ip : int32 -> string
