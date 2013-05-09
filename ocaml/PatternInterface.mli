open NetworkPacket
open OpenFlow0x01Types

module type PATTERN = 
 sig 
  type t 
  
  val inter : t -> t -> t
  
  val mask : t -> t -> t
  
  val all : t
  
  val empty : t
  
  val exact_pattern : packet -> portId -> t
  
  val is_empty : t -> bool
  
  val match_packet : portId -> packet -> t -> bool
  
  val is_exact : t -> bool
  
  val to_match : t -> of_match
  
  val beq : t -> t -> bool
  
  val dlSrc : dlAddr -> t
  
  val dlDst : dlAddr -> t
  
  val dlTyp : dlTyp -> t
  
  val dlVlan : dlVlan -> t
  
  val dlVlanPcp : dlVlanPcp -> t
  
  val ipSrc : nwAddr -> t
  
  val ipDst : nwAddr -> t
  
  val ipProto : nwProto -> t
  
  val inPort : portId -> t
  
  val tcpSrcPort : tpPort -> t
  
  val tcpDstPort : tpPort -> t
  
  val udpSrcPort : tpPort -> t
  
  val udpDstPort : tpPort -> t
 end

