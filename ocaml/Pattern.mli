open Monad
open NetworkPacket
open OpenFlow0x01Types
open Word

module type PORT = sig
  type t 
  val opt_portId : t -> portId option
 end

module type PATTERN = sig
  type port 
  
  type t 
  
  val inter : t -> t -> t
  
  val all : t
  
  val empty : t
  
  val exact_pattern : packet -> port -> t
  
  val is_empty : t -> bool
  
  val match_packet : port -> packet -> t -> bool
  
  val is_exact : t -> bool
  
  val to_match : t -> of_match option
  
  val beq : t -> t -> bool
  
  val dlSrc : dlAddr -> t
  
  val dlDst : dlAddr -> t
  
  val dlTyp : dlTyp -> t
  
  val dlVlan : dlVlan -> t
  
  val dlVlanPcp : dlVlanPcp -> t
  
  val ipSrc : nwAddr -> t
  
  val ipDst : nwAddr -> t
  
  val ipProto : nwProto -> t
  
  val inPort : port -> t
  
  val tcpSrcPort : tpPort -> t
  
  val tcpDstPort : tpPort -> t
  
  val udpSrcPort : tpPort -> t
  
  val udpDstPort : tpPort -> t
  
  val setDlSrc : dlAddr -> t -> t
  
  val setDlDst : dlAddr -> t -> t
 end

module type MAKE = functor (Port:PORT) -> sig
  include PATTERN
end
  with type port = Port.t

module Make : MAKE

