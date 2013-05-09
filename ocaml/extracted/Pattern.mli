open NetworkPacket
open OpenFlow0x01Types
open PatternImplDef
open WordInterface

type __ = Obj.t

module Pattern : 
 sig 
  type pat =
    pattern
    (* singleton inductive, whose constructor was Pat *)
  
  val pat_rect : (pattern -> __ -> 'a1) -> pat -> 'a1
  
  val pat_rec : (pattern -> __ -> 'a1) -> pat -> 'a1
  
  val raw : pat -> pattern
  
  type t = pat
  
  val beq : t -> t -> bool
  
  val inter : t -> t -> pat
  
  val mask : t -> t -> t
  
  val all : t
  
  val empty : t
  
  val exact_pattern : packet -> Word16.t -> t
  
  val is_empty : pat -> bool
  
  val match_packet : Word16.t -> packet -> pat -> bool
  
  val is_exact : pat -> bool
  
  val to_match : pat -> of_match
  
  val inPort : portId -> t
  
  val dlSrc : dlAddr -> t
  
  val dlDst : dlAddr -> t
  
  val dlTyp : dlTyp -> t
  
  val dlVlan : dlVlan -> t
  
  val dlVlanPcp : dlVlanPcp -> t
  
  val ipSrc : nwAddr -> t
  
  val ipDst : nwAddr -> t
  
  val ipProto : nwProto -> t
  
  val tpSrcPort : int -> tpPort -> t
  
  val tpDstPort : int -> tpPort -> t
  
  val tcpSrcPort : tpPort -> t
  
  val tcpDstPort : tpPort -> t
  
  val udpSrcPort : tpPort -> t
  
  val udpDstPort : tpPort -> t
 end

type pattern = Pattern.t

