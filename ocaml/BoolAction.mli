open NetworkPacket
open OpenFlow0x01Types
open PatternSignatures
(* open Types *)

module Make : 
 functor (Pattern_:PATTERN) ->
 sig 
  module Pattern : 
   sig 
    type port = Pattern_.port
    
    type t = Pattern_.t
    
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
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t = bool
  
  type e = bool
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> pattern -> pattern
  
  val domain : e -> Pattern.t
 end

