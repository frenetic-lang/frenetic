open ClassifierSignatures
open Datatypes
open List0
open NetworkPacket
open OpenFlow0x01Types

module Make : 
 functor (Action:ACTION) ->
 sig 
  module Pattern : 
   sig 
    type port = Action.Pattern.port
    
    type t = Action.Pattern.t
    
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
  
  type pattern = Action.pattern
  
  type port = Action.port
  
  type action = Action.t
  
  type t = (pattern * action) list
  
  val scan' : action -> t -> port -> packet -> action
  
  val scan : t -> port -> packet -> action
  
  val elim_shadowed_helper : t -> t -> t
  
  val elim_shadowed : t -> t
  
  val strip_empty_rules : t -> t
  
  val opt : t -> t
  
  val inter_entry : t -> (pattern * action) -> (Pattern.t * Action.t) list
  
  val inter_no_opt : t -> t -> (Pattern.t * Action.t) list
  
  val union_no_opt : t -> t -> (Pattern.t * Action.t) list
  
  val par_actions : action list -> Action.t
  
  val seq : t -> t -> port -> packet -> Action.t
  
  val union : t -> t -> t
  
  val inter : t -> t -> t
  
  val unions : (pattern * action) list list -> (pattern * action) list
  
  val coq_Pick :
    pattern -> action -> Action.e -> t -> (Pattern.t * Action.t) list
  
  val sequence_no_opt : t -> t -> (pattern * Action.t) list
  
  val sequence : t -> t -> t
 end

