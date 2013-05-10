open BoolAction
open ClassifierImpl
open Datatypes
open List
open NetCoreEval
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module Classifier : 
 sig 
  module Pattern : 
   sig 
    type port = NetCoreAction.NetCoreAction.Pattern.port
    
    type t = NetCoreAction.NetCoreAction.Pattern.t
    
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
  
  type pattern = NetCoreAction.NetCoreAction.pattern
  
  type port = NetCoreAction.NetCoreAction.port
  
  type action = NetCoreAction.NetCoreAction.t
  
  type t = (pattern * action) list
  
  val scan' : action -> t -> port -> packet -> action
  
  val scan : t -> port -> packet -> action
  
  val elim_shadowed_helper : t -> t -> t
  
  val elim_shadowed : t -> t
  
  val strip_empty_rules : t -> t
  
  val opt : t -> t
  
  val inter_entry :
    t -> (pattern * action) -> (Pattern.t * NetCoreAction.NetCoreAction.t)
    list
  
  val inter_no_opt :
    t -> t -> (Pattern.t * NetCoreAction.NetCoreAction.t) list
  
  val union_no_opt :
    t -> t -> (Pattern.t * NetCoreAction.NetCoreAction.t) list
  
  val par_actions : action list -> NetCoreAction.NetCoreAction.t
  
  val seq : t -> t -> port -> packet -> NetCoreAction.NetCoreAction.t
  
  val union : t -> t -> t
  
  val inter : t -> t -> t
  
  val unions : (pattern * action) list list -> (pattern * action) list
  
  val coq_Pick :
    pattern -> action -> NetCoreAction.NetCoreAction.e -> t ->
    (Pattern.t * NetCoreAction.NetCoreAction.t) list
  
  val sequence_no_opt :
    t -> t -> (pattern * NetCoreAction.NetCoreAction.t) list
  
  val sequence : t -> t -> t
 end

module BoolAction : 
 sig 
  module Pattern : 
   sig 
    type port = NetCoreAction.NetCoreAction.Pattern.port
    
    type t = NetCoreAction.NetCoreAction.Pattern.t
    
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

module BoolClassifier : 
 sig 
  module Pattern : 
   sig 
    type port = BoolAction.Pattern.port
    
    type t = BoolAction.Pattern.t
    
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
  
  type pattern = BoolAction.pattern
  
  type port = BoolAction.port
  
  type action = BoolAction.t
  
  type t = (pattern * action) list
  
  val scan' : action -> t -> port -> packet -> action
  
  val scan : t -> port -> packet -> action
  
  val elim_shadowed_helper : t -> t -> t
  
  val elim_shadowed : t -> t
  
  val strip_empty_rules : t -> t
  
  val opt : t -> t
  
  val inter_entry :
    t -> (pattern * action) -> (Pattern.t * BoolAction.t) list
  
  val inter_no_opt : t -> t -> (Pattern.t * BoolAction.t) list
  
  val union_no_opt : t -> t -> (Pattern.t * BoolAction.t) list
  
  val par_actions : action list -> BoolAction.t
  
  val seq : t -> t -> port -> packet -> BoolAction.t
  
  val union : t -> t -> t
  
  val inter : t -> t -> t
  
  val unions : (pattern * action) list list -> (pattern * action) list
  
  val coq_Pick :
    pattern -> action -> BoolAction.e -> t -> (Pattern.t * BoolAction.t) list
  
  val sequence_no_opt : t -> t -> (pattern * BoolAction.t) list
  
  val sequence : t -> t -> t
 end

val compile_pred : pred -> switchId -> BoolClassifier.t

val maybe_action :
  NetCoreAction.NetCoreAction.t -> bool -> NetCoreAction.NetCoreAction.t

val compile_pol : pol -> switchId -> Classifier.t

val to_rule :
  (Classifier.pattern * Classifier.action) -> (of_match * action list) option

val flow_table_of_policy :
  switchId -> pol -> (of_match * actionSequence) list

