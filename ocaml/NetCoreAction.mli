open Datatypes
open List
open NetworkPacket
open OpenFlow0x01Types
open PatternImpl
open PatternSignatures
open WordInterface

module Port : 
 sig 
  type port =
  | Physical of portId
  | Here
  | Bucket of int
  
  val port_rect : (portId -> 'a1) -> 'a1 -> (int -> 'a1) -> port -> 'a1
  
  val port_rec : (portId -> 'a1) -> 'a1 -> (int -> 'a1) -> port -> 'a1
  
  val eqdec : port -> port -> bool
  
  val opt_portId : port -> portId option
  
  type t = port
 end

module type NETCORE_ACTION = 
 sig 
  module Pattern : 
   PATTERN
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t 
  
  type e 
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> pattern -> pattern
  
  val domain : e -> pattern
  
  val forward : portId -> t
  
  val bucket : int -> t
  
  val updateDlSrc : dlAddr -> dlAddr -> t
  
  val as_actionSequence : portId option -> t -> actionSequence
 end

module NetCoreAction : 
 sig 
  module Pattern : 
   sig 
    type port = Port.t
    
    type pattern = { ptrnDlSrc : dlAddr Wildcard.coq_Wildcard;
                     ptrnDlDst : dlAddr Wildcard.coq_Wildcard;
                     ptrnDlType : dlTyp Wildcard.coq_Wildcard;
                     ptrnDlVlan : dlVlan Wildcard.coq_Wildcard;
                     ptrnDlVlanPcp : dlVlanPcp Wildcard.coq_Wildcard;
                     ptrnNwSrc : nwAddr Wildcard.coq_Wildcard;
                     ptrnNwDst : nwAddr Wildcard.coq_Wildcard;
                     ptrnNwProto : nwProto Wildcard.coq_Wildcard;
                     ptrnNwTos : nwTos Wildcard.coq_Wildcard;
                     ptrnTpSrc : tpPort Wildcard.coq_Wildcard;
                     ptrnTpDst : tpPort Wildcard.coq_Wildcard;
                     ptrnInPort : port Wildcard.coq_Wildcard }
    
    val pattern_rect :
      (dlAddr Wildcard.coq_Wildcard -> dlAddr Wildcard.coq_Wildcard -> dlTyp
      Wildcard.coq_Wildcard -> dlVlan Wildcard.coq_Wildcard -> dlVlanPcp
      Wildcard.coq_Wildcard -> nwAddr Wildcard.coq_Wildcard -> nwAddr
      Wildcard.coq_Wildcard -> nwProto Wildcard.coq_Wildcard -> nwTos
      Wildcard.coq_Wildcard -> tpPort Wildcard.coq_Wildcard -> tpPort
      Wildcard.coq_Wildcard -> port Wildcard.coq_Wildcard -> 'a1) -> pattern
      -> 'a1
    
    val pattern_rec :
      (dlAddr Wildcard.coq_Wildcard -> dlAddr Wildcard.coq_Wildcard -> dlTyp
      Wildcard.coq_Wildcard -> dlVlan Wildcard.coq_Wildcard -> dlVlanPcp
      Wildcard.coq_Wildcard -> nwAddr Wildcard.coq_Wildcard -> nwAddr
      Wildcard.coq_Wildcard -> nwProto Wildcard.coq_Wildcard -> nwTos
      Wildcard.coq_Wildcard -> tpPort Wildcard.coq_Wildcard -> tpPort
      Wildcard.coq_Wildcard -> port Wildcard.coq_Wildcard -> 'a1) -> pattern
      -> 'a1
    
    val ptrnDlSrc : pattern -> dlAddr Wildcard.coq_Wildcard
    
    val ptrnDlDst : pattern -> dlAddr Wildcard.coq_Wildcard
    
    val ptrnDlType : pattern -> dlTyp Wildcard.coq_Wildcard
    
    val ptrnDlVlan : pattern -> dlVlan Wildcard.coq_Wildcard
    
    val ptrnDlVlanPcp : pattern -> dlVlanPcp Wildcard.coq_Wildcard
    
    val ptrnNwSrc : pattern -> nwAddr Wildcard.coq_Wildcard
    
    val ptrnNwDst : pattern -> nwAddr Wildcard.coq_Wildcard
    
    val ptrnNwProto : pattern -> nwProto Wildcard.coq_Wildcard
    
    val ptrnNwTos : pattern -> nwTos Wildcard.coq_Wildcard
    
    val ptrnTpSrc : pattern -> tpPort Wildcard.coq_Wildcard
    
    val ptrnTpDst : pattern -> tpPort Wildcard.coq_Wildcard
    
    val ptrnInPort : pattern -> port Wildcard.coq_Wildcard
    
    val eq_dec : pattern -> pattern -> bool
    
    val all : pattern
    
    val empty : pattern
    
    val is_empty : pattern -> bool
    
    val wild_to_opt : 'a1 Wildcard.coq_Wildcard -> 'a1 option option
    
    val to_match : pattern -> of_match option
    
    val inter : pattern -> pattern -> pattern
    
    val exact_pattern : packet -> port -> pattern
    
    val match_packet : port -> packet -> pattern -> bool
    
    val is_exact : pattern -> bool
    
    val coq_SupportedNwProto : int list
    
    val coq_SupportedDlTyp : int list
    
    val to_valid : pattern -> pattern
    
    val to_all :
      'a1 Wildcard.coq_Wildcard -> bool -> 'a1 Wildcard.coq_Wildcard
    
    val setDlSrc : dlAddr -> pattern -> pattern
    
    val setDlDst : dlAddr -> pattern -> pattern
    
    type t = pattern
    
    val beq : pattern -> pattern -> bool
    
    val inPort : port -> t
    
    val dlSrc : dlAddr -> t
    
    val dlDst : dlAddr -> t
    
    val dlTyp : dlTyp -> t
    
    val dlVlan : dlVlan -> t
    
    val dlVlanPcp : dlVlanPcp -> t
    
    val ipSrc : nwAddr -> t
    
    val ipDst : nwAddr -> t
    
    val ipProto : nwProto -> t
    
    val tpSrcPort : nwProto -> tpPort -> t
    
    val tpDstPort : nwProto -> tpPort -> t
    
    val tcpSrcPort : tpPort -> t
    
    val tcpDstPort : tpPort -> t
    
    val udpSrcPort : tpPort -> t
    
    val udpDstPort : tpPort -> t
   end
  
  type 'a match_modify = ('a * 'a) option
  
  type output = { outDlSrc : dlAddr match_modify;
                  outDlDst : dlAddr match_modify;
                  outDlVlan : dlVlan option match_modify;
                  outDlVlanPcp : dlVlanPcp match_modify;
                  outNwSrc : nwAddr match_modify;
                  outNwDst : nwAddr match_modify;
                  outNwTos : nwTos match_modify;
                  outTpSrc : tpPort match_modify;
                  outTpDst : tpPort match_modify; outPort : Pattern.port }
  
  val output_rect :
    (dlAddr match_modify -> dlAddr match_modify -> dlVlan option match_modify
    -> dlVlanPcp match_modify -> nwAddr match_modify -> nwAddr match_modify
    -> nwTos match_modify -> tpPort match_modify -> tpPort match_modify ->
    Pattern.port -> 'a1) -> output -> 'a1
  
  val output_rec :
    (dlAddr match_modify -> dlAddr match_modify -> dlVlan option match_modify
    -> dlVlanPcp match_modify -> nwAddr match_modify -> nwAddr match_modify
    -> nwTos match_modify -> tpPort match_modify -> tpPort match_modify ->
    Pattern.port -> 'a1) -> output -> 'a1
  
  val outDlSrc : output -> dlAddr match_modify
  
  val outDlDst : output -> dlAddr match_modify
  
  val outDlVlan : output -> dlVlan option match_modify
  
  val outDlVlanPcp : output -> dlVlanPcp match_modify
  
  val outNwSrc : output -> nwAddr match_modify
  
  val outNwDst : output -> nwAddr match_modify
  
  val outNwTos : output -> nwTos match_modify
  
  val outTpSrc : output -> tpPort match_modify
  
  val outTpDst : output -> tpPort match_modify
  
  val outPort : output -> Pattern.port
  
  type act = output list
  
  val drop : act
  
  val pass : output list
  
  val forward : portId -> output list
  
  val bucket : int -> output list
  
  val updateDlSrc : dlAddr -> dlAddr -> output list
  
  val par_action : act -> act -> act
  
  val seq_mod :
    ('a1 -> 'a1 -> bool) -> 'a1 match_modify -> 'a1 match_modify ->
    ('a1 * 'a1) option option
  
  val seq_port : Pattern.port -> Pattern.port -> Pattern.port
  
  val optword16beq : Word16.t option -> Word16.t option -> bool
  
  val seq_output : output -> output -> output option
  
  val cross : 'a1 list -> 'a2 list -> ('a1 * 'a2) list
  
  val seq_action : act -> act -> act
  
  val maybe_modify :
    'a1 match_modify -> (packet -> 'a1 -> packet) -> packet -> packet
  
  val withVlanNone : dlVlan option match_modify -> (dlVlan * dlVlan) option
  
  val apply_atom :
    output -> (Pattern.port * packet) -> (Pattern.port * packet) option
  
  val trans :
    'a1 match_modify -> ('a1 -> Pattern.t -> Pattern.t) -> Pattern.t ->
    Pattern.t
  
  val sel : ('a1 -> Pattern.t) -> 'a1 match_modify -> Pattern.t
  
  val restrict_range : output -> Pattern.t -> Pattern.t
  
  val domain : output -> Pattern.pattern
  
  val set :
    'a1 match_modify -> ('a1 -> action) -> actionSequence -> action list
  
  val unset :
    'a1 match_modify -> ('a1 -> action) -> actionSequence -> action list
  
  val setDlVlan' : dlVlan option -> action
  
  val modify : output -> actionSequence
  
  val unmodify : output -> actionSequence
  
  val output_to_of : portId option -> output -> actionSequence
  
  val as_actionSequence : portId option -> act -> action list
  
  type t = act
  
  type e = output
  
  type pattern = Pattern.t
  
  type port = Port.t
  
  val atoms : t -> e list
  
  val apply_action : t -> (Port.t * packet) -> (Pattern.port * packet) list
 end

