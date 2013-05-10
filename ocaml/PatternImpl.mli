open Datatypes
open Monad
open NetworkPacket
open OpenFlow0x01Types
open PatternSignatures
open WordInterface

module Make : 
 functor (Port:PORT) ->
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
    Wildcard.coq_Wildcard -> port Wildcard.coq_Wildcard -> 'a1) -> pattern ->
    'a1
  
  val pattern_rec :
    (dlAddr Wildcard.coq_Wildcard -> dlAddr Wildcard.coq_Wildcard -> dlTyp
    Wildcard.coq_Wildcard -> dlVlan Wildcard.coq_Wildcard -> dlVlanPcp
    Wildcard.coq_Wildcard -> nwAddr Wildcard.coq_Wildcard -> nwAddr
    Wildcard.coq_Wildcard -> nwProto Wildcard.coq_Wildcard -> nwTos
    Wildcard.coq_Wildcard -> tpPort Wildcard.coq_Wildcard -> tpPort
    Wildcard.coq_Wildcard -> port Wildcard.coq_Wildcard -> 'a1) -> pattern ->
    'a1
  
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
  
  val to_all : 'a1 Wildcard.coq_Wildcard -> bool -> 'a1 Wildcard.coq_Wildcard
  
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

