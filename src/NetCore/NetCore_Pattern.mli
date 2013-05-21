open Monad
open Packet
open OpenFlow0x01
open Word

type port =
  | Physical of portId
  | All
  | Bucket of int

val string_of_port : port -> string

type t 

val inter : t -> t -> t
  
val all : t
  
val empty : t
  
val exact_pattern : packet -> port -> t
  
val is_empty : t -> bool
  
val match_packet : port -> packet -> t -> bool
  
val is_exact : t -> bool
  
val to_match : t -> Match.t option
  
val dlSrc : dlAddr -> t
  
val dlDst : dlAddr -> t
  
val dlType : dlTyp -> t
  
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

val setDlVlan : dlVlan -> t -> t

val setPort : port -> t -> t

val wildcardDlSrc : t -> t
  
val wildcardDlDst : t -> t
  
val wildcardDlVlan : t -> t

val wildcardPort : t -> t

val to_string : t -> string

