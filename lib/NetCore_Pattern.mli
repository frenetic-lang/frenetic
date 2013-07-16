open Packet
open NetCore_Types

type t = ptrn

val inter : t -> t -> t

(** [contains x y] returns [true] if the pattern [x] is a sub-pattern of [y]. *)
val contains : t -> t -> bool
  
val exact_pattern : packet -> port -> t
  
val is_empty : t -> bool

val is_all : t -> bool
  
val match_packet : port -> packet -> t -> bool
  
val to_match0x01 : t -> OpenFlow0x01.Match.t option

val to_match0x04 : t -> OpenFlow0x04_Core.oxmMatch * portId option
  
val setDlSrc : dlAddr -> t -> t
  
val setDlDst : dlAddr -> t -> t

val setDlVlan : dlVlan -> t -> t

val setPort : port -> t -> t

val wildcardDlSrc : t -> t
  
val wildcardDlDst : t -> t
  
val wildcardDlVlan : t -> t

val wildcardPort : t -> t

val wildcardDlVlanPcp : t -> t

val wildcardNwSrc : t -> t

val wildcardNwDst : t -> t

val wildcardNwTos : t -> t

val wildcardTpSrc : t -> t

val wildcardTpDst : t -> t
