open Packet
open NetCore_Wildcard

type portId = int32
type queueId = int32

type port =
  | Physical of portId
  | Queue of portId * queueId
  | All
  | Here

type t = {
  ptrnDlSrc : dlAddr wildcard;
  ptrnDlDst : dlAddr wildcard;
  ptrnDlTyp : dlTyp wildcard;
  ptrnDlVlan : dlVlan wildcard;
  ptrnDlVlanPcp : dlVlanPcp wildcard;
  ptrnNwSrc : nwAddr wildcard;
  ptrnNwDst : nwAddr wildcard;
  ptrnNwProto : nwProto wildcard;
  ptrnNwTos : nwTos wildcard;
  ptrnTpSrc : tpPort wildcard;
  ptrnTpDst : tpPort wildcard;
  ptrnInPort : port wildcard
}

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

val all : t

val empty : t

val dlSrc : dlAddr -> t

val dlDst : dlAddr -> t

val dlTyp : dlTyp -> t

val dlVlan : dlVlan -> t

val dlVlanPcp : dlVlanPcp -> t

val ipSrc : nwAddr -> t

val ipDst : nwAddr -> t

val ipProto : nwProto -> t

val ipTos : nwTos -> t

val inPort : port -> t

val tcpSrcPort : tpPort -> t

val tcpDstPort : tpPort -> t

val udpSrcPort : tpPort -> t

val udpDstPort : tpPort -> t
