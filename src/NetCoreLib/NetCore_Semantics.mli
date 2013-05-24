open Packet
open OpenFlow0x01
open NetCore_Types.Internal
open NetCore_Pattern

val match_pred : pred -> switchId -> port -> packet -> bool

val classify : pol -> value -> value list
