open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Pattern

val match_pred : pred -> switchId -> port -> packet -> bool

val eval_action : value -> action -> value list

val eval : pol -> value -> action
val classify : pol -> value -> value list
