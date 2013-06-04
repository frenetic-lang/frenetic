open Packet
open OpenFlow0x01
open NetCore_Types

val match_pred : pred -> switchId -> port -> packet -> bool

val eval_action : value -> action -> value list

val eval : pol -> value -> action
val classify : pol -> value -> value list

val handle_switch_events : switchEvent -> pol -> unit
