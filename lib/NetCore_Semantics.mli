open Packet
(* open OpenFlow0x01 *)
open NetCore_Types

val match_pred : pred -> switchId -> port -> packet -> bool

val eval_action : action -> value -> value list

val eval : pol -> value -> value list

val handle_switch_events : switchEvent -> pol -> unit
