open Core.Std
open NetKAT_Types
open SDN_Types
open Yojson.Basic

val policy_from_json : json -> policy
(** Note: errors may occur when converting between 64-bit values and
    JSON-representable integers. *)
val policy_to_json : policy -> json
val event_to_json : event -> json

val policy_from_json_channel : In_channel.t -> policy
val policy_from_json_string : string -> policy
val event_to_json_string : event -> string
val policy_to_json_string : policy -> string
val stats_to_json_string : Int64.t * Int64.t -> string
val port_stats_to_json_string : OpenFlow0x01_Stats.portStats -> string
