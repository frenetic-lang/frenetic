open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow
open Yojson.Basic

val policy_from_json : json -> policy
(** Note: errors may occur when converting between 64-bit values and
    JSON-representable integers. *)
val policy_to_json : policy -> json
val from_json_header_val : json -> header_val

val policy_from_json_channel : In_channel.t -> policy
val policy_from_json_string : string -> policy
val policy_to_json_string : policy -> string
val stats_to_json_string : Int64.t * Int64.t -> string

(* Used to be in Frenetic_NetKAT_SDN_Json *)                                                                         
val pseudoport_to_json : pseudoport -> json
val pseudoport_from_json : json -> pseudoport
val flowTable_to_json : flowTable -> json
val pkt_out_from_json : json -> switchId * payload * policy
