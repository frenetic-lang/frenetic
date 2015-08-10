open Core.Std
open Frenetic_OpenFlow
open Yojson.Basic

val pseudoport_to_json : pseudoport -> json
val pseudoport_from_json : json -> pseudoport
val flowTable_to_json : flowTable -> json
val flowTable'_to_json : (flow * string list) list -> json
val pkt_out_from_json : json -> switchId * pktOut
