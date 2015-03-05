open Core.Std
open SDN_Types
open Yojson.Basic

val pseudoport_to_json : pseudoport -> json
val pseudoport_from_json : json -> pseudoport
val flowTable_to_json : flowTable -> json
val pkt_out_from_json : json -> switchId * pktOut
