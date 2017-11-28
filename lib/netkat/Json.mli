(** JSON serialization/deserialization of NetKAT messages.

  NetKAT comes in two syntaxes: "regular" which is handled by Parser/Lexer
  and "json" which is handled here.  We translate most things to the Syntax.policy
  type

  This module also serializes/deserializes switch-to-controller OpenFlow messages for net apps.
*)
open Core
open Yojson.Basic

open Frenetic_kernel.OpenFlow
open Syntax

(* {1 Json Serialization/Deserialization} *)

(** Deserialize a Yojson repesentation of a NetKAT policy *)
val pol_of_json : json -> policy

(** Serialize a NetKAT policy to Yojson format.  Note: errors may occur when converting between 64-bit values and
    JSON-representable integers. *)
val policy_to_json : policy -> json

val from_json_header_val : json -> header_val

(** Same as pol_of_json, but reads json from input channel *)
val pol_of_json_channel : In_channel.t -> policy

(** Same as event_to_json but returns json string *)
val event_to_json_string : event -> string

(** Same as pol_of_json, but receives json string *)
val pol_of_json_string : string -> policy

val policy_to_json_string : policy -> string

(** Sames as stats_to_json but returns json string *)
val stats_to_json_string : Int64.t * Int64.t -> string

val port_stat_to_json_string : portStats -> string

(* Used to be in SDN_Json *)
val pseudoport_to_json : pseudoport -> json
val pseudoport_from_json : json -> pseudoport
val flowTable_to_json : flowTable -> json
val pkt_out_from_json : json -> switchId * portId option * payload * policy list
