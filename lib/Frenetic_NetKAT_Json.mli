(** JSON serialization/deserialization of NetKAT messages.

  NetKAT comes in two syntaxes: "regular" which is handled by Frenetic_NetKAT_Parser/Lexer
  and "json" which is handled here.  We translate most things to the Frenetic_NetKAT.policy
  type

  This module also serializes/deserializes switch-to-controller OpenFlow messages for net apps.
  TODO: These functions should be moved to Frenetic_NetKAT_SDN_Json, or should be consolidated
  here.  There's no good reason to differentiate between switch-to-controller and
  controller-to-switch OpenFlow messages.  The only problem is this module can't open
  Frenetic_OpenFlow due to collisions in some data types ... that'll need to be solved.

*)
open Core.Std
open Frenetic_NetKAT
open Yojson.Basic

(* {1 Json Serialization/Deserialization} *)

(** Deserialize a Yojson repesentation of a NetKAT policy *)
val policy_of_json : json -> policy

(** Serialize a NetKAT policy to Yojson format.  Note: errors may occur when converting between 64-bit values and
    JSON-representable integers. *)
val policy_to_json : policy -> json

val from_json_header_val : json -> header_val

(** Serialize an abstract OpenFlow event (PacketIn, SwitchUp, etc.) to Yojson format. *)
val event_to_json : event -> json

val event_from_json:json -> event

(** Serialize an OpenFlow switch stats response to Yojson format. *)
val stats_to_json : Int64.t * Int64.t -> json

(** Serialize an OpenFlow port stats response to Yojson format *)
val port_stats_to_json : Frenetic_OpenFlow0x01.portStats list -> json

(* {1 Shortcuts} *)

(** Same as policy_of_json, but receives json string *)
val policy_of_json_string : string -> policy

(** Same as policy_of_json, but reads json from input channel *)
val policy_of_json_channel : In_channel.t -> policy

(** Same as event_to_json but returns json string *)
val event_to_json_string : event -> string

(** Same as policy_to_json but returns json string *)
val policy_to_json_string : policy -> string

(** Sames as stats_to_json but returns json string *)
val stats_to_json_string : Int64.t * Int64.t -> string

val stats_to_json : Int64.t * Int64.t -> json

(** Same as port_stats_to_json but returns json string *)
val port_stats_to_json_string : Frenetic_OpenFlow0x01.portStats list -> string

