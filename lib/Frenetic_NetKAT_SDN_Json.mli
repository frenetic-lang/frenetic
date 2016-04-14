(** JSON serialization/deserialization of abstract OpenFlow messages.  

  Frenetic acts as a passthrough mechanism for certain OpenFlow events.  For example,
  a net app can execute an OpenFlow Packet Out message to send a packet to the 
  switch.  NetKAT has nothing to say about this transaction, so the JSON used to
  construct the message pretty much mirrors the OpenFlow.  

  JSON NetKAT-specific messages to Frenetic (specifically "implement this policy") are
  serialized/deserailized by the similarly-named Frenetic_NetKAT_Json module  
  
*)

open Core.Std
open Frenetic_OpenFlow
open Yojson.Basic

(** Construct a Json representiation from a pseudoport like Physical(2l) *)
val pseudoport_to_json : pseudoport -> json

(** Construct a pseudoport from Json representiation like Assoc `physical: 2  *)
val pseudoport_from_json : json -> pseudoport

(** Construct an abstract PacketOut message from a Json representation. *)
val pkt_out_from_json : json -> switchId * pktOut

(** Construct a json repesentation from an abstract flow table.  Used mostly for Frenetic compile_server *)
val flowTable_to_json : flowTable -> json

