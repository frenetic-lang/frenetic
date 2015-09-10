open Core.Std
open Async.Std
module Log = Frenetic_Log

(* Marshal and send a message to the switch *)
val send_message : Writer.t -> Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit

(* Send group messages to switch to make group table *)
val implement_group_table : Writer.t -> Frenetic_GroupTable0x04.t -> unit

(* Add mask so that the meta value can be changed *)
val mask_meta : int -> int64 Frenetic_OpenFlow0x04.mask

(* Send FlowMod messages to switch to implement policy *)
val implement_flow :
  Writer.t
  -> Frenetic_NetKAT_Compiler.t
  -> Frenetic_NetKAT_Compiler.flow_layout
  -> Frenetic_OpenFlow.switchId
  -> unit

(* Send FlowMod messages to switch to implement the policy, use topology to
 * generate fault tolerant group tables. *)
val implement_tolerant_flow :
  Writer.t
  -> Frenetic_NetKAT_Compiler.t
  -> Frenetic_NetKAT_Net.Net.Topology.t
  -> Frenetic_OpenFlow.switchId
  -> unit

(* Respond to message from switch *)
val process_message :
  Frenetic_OpenFlow_Header.xid
  -> Frenetic_OpenFlow0x04.Message.t
  -> (Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit)
  -> (Frenetic_OpenFlow.switchId -> unit)
  -> unit

(* Parse incoming client messages and respond. `Finished is sent if an
 * error occurs, otherwise `Repeat indefinitely. *)
val read_respond_loop :
  Reader.t
  -> (Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit)
  -> (Frenetic_OpenFlow.switchId -> unit)
  -> unit
  -> [ `Finished of unit | `Repeat of unit ] Deferred.t

(* Send the initil handshake, loop on client response *)
val client_handler :
  Reader.t
  -> (Frenetic_OpenFlow_Header.xid -> Frenetic_OpenFlow0x04.Message.t -> unit)
  -> (Frenetic_OpenFlow.switchId -> unit)
  -> unit Deferred.t

(* Implement multi-table policies. Extract the policy from a kat file,
 * run client handler for each connecting client *)
val main : int -> string -> Frenetic_NetKAT_Compiler.flow_layout -> unit -> unit

(* Implement fault tolerant policies. Extract the policy and topology from
 * kat and dot files, run client_handler for each connecting client
 * TODO(mulias): This is a SHAM. Parsing the topology from a .dot file is not
 * yet implemented. *)
val fault_tolerant_main : int -> string -> string -> unit -> unit
