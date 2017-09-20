open Core
open Async

(* Marshal and send a message to the switch *)
val send_message : Writer.t -> Frenetic_std.OpenFlow_Header.xid -> Frenetic_std.OpenFlow0x04.Message.t -> unit

(* Send group messages to switch to make group table *)
val implement_group_table : Writer.t -> Frenetic_std.GroupTable0x04.t -> unit

(* Add mask so that the meta value can be changed *)
val mask_meta : int -> int64 Frenetic_std.OpenFlow0x04.mask

(* Send FlowMod messages to switch to implement policy *)
val implement_flow :
  Writer.t
  -> Frenetic_netkat.Compiler.t
  -> Frenetic_netkat.Compiler.flow_layout
  -> Frenetic_std.OpenFlow.switchId
  -> unit

(* Send FlowMod messages to switch to implement the policy, use topology to
 * generate fault tolerant group tables. *)
val implement_tolerant_flow :
  Writer.t
  -> Frenetic_netkat.Compiler.t
  -> Frenetic_std.Net.Net.Topology.t
  -> Frenetic_std.OpenFlow.switchId
  -> unit

(* Respond to message from switch *)
val process_message :
  Frenetic_std.OpenFlow_Header.xid
  -> Frenetic_std.OpenFlow0x04.Message.t
  -> (Frenetic_std.OpenFlow_Header.xid -> Frenetic_std.OpenFlow0x04.Message.t -> unit)
  -> (Frenetic_std.OpenFlow.switchId -> unit)
  -> unit

(* Parse incoming client messages and respond. `Finished is sent if an
 * error occurs, otherwise `Repeat indefinitely. *)
val read_respond_loop :
  Reader.t
  -> (Frenetic_std.OpenFlow_Header.xid -> Frenetic_std.OpenFlow0x04.Message.t -> unit)
  -> (Frenetic_std.OpenFlow.switchId -> unit)
  -> unit
  -> [ `Finished of unit | `Repeat of unit ] Deferred.t

(* Send the initil handshake, loop on client response *)
val client_handler :
  Reader.t
  -> (Frenetic_std.OpenFlow_Header.xid -> Frenetic_std.OpenFlow0x04.Message.t -> unit)
  -> (Frenetic_std.OpenFlow.switchId -> unit)
  -> unit Deferred.t

(* Implement multi-table policies. Extract the policy from a kat file,
 * run client handler for each connecting client *)
val main : int -> string -> Frenetic_netkat.Compiler.flow_layout -> unit -> unit

(* Implement fault tolerant policies. Extract the policy and topology from
 * kat and dot files, run client_handler for each connecting client
 * TODO(mulias): This is a SHAM. Parsing the topology from a .dot file is not
 * yet implemented. *)
val fault_tolerant_main : int -> string -> string -> unit -> unit
