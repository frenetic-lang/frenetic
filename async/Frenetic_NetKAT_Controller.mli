open Core.Std
open Async.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

module OF10 = Frenetic_OpenFlow0x01
module Controller = Frenetic_OpenFlow0x01_Controller
module Log = Frenetic_Log
module Upd = Frenetic_NetKAT_Updates

val bytes_to_headers : 
	Frenetic_OpenFlow.portId -> 
	Cstruct.t -> 
	Frenetic_NetKAT_Semantics.HeadersValues.t

val packet_sync_headers : 
  Frenetic_NetKAT_Semantics.packet -> 
	Frenetic_NetKAT_Semantics.packet * bool

val of_to_netkat_event : 
  Frenetic_NetKAT_Local_Compiler.t -> 
  Controller.event -> 
  Frenetic_NetKAT.event list 

module type CONTROLLER = sig
  val update_policy : policy -> unit Deferred.t
  val send_packet_out : switchId -> Frenetic_OpenFlow.pktOut -> unit Deferred.t
  val event : unit -> event Deferred.t
  val query : string -> (Int64.t * Int64.t) Deferred.t
  val port_stats : switchId -> portId -> OF10.portStats Deferred.t
  val is_query : string -> bool
  val start : ?port:int -> unit -> unit
  val current_switches : unit -> (switchId * portId list) list
  val get_table : switchId -> (Frenetic_OpenFlow.flow * string list) list
  val get_policy: unit ->  policy
end

module Make : CONTROLLER 
