open Classifier
open Packet
open ControllerInterface0x04
open Datatypes
open List0
open OpenFlow0x04Types
open NetCoreCompiler0x04
open NetCoreEval0x04
open Types
open WordInterface

val prio_rec :
  Word16.t -> 'a1 coq_Classifier -> ((Word16.t*Pattern.pattern)*'a1) list

val prioritize : 'a1 coq_Classifier -> ((Word16.t*Pattern.pattern)*'a1) list

val packetIn_to_in : switchId -> packetIn -> input

val maybe_openflow0x01_modification :
  'a1 option -> ('a1 -> action) -> actionSequence

val modification_to_openflow0x01 : modification -> actionSequence

val translate_action : portId option -> act -> actionSequence

val to_flow_mod : Word16.t -> Pattern.pattern -> act list -> tableId -> flowMod

val flow_mods_of_classifier : act list coq_Classifier -> tableId -> flowMod list

val to_group_mod : groupId -> groupType -> act list list -> groupMod

val group_mods_of_classifier : (groupId*groupType*(act list list)) list -> groupMod list

val delete_all_flows : tableId -> flowMod 

module type NETCORE_MONAD = 
 sig 
  type 'x m 
  
  val bind : 'a1 m -> ('a1 -> 'a2 m) -> 'a2 m
  
  val ret : 'a1 -> 'a1 m

  type state = { policy : pol; switches : switchId list }

  val policy : state -> pol

  val switches : state -> switchId list
  
  val get : state m
  
  val put : state -> unit m
  
  val send : switchId -> xid -> message -> unit m
  
  val recv : event m
  
  val forever : unit m -> unit m
  
  val handle_get_packet : id -> switchId -> portId -> packet -> unit m
 end

module Make : 
 functor (Monad:NETCORE_MONAD) ->
 sig 
  val sequence : unit Monad.m list -> unit Monad.m
  
  val config_commands : pol -> switchId -> tableId -> unit Monad.m
  
  val set_policy : pol -> unit Monad.m
  
  val handle_switch_disconnected : switchId -> unit Monad.m
  
  val handle_switch_connected : switchId -> unit Monad.m
  
  val send_output : output -> unit Monad.m
  
  val handle_packet_in : switchId -> packetIn -> unit Monad.m
  
  val handle_event : event -> unit Monad.m
  
  val main : unit Monad.m
 end

