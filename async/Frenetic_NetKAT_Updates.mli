open Core.Std
open Async.Std

module Net = Frenetic_NetKAT_Net.Net
module SDN = Frenetic_OpenFlow
module LC = Frenetic_NetKAT_Local_Compiler

open SDN.To0x01

exception UpdateError

module SwitchMap : sig
  type 'a t with sexp
end

type edge = (SDN.flow * int) list SwitchMap.t 

module type UPDATE_ARGS = sig
  val get_order : unit -> LC.order
  val get_prev_order : unit -> LC.order
end

module type CONSISTENT_UPDATE_ARGS = sig
  val get_order : unit -> LC.order
  val get_prev_order : unit -> LC.order
  val get_nib : unit -> Net.Topology.t
  val get_edge : unit -> edge
  val set_edge : edge -> unit
end


module type UPDATE = sig
  val bring_up_switch : 
    ?old:LC.t ->
    SDN.switchId ->
    LC.t ->
    unit Deferred.t

  val implement_policy :
    ?old:LC.t ->
    LC.t ->
    unit Deferred.t

  val set_current_compiler_options : LC.compiler_options -> unit
end

module BestEffortUpdate : UPDATE

module PerPacketConsistent (Args : CONSISTENT_UPDATE_ARGS) : UPDATE
