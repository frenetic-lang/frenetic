(* TODO: Currently Unused.  See Frenetic_OpenFlow0x01_Controller for Future Directions *)

open Core.Std
open Async.Std

module Net = Frenetic_NetKAT_Net.Net
module SDN = Frenetic_OpenFlow
module Comp = Frenetic_NetKAT_Compiler

exception UpdateError

module SwitchMap : sig
  type 'a t [@@deriving sexp]
end

type edge = (SDN.flow * int) list SwitchMap.t

module type UPDATE_ARGS = sig
  val get_order : unit -> Comp.order
  val get_prev_order : unit -> Comp.order
end

module type CONSISTENT_UPDATE_ARGS = sig
  val get_order : unit -> Comp.order
  val get_prev_order : unit -> Comp.order
  val get_nib : unit -> Net.Topology.t
  val get_edge : unit -> edge
  val set_edge : edge -> unit
end


module type UPDATE = sig
  val bring_up_switch :
    SDN.switchId ->
    Comp.t ->
    unit Deferred.t

  val implement_policy :
    Comp.t ->
    unit Deferred.t

  val set_current_compiler_options : Comp.compiler_options -> unit
end

module BestEffortUpdate : UPDATE

module PerPacketConsistent (Args : CONSISTENT_UPDATE_ARGS) : UPDATE
