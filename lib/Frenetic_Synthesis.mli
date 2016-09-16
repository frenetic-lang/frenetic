open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow
open Frenetic_Fabric

type heuristic =
  | Random of int * int
  | MaxSpread
  | MinSpread

type topology = {
  topo : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type decider   = topology -> stream -> stream -> bool
type generator = topology -> (stream * stream list) list -> (policy * policy)

module SMT : sig
  type condition
  type action
  type dyad

  val of_condition : Frenetic_Fabric.condition -> condition
  val of_action : Frenetic_Fdd.Action.t -> action

  val of_dyad : stream -> dyad
end

module type MAPPING = sig
  val decide   : decider
  val generate : generator
end

module Make(M:MAPPING) : sig
  val synthesize : ?heuristic:heuristic -> policy -> policy -> policy -> policy
end

module Optical : MAPPING
module Generic : MAPPING
