open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow
open Frenetic_Fabric

exception UnmatchedDyad of Dyad.t

type topology = {
  topo  : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type decider   = topology -> Dyad.t -> Dyad.t -> bool
type chooser   = topology -> Dyad.t -> Dyad.t list -> Dyad.t
type generator = topology -> (Dyad.t * Dyad.t) list -> (policy * policy)

module type SOLVER = sig
  val decide   : decider
  val choose   : chooser
  val generate : generator
end

module Z3 : sig

  exception Inconvertible of string

  type restraint =
    | Subset
    | Adjacent
    | PlacesOnly
    | Not of restraint
    | And of restraint * restraint
    | Or  of restraint * restraint

  val mk_decider : ?prereqs_file:string -> restraint -> decider
end

module Make(S:SOLVER) : sig
  val synthesize : policy -> policy -> policy -> policy
end

module Optical : SOLVER
module Generic : SOLVER
