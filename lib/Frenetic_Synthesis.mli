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

type decider   = topology -> Dyad.t -> Dyad.t -> bool
type chooser   = topology -> Dyad.t -> Dyad.t list -> Dyad.t
type generator = topology -> (Dyad.t * Dyad.t) list -> (policy * policy)

exception UnmatchedDyad of Dyad.t

module type MAPPING = sig
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

module Make(M:MAPPING) : sig
  val synthesize : policy -> policy -> policy -> policy
end

module Optical : MAPPING
module Generic : MAPPING
