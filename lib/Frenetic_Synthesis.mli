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

module type SYNTH = sig
  val synthesize : policy -> policy -> policy -> policy
end


module Z3 : sig

  open Frenetic_Fdd

  exception Inconvertible of string

  type source = Policy | Fabric

  type restraint =
    | Subset
    | Adjacent
    | Tests        of source * Field.t
    | TestsAll     of source * FieldSet.t
    | TestsOnly    of source * FieldSet.t
    | TestsExactly of source * FieldSet.t
    | Not of restraint
    | And of restraint * restraint
    | Or  of restraint * restraint

  val mk_decider : ?prereqs_file:string -> restraint -> decider
end

module Make(S:SOLVER) : SYNTH

module Optical : SOLVER
module Generic : SOLVER
