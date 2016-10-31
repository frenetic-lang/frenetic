open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow
open Frenetic_Fabric

exception UnmatchedDyad of Dyad.t

type topology = {
  topo  : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type 'a decider   = topology -> 'a -> 'a -> bool
type 'a chooser   = topology -> 'a -> 'a list -> 'a
type 'a generator = topology -> ('a * 'a) list -> (policy * policy)

module type SOLVER = sig
  type t
  val decide   : t decider
  val choose   : t chooser
  val generate : t generator
end

module type DYAD_SOLVER = SOLVER with type t = Dyad.t

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

  val mk_dyad_decider : ?preamble:string -> restraint -> Dyad.t decider
end

module MakeForDyads(S:DYAD_SOLVER) : SYNTH

module Optical : DYAD_SOLVER
module Generic : DYAD_SOLVER
