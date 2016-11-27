open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow
open Frenetic_Fabric

type fiber = Frenetic_Topology.CoroNet.Waypath.fiber
type assemblage = Assemblage.t

exception UnmatchedDyad of Dyad.t
exception UnmatchedFiber of fiber

type topology = {
  topo  : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type timings = (string * Int64.t) list
type result = policy * timings

type 'a decider   = topology -> 'a -> 'a -> bool
type 'a chooser   = topology -> 'a -> 'a list -> 'a
type 'a generator = topology -> ('a * 'a) list -> (policy * policy)

module type COMPARATOR = sig
  type t
  val decide   : t decider
  val choose   : t chooser
  val generate : t generator
end

module type SYNTHESIZER = sig
  type input
  type solution
  val synthesize : input -> input -> policy -> solution
end

module type DYADIC = SYNTHESIZER with
  type input = Dyad.t list and type solution = result

module MakeStrict(C:COMPARATOR) : SYNTHESIZER with
  type input = C.t list and type solution = result

module Coronet : SYNTHESIZER with
  type input = fiber list and type solution = result Async.Std.Deferred.t

module Gurobi : SYNTHESIZER with
  type input = Dyad.t list and type solution = result

module Optical : COMPARATOR with type t = Dyad.t
module Generic : COMPARATOR with type t = Dyad.t

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
