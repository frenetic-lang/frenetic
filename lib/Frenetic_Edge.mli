open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow
open Frenetic_Fabric

type fiber = Frenetic_Topology.CoroNet.Waypath.fiber
type assemblage = Assemblage.t

type path  = Dyad.t * ( place list )
type spread = (Dyad.t list * int )

type topology = {
  topo  : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type timings = (string * Int64.t) list
type result = policy * timings

(** Module for dealing with policies specified as end-to-end paths directly, *)
(** instead of as NetKAT programs *)
module Path : sig
  type t = pred * place list

  val of_string : string -> (t list, string) Result.t

end

module type SYNTHESIZER = sig
  type input
  type solution
  val synthesize : input -> input -> policy -> solution
end

module type MATCHER = SYNTHESIZER with
  type input = Dyad.t list and type solution = result

module type SEGMENTER = SYNTHESIZER with
  type input = path list and type solution = result

module type DIVERSIFIER = SYNTHESIZER with
  type input = spread list and type solution = result

module GraphicalMatching : MATCHER

module LinearMatching : MATCHER

module GraphicalSegmenting : SEGMENTER

module LinearSegmenting : SEGMENTER

(* module GraphicalDiverse : DIVERSIFIER *)

module LinearDiverse : SEGMENTER
