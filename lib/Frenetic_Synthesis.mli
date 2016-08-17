open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

type heuristic =
  | Graphical
  | Synthesis
  | Distance

type graph

val syngraph: policy -> policy -> graph
val synthesize : ?heuristic:heuristic -> policy -> policy -> policy -> policy
