open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

type approach =
  | Graphical
  | Synthesis

type heuristic =
  | Random of int * int
  | MaxSpread
  | MinSpread
  | MinConflict

type graph

val syngraph: policy -> policy -> graph
val synthesize : ?approach:approach -> ?heuristic:heuristic ->
  policy -> policy -> policy -> policy
