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

type decider = Frenetic_Fabric.stream -> Frenetic_Fabric.stream -> bool

val synthesize : ?approach:approach -> ?heuristic:heuristic ->
  policy -> policy -> policy -> policy
