open Core
open Netkat.Syntax

module Net = Frenetic.Network.Net
module Topology = Net.Topology
type topo = Net.Topology.t * Topology.VertexSet.t * Topology.VertexSet.t *
  Topology.VertexSet.t

val parse_topo_file : ?log:bool -> ?hostlimit:int -> string -> topo
val shortest_paths : topo -> policy
val shortest_paths_global_policy : topo -> policy

val big_switch : topo:topo -> policy * pred * policy * policy * pred * policy * pred
val print_topo : string -> topo -> string list -> unit
