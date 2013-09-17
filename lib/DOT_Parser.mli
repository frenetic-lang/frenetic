open DOT_Types
open Topology

val parse_dotfile : string -> dotgraph
val topo_from_ast : dotgraph -> Topology.t
