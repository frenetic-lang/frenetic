open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow

type fabric = (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t

val vlan_per_port : Net.Topology.t -> fabric
val shortest_path : Net.Topology.t -> switchId list -> switchId list -> fabric
val of_local_policy : Frenetic_NetKAT.policy -> switchId list -> fabric
val of_global_policy : Frenetic_NetKAT.policy -> switchId list -> fabric
val to_string : fabric -> string
