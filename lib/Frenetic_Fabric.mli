open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow0x01

type fabric = (switchId, Frenetic_OpenFlow0x01.flowMod list) Hashtbl.t

val vlan_per_port : Net.Topology.t -> fabric
val shortest_path : Net.Topology.t -> switchId list -> switchId list -> fabric
