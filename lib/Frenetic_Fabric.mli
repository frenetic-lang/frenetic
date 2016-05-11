open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow
open Frenetic_NetKAT

type fabric = (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t
type stream = policy * policy

val vlan_per_port : Net.Topology.t -> fabric
val shortest_path : Net.Topology.t -> switchId list -> switchId list -> fabric
val of_local_policy : policy -> switchId list -> fabric
val of_global_policy : policy -> switchId list -> fabric
val to_string : fabric -> string

val remove_dups : policy -> policy
val extract : policy ->
  (policy * policy) list
val assemble : policy -> policy ->
  (switchId * portId) list -> (switchId * portId) list ->
  policy
val print_partition : stream -> unit
val retarget : stream list -> stream list -> policy -> (policy list * policy list)
