(* Topology utility functions. This module should eventually be replaced with a
 * Frenetic-specific topology module that includes the ocaml-topology module.
 *)

open Core
module SDN = OpenFlow
module Net : module type of Net.Net

val switch_ids : Net.Topology.t -> SDN.switchId list

(* Topology detection doesn't really detect hosts. So, I treat any
   port not connected to a known switch as an edge port *)
val internal_ports : Net.Topology.t -> SDN.switchId -> Net.Topology.PortSet.t

val in_edge : Net.Topology.t -> SDN.switchId -> SDN.portId -> bool

val edge : Net.Topology.t -> (SDN.switchId * SDN.portId) list

module Mininet: sig
  type topo_name =
    | Tree of int * int
    | Linear of int
    | Single of int
    | Minimal

  val topo_from_name: topo_name -> Network.Net.Topology.t
end