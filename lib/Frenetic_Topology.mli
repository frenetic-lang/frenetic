(* Topology utility functions. This module should eventually be replaced with a
 * Frenetic-specific topology module that includes the ocaml-topology module.
 *)

open Core.Std

module Net = Frenetic_NetKAT_Net.Net
module SDN = Frenetic_OpenFlow

val switch_ids : Net.Topology.t -> SDN.switchId list

(* Topology detection doesn't really detect hosts. So, I treat any
   port not connected to a known switch as an edge port *)
val internal_ports : Net.Topology.t -> SDN.switchId -> Net.Topology.PortSet.t

val in_edge : Net.Topology.t -> SDN.switchId -> SDN.portId -> bool

val edge : Net.Topology.t -> (SDN.switchId * SDN.portId) list

module CoroNode : Frenetic_Network.VERTEX
module CoroLink : Frenetic_Network.EDGE
module CoroNet : sig
  include Frenetic_Network.NETWORK
  val from_csv_file : string -> ( Topology.t  * (string, Frenetic_NetKAT.switchId) Hashtbl.t)
end
