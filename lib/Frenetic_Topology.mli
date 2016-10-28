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
module Distance : Frenetic_Network.WEIGHT

type name_table = (string, CoroNode.t) Hashtbl.t
type port_table = (string, SDN.portId) Hashtbl.t
type circuit = Frenetic_Circuit_NetKAT.circuit

module CoroNet : sig
  include Frenetic_Network.NETWORK

  exception NonexistentNode of string

  module CoroPath : PATH with type weight = Distance.t

  type path = CoroPath.t * int
  type pathset = { src : Topology.vertex
                 ; dst : Topology.vertex
                 ; shortest : path option
                 ; local    : path option
                 ; across   : path option
                 }
  type waypath = { path : CoroPath.t
                 ; start : Topology.vertex
                 ; stop  : Topology.vertex
                 ; waypoints : Topology.vertex list
                 ; channel : int
                 }

  val string_of_path : path -> string
  val string_of_pathset : Topology.t -> pathset -> string

  val from_csv_file : string -> ( Topology.t  * name_table * port_table)
  val surround : Topology.t -> name_table -> port_table -> string list -> string
      list -> CoroPath.t list -> Topology.t
  val cross_connect : Topology.t -> name_table -> string list -> string list ->
    pathset list
  val path_connect : Topology.t -> name_table -> port_table ->
    string list -> string list ->
    CoroPath.t list -> waypath list

  val circuit_of_path : Topology.t ->
    Topology.vertex -> Frenetic_NetKAT.portId ->
    Topology.vertex -> Frenetic_NetKAT.portId ->
    path -> circuit

  val circuits_of_pathset : Topology.t ->
    Frenetic_NetKAT.portId -> Frenetic_NetKAT.portId ->
    pathset -> (circuit option * circuit option * circuit option)

end with type Topology.Vertex.t = CoroNode.t
     and type Topology.Edge.t = CoroLink.t


