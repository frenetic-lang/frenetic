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
type switchId = Frenetic_NetKAT.switchId
type portId = Frenetic_NetKAT.portId
type place = Frenetic_Fabric.place
type pred = Frenetic_NetKAT.pred

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

  module Waypath : sig

    (* A waypath connects transceiver ports on optical nodes on separate coasts
       using a predetermined physical path. The `waypoints` are at least the
       endpoints of the predetermined path. *)
    type t = { uid : int
             ; start : Topology.vertex * switchId * portId
             ; stop  : Topology.vertex * switchId * portId
             ; waypoints : Topology.vertex list
             ; path : CoroPath.t
             ; channel : int
             }

    type fiber = { uid : int
                 ; name : string
                 ; src : place
                 ; dst : place
                 ; condition : Frenetic_Fabric.Condition.t
                 ; action : Frenetic_Fdd.Action.t
                 ; points : switchId list
                 }

    type endtable = ((string * string), t list) Hashtbl.t

    val places : t -> place * place
    val vertexes : t -> Topology.vertex * Topology.vertex

    val of_coronet : Topology.t -> name_table -> port_table ->
      string list -> string list -> CoroPath.t list ->
      ( t list * endtable )

    val to_dyad : fiber -> Frenetic_Fabric.Dyad.t
    val to_circuit : t -> Topology.t -> circuit

    val to_policy : t -> Topology.t -> pred -> Frenetic_NetKAT.policy
    val to_policies : endtable -> Topology.t -> pred list ->
      Frenetic_NetKAT.policy list

    val to_fabric_fibers : t list -> Topology.t -> fiber list
    val to_policy_fibers : endtable -> Topology.t -> pred list -> fiber list

  end

  val string_of_path : path -> string
  val string_of_pathset : Topology.t -> pathset -> string

  val from_csv_file : string -> ( Topology.t  * name_table * port_table)
  val surround : Topology.t -> name_table -> port_table -> string list -> string
      list -> CoroPath.t list -> Topology.t * switchId list * place list * place list

  val cross_connect : Topology.t -> name_table -> string list -> string list ->
    pathset list

  val circuit_of_path : Topology.t ->
    ( Topology.vertex * Frenetic_NetKAT.portId ) ->
    ( Topology.vertex * Frenetic_NetKAT.portId ) ->
    path -> circuit

  val circuits_of_pathset : Topology.t ->
    Frenetic_NetKAT.portId -> Frenetic_NetKAT.portId ->
    pathset -> (circuit option * circuit option * circuit option)


end with type Topology.Vertex.t = CoroNode.t
     and type Topology.Edge.t = CoroLink.t


