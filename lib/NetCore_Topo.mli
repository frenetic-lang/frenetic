type switchId = NetCore_Types.switchId
type portId = NetCore_Types.portId
type dlAddr = Packet.dlAddr

module type Arg = sig

  val dl_typ : Packet.dlTyp

end

module type TOPO = sig

  val create : (switchId * portId * Packet.bytes -> unit Lwt.t) 
    -> NetCore_Types.pol NetCore_Stream.t * unit Lwt.t

  val graph : NetCore_Graph.Graph.graph

  val ports_of_switch : switchId -> portId list

  val edge_ports_of_switch : switchId -> portId list

  val get_switches : unit -> switchId list

end

module Make : functor (A : Arg) -> TOPO

(* uses 0x7FF for discovery packets *)
module Topo : TOPO
