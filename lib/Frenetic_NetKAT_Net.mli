open Core.Std

module SDN = Frenetic_OpenFlow

type node =
  | Switch of SDN.switchId
  | Host of Frenetic_Packet.dlAddr * Frenetic_Packet.nwAddr [@@deriving sexp]

module Node: sig
  type t = node [@@deriving sexp]
end

module Link: Frenetic_Network.EDGE

module Net: Frenetic_Network.NETWORK 
  with type Topology.Vertex.t = Node.t
  and type Topology.Edge.t = Link.t 
