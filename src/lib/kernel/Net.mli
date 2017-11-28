open Core

module SDN = OpenFlow

type node =
  | Switch of SDN.switchId
  | Host of Packet.dlAddr * Packet.nwAddr [@@deriving sexp]

module Node: sig
  type t = node [@@deriving sexp]
end

module Link: Network.EDGE

module Net: Network.NETWORK
  with type Topology.Vertex.t = Node.t
  and type Topology.Edge.t = Link.t
