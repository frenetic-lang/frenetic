open Graph
open Packet

type switchId = Core.switchId
type portId = Core.portId
type addrMAC = Core.addrMAC
type addrIP = Core.addrIP


module type NODE = Core.NODE
module type LINK = Core.LINK
module type TOPO = Core.TOPO


module Node : NODE
module Link : LINK with type v = Node.t
module Topology : TOPO with type V.t = Node.t and type E.t = Link.e
                       and type V.label = Node.label and type E.label = Link.t

val from_dotfile : string -> Topology.t
val from_gmlfile : string -> Topology.t
