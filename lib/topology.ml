open Graph
open Packet


type switchId = Core.switchId
type portId = Core.portId
type addrMAC = Core.addrMAC
type addrIP = Core.addrIP

module type NODE = Core.NODE
module type LINK = Core.LINK
module type TOPO = Core.TOPO

module Node = Core.Node
module Link = Core.Link
module Topology = Core.Topology


let from_dotfile = Parsers.from_dotfile
let from_gmlfile = Parsers.from_gmlfile
