
type switchId = int64
type portId = int32
type rate = Rate of int64 * int64

module Node : Core.NODE
module Link : Core.LINK with type v = Node.t
module Topology : Core.TOPO with type V.t = Node.t and type E.t = Link.e and type V.label
  = Node.label and type E.label = Link.t

val from_dotfile : string -> Topology.t
val from_gmlfile : string -> Topology.t
