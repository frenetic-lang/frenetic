
type switchId = int64
type portId = int32
type rate = Rate of int64 * int64

module Node = Core.Node
module Link = Core.Link
module Topology = Core.Topology


let from_dotfile = Parsers.from_dotfile
let from_gmlfile = Parsers.from_gmlfile
