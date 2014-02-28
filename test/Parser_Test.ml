open Network
open Physical

open OUnitHack
TEST "true" = true

TEST "single node" =
    let t = Parse.from_dotfile "test/dot/single_node.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 1)
