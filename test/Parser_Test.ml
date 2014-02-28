open Network
open Physical

open OUnitHack
TEST "true" = true

TEST "single node" =
    let t = Parse.from_dotfile "test/dot/single_node.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 1)

TEST "single edge" =
    let t = Parse.from_dotfile "test/dot/single_edge.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 2) &&
      (Topology.EdgeSet.cardinal (Topology.edges t) = 1)
