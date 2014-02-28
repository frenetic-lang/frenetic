open Network
open Network_Physical.Net

open OUnitHack
TEST "true" = true

TEST "single node" =
    let t = Parse.from_dotfile "test/dot/single_node.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 1)

TEST "single edge" =
    let t = Parse.from_dotfile "test/dot/single_edge.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 2) &&
      (Topology.EdgeSet.cardinal (Topology.edges t) = 1)

TEST "single bidirectional edge" =
    let t = Parse.from_dotfile "test/dot/single_biedge.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 2) &&
      (Topology.EdgeSet.cardinal (Topology.edges t) = 2)

TEST "double hop" =
    let t = Parse.from_dotfile "test/dot/double_hop.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 3) &&
      (Topology.EdgeSet.cardinal (Topology.edges t) = 2)

TEST "double bidirectional hop" =
    let t = Parse.from_dotfile "test/dot/double_bihop.dot" in
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 3) &&
      (Topology.EdgeSet.cardinal (Topology.edges t) = 4)
