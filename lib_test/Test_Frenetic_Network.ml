open Frenetic_Network.Net

let%test "single node" =
    let t = Parse.from_dotfile "lib_test/data/single_node.dot" in
    (Topology.VertexSet.length (Topology.vertexes t) = 1)

let%test "single edge" =
    let t = Parse.from_dotfile "lib_test/data/single_edge.dot" in
    (Topology.VertexSet.length (Topology.vertexes t) = 2) &&
      (Topology.EdgeSet.length (Topology.edges t) = 1)

let%test "single bidirectional edge" =
    let t = Parse.from_dotfile "lib_test/data/single_biedge.dot" in
    (Topology.VertexSet.length (Topology.vertexes t) = 2) &&
      (Topology.EdgeSet.length (Topology.edges t) = 2)

let%test "double hop" =
    let t = Parse.from_dotfile "lib_test/data/double_hop.dot" in
    (Topology.VertexSet.length (Topology.vertexes t) = 3) &&
      (Topology.EdgeSet.length (Topology.edges t) = 2)

let%test "double bidirectional hop" =
    let t = Parse.from_dotfile "lib_test/data/double_bihop.dot" in
    (Topology.VertexSet.length (Topology.vertexes t) = 3) &&
      (Topology.EdgeSet.length (Topology.edges t) = 4)
