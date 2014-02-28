open Network
open Physical

open OUnitHack
TEST "true" = true

TEST "single node" =
    let t = Parse.from_dotfile "test/dot/single_node.dot" in
    Printf.printf "\n%s\n" (Pretty.to_string t);
    (Topology.VertexSet.cardinal (Topology.vertexes t) = 1)
