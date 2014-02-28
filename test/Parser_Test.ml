open Network

open OUnitHack
TEST "true" = true

TEST "single node" =
    let t = Physical.Parse.from_dotfile "test/dot/single_node.dot" in
    Printf.printf "Parsed\n";
    Printf.printf "\n%s\n" (Physical.Pretty.to_string t);
    false
