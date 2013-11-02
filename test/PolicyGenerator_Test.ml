open Types
open Topology
open PolicyGenerator

let topo_test g pol = 
  let pol' = all_pairs_shortest_paths g in 
  if pol = pol' then 
    true
  else
    begin
      Format.printf "Generating shortest paths from\n%s\nproduced %a\nexpected\n%a\n%!"
	(Topology.to_string g) Pretty.format_policy pol' Pretty.format_policy pol;
      false
    end
  

TEST "empty topology" =
  topo_test 
    Topology.empty 
    (Filter False)

