open VInt
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

let generate_random_graph num_hosts num_switches edge_prob = 
  let hosts = Array.init num_hosts (fun i -> Node.Host("","",string_of_int i)) in 
  let switches = Array.init num_switches (fun i -> Node.Switch("",Int64(Int64.of_int i))) in
  let g = Array.fold_left (fun g h -> Topology.add_node g h) Topology.empty hosts in
  let g = Array.fold_left (fun g s -> Topology.add_node g s) g switches in
  let graph = ref g in
  let gen_edges arr gr prob =
    let n = Array.length arr in
    for i = 0 to n-1 do
      for j = 0 to n-1 do
          let rand = Random.float 1. in
          if rand > edge_prob || i = j then
            ()
          else
            gr := Topology.add_edge_e (!gr) (Link.mk_link arr.(i) 0 arr.(j) 0 Int64.zero Int64.zero)
      done;
    done 
  in
  let gen_edges_btw arr1 arr2 gr prob =
    let n1 = Array.length arr1 and n2 = Array.length arr2 in
    for i = 0 to n1-1 do
      for j = 0 to n2-1 do
          let rand = Random.float 1. in
          if rand > edge_prob then
            ()
          else
            gr := Topology.add_edge_e (!gr) (Link.mk_link arr1.(i) 0 arr2.(j) 0 Int64.zero Int64.zero)
      done;
    done 
  in
  gen_edges hosts graph edge_prob;
  gen_edges switches graph edge_prob;
  gen_edges_btw hosts switches graph edge_prob;
  gen_edges_btw switches hosts graph edge_prob;
  !graph
  
let shortest_path_test g v1 v2 l= 
  let paths = Topology.floyd_warshall g in
  try
    let path = List.assoc (v1,v2) paths in
    if path = l then
      (print_endline "path matches exactly";
      true)
    else if List.length path = List.length l then
      (print_endline "path does not match exactly but both are the shortest";
      true)
    else
      (false)
  with Not_found -> 
    print_endline "vertices not found"; false

let edge_list_to_vertex_list el= 
  let rec helper el acc = match el with
      [(v1,_,v2)] -> List.rev (v2::v1::acc)
    | (v1,_,v2)::t -> helper t (v1::acc)
    | _ -> failwith "cannot happen" in
  helper el []

let test_n_times n num_hosts num_switches edge_prob =
  let count = ref n in
  let b = ref true in
  while !count > 0 do
    let g = generate_random_graph num_hosts num_switches edge_prob in
    let nodes = Array.of_list (Topology.get_vertices g) in
    let len = Array.length nodes in
    for i = 0 to len-1 do
      for j = 0 to len-1 do
        let v1 = nodes.(i) and v2 = nodes.(j) in
        let l = edge_list_to_vertex_list (Topology.shortest_path g v1 v2) in
        b := (!b) && (shortest_path_test g v1 v2 l);
      done;
    done;
    decr count;
  done; !b

TEST "empty topology" =
  (topo_test (Topology.empty) (Filter False))

TEST "host only" =
  (let g = (Topology.add_host (Topology.empty) ("") ("00:00:00:00:00:01") ("192.168.0.1")) in
  (topo_test g (Filter False)))

TEST "Switch only" = 
  (topo_test
    (Topology.add_switch (Topology.empty) ("") (Int64(Int64.of_int 1)))
    (Filter False))

TEST "Graph with no links" = 
  (topo_test
  (Topology.add_switch 
    (Topology.add_host (Topology.empty) ("") ("00:00:00:00:00:01") ("192.168.0.1"))
    ("") (Int64(Int64.of_int 1)))
  (Filter False))

TEST "test floyd_warshall works" = 
  (test_n_times 100 10 10 0.5)





