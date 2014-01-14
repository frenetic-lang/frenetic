open OUnitHack
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
  let hosts = Array.init num_hosts
    (fun i -> Node.Host("h"^(string_of_int i),VInt.Int16 0, VInt.Int16 i)) in
  let switches = Array.init num_switches (fun i -> Node.Switch (VInt.Int16 i)) in
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
            gr := Topology.add_edge_e (!gr)
              (Link.mk_link arr.(i) (VInt.Int16 0) arr.(j) (VInt.Int16 0) (VInt.Int16 1) (VInt.Int16 1))
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
            gr := Topology.add_edge_e (!gr)
              (Link.mk_link arr1.(i) (VInt.Int16 0) arr2.(j) (VInt.Int16 0) (VInt.Int16 1) (VInt.Int16 1))
      done;
    done
  in
  gen_edges hosts graph edge_prob;
  gen_edges switches graph edge_prob;
  gen_edges_btw hosts switches graph edge_prob;
  gen_edges_btw switches hosts graph edge_prob;
  !graph

let spanningtree_test paths v1 v2 tree = 
  let paths = Topology.floyd_warshall tree in
  try 
    let path = List.assoc (v1,v2) paths in
    if List.length path > 0 
      then 
        true
      else
        false
  with Not_found ->
    Format.printf "Spanning tree is \n%s\n, and path from %s to %s is not found, expected to exist."
      (Topology.to_string tree) (Node.to_string v1) (Node.to_string v2);
      false

let edge_list_to_vertex_list el=
  let rec helper el acc = match el with
      [(v1,_,v2)] -> if v1 = v2 then [v1] else List.rev (v2::v1::acc)
    | (v1,_,v2)::t -> helper t (v1::acc)
    | [] -> acc in
  helper el []

let shortest_path_test g v1 v2 paths =
  let l = ref [] in 
  (try
    l := edge_list_to_vertex_list (Topology.shortest_path g v1 v2)
  with Not_found ->
    l := []);
  try
    let path = List.assoc (v1,v2) paths in
    if path = (!l) || List.length path = List.length (!l) then
      true
    else
      (let n1 = Node.to_string v1 in
      let n2 = Node.to_string v2 in
      let p1 = List.fold_left (fun acc v -> acc ^ "," ^ (Node.to_string v)) "" path in
      let p2 = List.fold_left (fun acc v -> acc ^ "," ^(Node.to_string v)) "" (!l) in
      Format.printf "Shortest path from %s to %s in graph \n%s\n Floyd_warshall produced \n%s\n Dijkstra produced \n%s\n"
        n1 n2 (Topology.to_string g) p1 p2;
        false)
  with Not_found ->
    print_endline "should not happen"; false

let test_n_times test_name n num_hosts num_switches edge_prob f2 =
  let count = ref n in
  let b = ref true in
  while !count > 0 do
    let g = generate_random_graph num_hosts num_switches edge_prob in
    let nodes = Array.of_list (Topology.get_vertices g) in
    let len = Array.length nodes in
    for i = 0 to len-1 do
      for j = 0 to len-1 do
        let v1 = nodes.(i) and v2 = nodes.(j) in
        if v1 != v2 then
            (let lst = f2 g in
            b := (!b) && (test_name g v1 v2 lst))
        else
          ()
      done;
    done;
    decr count;
  done; !b

TEST "empty topology" =
  (topo_test (Topology.empty) (Filter False))
(*
The result is Par(drop,drop) if I test for drop, and the result is drop if I test for Par(drop,drop). Not sure why.

TEST "host only" =
  (let g = (Topology.add_host (Topology.empty) ("") ("00:00:00:00:00:01") ("192.168.0.1")) in
  (topo_test g (Filter False)))

TEST "Switch only" =
  (topo_test
    (Topology.add_switch (Topology.empty) ("") (Int64(Int64.of_int 1)))
    (drop))

TEST "Graph with no links" =
  (topo_test
  (Topology.add_switch
    (Topology.add_host (Topology.empty) ("") ("00:00:00:00:00:01") ("192.168.0.1"))
    ("") (Int64(Int64.of_int 1)))
  (drop)) *)

TEST "test floyd_warshall works" =
  (test_n_times shortest_path_test 10 5 5 0.3 Topology.floyd_warshall)

TEST "Spanning Tree Sanity Check" = 
  (test_n_times spanningtree_test 10 3 3 1.0 Topology.spanningtree)
