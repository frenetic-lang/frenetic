open NetCore

module G = Graph.Graph
module Q = Queue
module H = Hashtbl

let rec get_ports acts = match acts with
  | (To p) :: acts -> Some p :: get_ports acts
  | ToAll :: acts -> None :: get_ports acts
  | _ :: acts -> get_ports acts
  | [] -> []

let rec get_switches pred topo switches = match pred with
  | And (p1,p2) -> Graph.SwSet.inter (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Or (p1,p2) -> Graph.SwSet.union (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Not p1 -> Graph.SwSet.diff switches (get_switches p1 topo switches)
  | All -> switches
  | Switch sw -> Graph.SwSet.singleton sw
  | _ -> Graph.SwSet.empty

let rec get_links switches ports topo = match switches with
  | [] -> []
  | sw :: switches -> (List.map (fun p -> (sw,p)) ports) @ get_links switches ports topo

let normalize_link sw1 p1 sw2 p2 = 
  if sw1 > sw2 then ((sw1, p1), (sw2, p2))
  else if sw2 > sw1 then ((sw2, p2), (sw1, p1))
  else if p1 > p2 then ((sw1, p1), (sw2, p2))
  else ((sw2, p2), (sw1, p1))

(*
  Given a netcore policy and a topology, we can compute the
  edges/switches that policy 'depends' upon. When a needed link goes
  down, the policy has failed
*)

let rec dependent_links pol topo = match pol with
  | Par (p1, p2) -> (dependent_links p1 topo) @ (dependent_links p2 topo)
  | Pol (pred, acts) -> get_links (Graph.SwSet.elements (get_switches pred topo (G.nodes topo))) (get_ports acts) topo

(* Todo: given a list of NC policies, determine the minimum number of link failures to cause the policies to fail *)

(* 
   Let's simplify and assume that we have a single policy for a single
   flow. We represent the policies as a mapping from switches to ports,
   specifying the next hop from that switch 
*)
(* 
   Algorithm: BFS across the topology. At each node, calculate the
   minimum number of failures required to prevent forwarding. This number
   is the sum of the minimum number of failures required to reach this
   node + min number of port failures such that no policies are
   available 
*)

let rec from lst n = match (n, lst) with
  | 0,_ -> lst
  | n,[] -> raise (Invalid_argument (Pervasives.string_of_int n))
  | n, l::lst -> from lst (n - 1)

let min_num_port_failures sw pols = 0

(* First pass computes the minimum number of failures required to reach each switch *)
let rec min_failures topo policies queue arr = 
  if Q.is_empty queue then arr
  else
    let (sw, current) = (Q.take queue) in
    let best = try (Pervasives.min (H.find arr sw) current) with _ -> current in
    if current >= best then arr else
      let nbrs = List.mapi (fun p count -> (G.next_hop topo sw p, count)) (from (H.find policies sw) best) in
      let () = H.add arr sw best;
	List.iter (fun (sw', count) -> Q.add (sw', best + count) queue) nbrs in
      min_failures topo policies queue arr

(* Second pass computes the minimum number of failures at a switch to prevent forwarding *)

(* Third pass computes the sum of pass 1 and 2 and returns the min over all switches *)
