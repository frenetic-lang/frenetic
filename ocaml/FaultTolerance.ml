open NetCore
open Graph

let rec get_ports acts = match acts with
  | (To p) :: acts -> Some p :: get_ports acts
  | ToAll :: acts -> None :: get_ports acts
  | _ :: acts -> get_ports acts
  | [] -> []

let rec get_switches pred topo switches = match pred with
  | And (p1,p2) -> SwSet.inter (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Or (p1,p2) -> SwSet.union (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Not p1 -> SwSet.diff switches (get_switches p1 topo switches)
  | All -> switches
  | Switch sw -> SwSet.singleton sw
  | _ -> SwSet.empty

let rec get_links switches ports = match switches with
  | [] -> []
  | sw :: switches -> (List.map (fun p -> (sw,p)) ports) @ get_links switches ports

(* Given a netcore policy and a topology, we can compute the
   edges/switches that policy 'depends' upon. When a needed link goes
   down, the policy has failed *)
let rec dependent_links pol topo = match pol with
  | Par (p1, p2) -> (dependent_links p1 topo) @ (dependent_links p2 topo)
  | Pol (pred, acts) -> get_links (SwSet.elements (get_switches pred topo (get_nodes topo))) (get_ports acts)
