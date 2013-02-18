open MessagesDef
open NetCore
open Graph

(* type graph = (switchId * switchId * int) list *)


type regex =
  | Hop of switchId
  | Host of switchId
  | Star
  | Option of regex * regex
  | Sequence of regex * regex

type regex_policy = 
  | RegPol of predicate * regex
  | RegPar of regex_policy * regex_policy



let rec flatten_reg pol = match pol with
  | Hop sw -> [Hop sw]
  | Host sw -> [Host sw]
  | Star -> [Star]
  (* | Option reg1 reg2 -> [Option reg1 reg2] *)
  | Sequence (reg1, reg2) -> (flatten_reg reg1) @ (flatten_reg reg2)

let rec collapse_star pol = match pol with
  | Star :: Star :: pol -> collapse_star (Star :: pol)
  | a :: pol -> a :: collapse_star pol
  | [] -> []

let get_path topo s1 s2 = List.map (fun x -> Hop x) (shortest_path (fst topo) s1 s2)

  (* Naive compilation: does not guarantee loop-free semantics
     Possible issues:
     1) reg contains an explicit loop
     2) We compile star paths to contain the same node

     Possible solutions:
     1) Second compilation phase that detects repeated nodes and tags packets inbetween such repeats
  *)

(*
  Semantic issues: we should compile to use 'port' predicates. Otherwise we screw up the directionality of the rules. Also, this allows us to have paths connecting across a node without allowing hosts on that nodes to communicate w/ the path endpoints.
*)

let bad_hop_handler s1 s2 sw pt pk =
  Printf.printf "Can not forward pkt from %Ld to %Ld\n" s1 s2
 
let rec compile1 pred reg topo port = match reg with
  | Hop s1 :: Hop s2 :: reg -> 
    (match get_hop topo s1 s2 with
      | Some p ->  Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [To p])), ((compile1 pred ((Hop s2) :: reg) topo port)))
      | None -> Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [GetPacket (bad_hop_handler s1 s2)])), ((compile1 pred ((Hop s2) :: reg) topo port))))
  | Hop s1 :: Star :: Hop s2 :: reg -> compile1 pred (get_path topo s1 s2) topo port
  | Hop s1 :: [Host h] -> (match get_hop topo s1 h with
      | Some p ->  Pol ((And (pred, (And (InPort port,Switch s1)))), [To p])
      | None -> Pol (((And (pred, (And (InPort port,Switch s1))))), [GetPacket (bad_hop_handler s1 h)]))
  | _ -> Pol (pred, [])

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg) -> (match (collapse_star (flatten_reg reg)) with
      | Host h :: reg -> (match get_host_port topo h with
	  | Some p -> compile1 pred reg topo p))
  | RegPar (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)

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
