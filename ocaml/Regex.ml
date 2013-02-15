open NetCore
open MessagesDef

(* type graph = (switchId * switchId * int) list *)

module H = Hashtbl
module Q = Queue

exception NoPath of string*string


let rec bfs' target graph queue =
  let (sw, path) = (Q.take queue) in
  match sw = target with
    | true -> path
    | false -> let () = List.iter (fun x -> Q.add (x, x :: path) queue) (try (H.find graph sw) with _ -> []); H.remove graph sw in
	       bfs' target graph queue

let bfs graph src dst = 
  let q = Queue.create () in
  let () = Q.add (src, [src]) q in
  try (bfs' dst (Hashtbl.copy graph) q) 
  with Queue.Empty -> raise (NoPath(Int64.to_string src, Int64.to_string dst))

let shortest_path = bfs

type regex =
  | Hop of switchId
  | Star
  | Option of regex * regex
  | Sequence of regex * regex

type regex_policy = 
  | RegPol of predicate * regex
  | RegPar of regex_policy * regex_policy

module SwSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = MessagesDef.switchId
  end)


let get_hop topo s1 s2 = try Some (Hashtbl.find (Hashtbl.find (snd topo) s1) s2) with _ -> None

let get_path topo s1 s2 = List.map (fun x -> Hop x) (shortest_path (fst topo) s1 s2)

let rec flatten_reg pol = match pol with
  | Hop sw -> [Hop sw]
  | Star -> [Star]
  (* | Option reg1 reg2 -> [Option reg1 reg2] *)
  | Sequence (reg1, reg2) -> (flatten_reg reg1) @ (flatten_reg reg2)

let rec collapse_star pol = match pol with
  | Star :: Star :: pol -> collapse_star (Star :: pol)
  | a :: pol -> a :: collapse_star pol
  | [] -> []

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

let rec compile1 pred reg topo = match reg with
  | Hop s1 :: Hop s2 :: reg -> 
    (match get_hop topo s1 s1 with
      | Some p ->  Par ((Pol ((And (pred, (Switch s1))), [To p])), ((compile1 pred ((Hop s2) :: reg) topo)))
      | None -> Par ((Pol ((And (pred, (Switch s1))), [GetPacket (bad_hop_handler s1 s2)])), ((compile1 pred ((Hop s2) :: reg) topo))))
  | Hop s1 :: Star :: Hop s2 :: reg -> compile1 pred (get_path topo s1 s2) topo
  | _ -> Pol (pred, [])

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg) -> compile1 pred (collapse_star (flatten_reg reg)) topo
  | RegPar (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)

let rec get_ports acts = match acts with
  | (To p) :: acts -> Some p :: get_ports acts
  | ToAll :: acts -> None :: get_ports acts
  | _ :: acts -> get_ports acts
  | [] -> []

let get_nodes topo = SwSet.empty

let rec get_switches pred topo switches = match pred with
  | And (p1,p2) -> SwSet.inter (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Or (p1,p2) -> SwSet.union (get_switches p1 topo switches) (get_switches p2 topo switches)
  | Not p1 -> SwSet.diff switches (get_switches p1 topo switches)
  | All -> switches
  | Switch sw -> SwSet.singleton sw
  | _ -> SwSet.empty

(* let rec get_links switches ports = match switches with *)
(*   | sw :: switches  *)

(* (\* Given a netcore policy and a topology, we can compute the *)
(*    edges/switches that policy 'depends' upon. When a needed link goes *)
(*    down, the policy has failed *\) *)
(* let rec dependent_links pol topo = match pol with *)
(*   | Par (p1, p2) -> (dependent_links p1 topo) @ (dependent_links p2 topo) *)
(*   | Policy (pred, acts) -> get_links (get_switches pred topo (get_nodes topo)) (get_ports acts) *)
