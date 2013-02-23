open MessagesDef
open NetCore
open Graph.Graph

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

let get_path topo s1 s2 = let path = shortest_path topo s1 s2 in
			  let () = Printf.printf "[regex] get_path %Ld %Ld:\n" s1 s2 in
			  let () =  List.iter (fun x -> Printf.printf "\t%Ld\n" x ) path in
			  List.map (fun x -> Hop x) path

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
    (match get_ports topo s1 s2 with
      | Some (p1,p2) ->  Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [To p1])), ((compile1 pred ((Hop s2) :: reg) topo p2)))
      | None -> Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [GetPacket (bad_hop_handler s1 s2)])), ((compile1 pred ((Hop s2) :: reg) topo port))))
  | Hop s1 :: Star :: Hop s2 :: reg -> compile1 pred (get_path topo s1 s2) topo port
  | Hop s1 :: [Host h] -> (match get_ports topo s1 h with
      | Some (p1,_) ->  Pol ((And (pred, (And (InPort port,Switch s1)))), [To p1])
      | None -> Pol (((And (pred, (And (InPort port,Switch s1))))), [GetPacket (bad_hop_handler s1 h)]))
  | _ -> Pol (pred, [])

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg) -> (match (collapse_star (flatten_reg reg)) with
      | Host h :: reg -> (match get_host_port topo h with
	  | Some p -> compile1 pred reg topo p))
  | RegPar (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)

(* TODO: Figure out how to compile regex to a given fault tolerance level *)

