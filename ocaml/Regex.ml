open MessagesDef
open NetCoreFT
open Graph.Graph

(* type graph = (switchId * switchId * int) list *)


type regex =
  | Hop of switchId
  | Host of int
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

let get_path1 topo src dst = List.tl (get_path topo src dst)

let rec expand_path1 path hop topo = match path with
  | Star :: Hop s :: path -> get_path1 topo hop s @ expand_path1 path s topo
  | Hop s1 :: path -> Hop s1 :: expand_path1 path s1 topo
  | _ -> path

(* Input: path starting and ending with hosts
   Output: path starting and ending with hosts with no (Star, Host) (Host,Star) transitions 
*)
let rec install_hosts path topo = match path with
  | Host h1 :: Star :: [Host h2] -> 
    (match (get_host_port topo h1, get_host_port topo h2) with
      | (Some (s1,_), Some (s2,_)) -> if s1 = s2 then
	  Host h1 :: Hop s1 :: [Host h2]
	else
	  Host h1 :: Hop s1 :: Star :: Hop s2 :: [Host h2])
  | Host h1 :: Star :: path -> (match get_host_port topo h1 with
      | Some (s1,_) -> Host h1 :: Hop s1 ::  install_hosts (Star :: path) topo)
  | Star :: [Host h1] -> (match get_host_port topo h1 with
      | Some (s1,_) -> Hop s1 ::  [Host h1])
  | h :: path -> h :: install_hosts path topo
  | [] -> []

let rec expand_path path topo = match install_hosts path topo with
  | Host h1 :: Hop s1 :: path -> Host h1 :: Hop s1 :: expand_path1 path s1 topo

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
  | Hop s1 :: [Host h] -> (match get_host_port topo h with
      | Some (_,p1) ->  Pol ((And (pred, (And (InPort port,Switch s1)))), [To p1])
      | None -> Pol (((And (pred, (And (InPort port,Switch s1))))), [GetPacket (bad_hop_handler s1 (Int64.of_int h))]))
  | _ -> Pol (pred, [])

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg) -> (match (expand_path (collapse_star (flatten_reg reg)) topo) with
      | Host h :: Hop s :: reg -> (match get_host_port topo h with
	  (* assert s1 = 1 *)
	  | Some (s1,p) -> compile1 pred (Hop s :: reg) topo p))
  | RegPar (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)

let get_links pol topo = []

(* TODO: Figure out how to compile regex to a given fault tolerance level *)

let rec compile_regex_ft pol topo k = match pol with
  | RegPol (pred, reg) -> let pol' = compile_regex pol topo in
			  if k > 0 then let () = del_edges topo (get_links pol' topo) in
					(Par (pol', (compile_regex_ft pol topo (k - 1))))
			  else pol'
  | RegPar (pol1, pol2) -> Par (compile_regex_ft pol1 topo k, compile_regex_ft pol2 topo k)
