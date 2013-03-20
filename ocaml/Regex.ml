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
  | RegPol of predicate * regex * int
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

let bad_hop_handler s1 s2 sw pt pk =
  Printf.printf "Can not forward pkt from %Ld to %Ld\n" s1 s2
 
let rec compile_path1 pred path topo port = match path with
  | Hop s1 :: Hop s2 :: path -> 
    let p1,p2 = get_ports topo s1 s2 in
    Par ((Pol ((And (pred, (And (InPort port,Switch s1)))), [To (unmodified, p1)])), ((compile_path1 pred ((Hop s2) :: path) topo p2)))
  | Hop s1 :: [Host h] -> (match get_host_port topo h with
      | Some (_,p1) ->  Pol ((And (pred, (And (InPort port,Switch s1)))), [To ({unmodified with NetCoreEval.modifyDlVlan=(Some None)}, p1)])
      | None -> Pol (((And (pred, (And (InPort port,Switch s1))))), [GetPacket (bad_hop_handler s1 (Int64.of_int h))]))
  | _ -> Pol (pred, [])

let compile_path pred path topo vid = match path with
  | Host h1 :: Hop s :: [Host h2] -> (match (get_host_port topo h1, get_host_port topo h2) with
	  (* assert s1 = s *)
      | (Some (s1,p1), Some (s2,p2)) -> Pol ((And (pred, (And (InPort p1,Switch s)))), [To (unmodified, p2)]))
  | Host h :: Hop s1 :: Hop s2 :: path -> (match get_host_port topo h with
	  (* assert s1 = s *)
	  | Some (s1,inport) -> let p1,p2 = get_ports topo s1 s2 in
				let pol = Pol (And (pred, (And (InPort inport,Switch s1))), [To ({unmodified with NetCoreEval.modifyDlVlan=(Some (Some vid))}, p1)]) in
				Par (pol, compile_path1 (And (DlVlan (Some vid), pred)) (Hop s2 :: path) topo p2))



module Gensym =
struct
  let count = ref 0
  let next () = incr count; !count
end

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg, _) -> compile_path pred (expand_path (collapse_star (flatten_reg reg)) topo) topo (Gensym.next ())
  | RegPar (pol1, pol2) -> Par (compile_regex pol1 topo, compile_regex pol2 topo)

let rec del_links path topo = match path with
  | Host h :: path -> del_links path topo
  | Hop s1 :: Hop s2 :: path -> let p1,p2 = get_ports topo s1 s2 in
				del_edge topo s1 p1; del_edge topo s2 p2; del_links path topo
  | Hop s :: [Host h] -> ()

let get_links _ _ = []
