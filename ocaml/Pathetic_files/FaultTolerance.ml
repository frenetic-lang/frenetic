open Pathetic.Regex
open OpenFlowTypes
open NetCoreFT

module G = Graph.Graph
module Q = Queue
module H = Hashtbl

let rec get_ports acts = match acts with
  | (To (_,p)) :: acts -> Some p :: get_ports acts
  | ToAll _ :: acts -> None :: get_ports acts
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

(* Stupid Ubuntu doesn't have OCaml 4, so List.mapi doesn't exist *)

let rec mapi' idx f lst =
  match lst with
    | [] -> []
    | l :: lst -> (f l idx) :: mapi' (idx + 1) f lst

let mapi f lst = mapi' 0 f lst

(* First pass computes the minimum number of failures required to reach each switch *)
let rec min_failures topo policies queue arr = 
  if Q.is_empty queue then arr
  else
    let (sw, current) = (Q.take queue) in
    let best = try (Pervasives.min (H.find arr sw) current) with _ -> current in
    if current >= best then arr else
      let nbrs = mapi (fun p count -> (G.next_hop topo sw p, count)) (from (H.find policies sw) best) in
      let () = H.add arr sw best;
	List.iter (fun (sw', count) -> Q.add (sw', best + count) queue) nbrs in
      min_failures topo policies queue arr

(* Second pass computes the minimum number of failures at a switch to prevent forwarding *)

module PtSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = G.b
  end)

let list_to_set lst =
  List.fold_left (fun acc sw -> PtSet.add sw acc) PtSet.empty lst

let rec min_port_failures switches policies arr = 
  match switches with 
    | [] -> arr
    | sw :: switches -> let () = H.add arr sw (PtSet.cardinal (list_to_set (H.find policies sw))) in
			min_port_failures switches policies arr

(* Third pass computes the sum of pass 1 and 2 and returns the min over all switches *)
let fault_tolerance topo policies = 
  let switches = (Graph.SwSet.elements (G.nodes topo)) in
  let arr1 = min_failures topo policies (Q.create ()) (H.create 5) in
  let arr2 = min_port_failures switches policies (H.create 5) in
  List.fold_left (fun acc sw -> Pervasives.min acc ((H.find arr1 sw) + (H.find arr2 sw))) Pervasives.max_int switches

(* Fault tolerance analysis implemented over a directed, rooted DAG *)

module Gensym =
struct
  let count = ref (Int32.of_int 0)
  let next () = count := Int32.succ !count; !count
end

let groups_to_string groups =
  String.concat ";\n" (List.map (fun (gid,_,acts) -> Printf.sprintf "\t%ld" gid) groups)

let add_group groups gid acts =
  groups := (gid, FF, acts) :: !groups

open NetCoreEval0x04
module NCE = NetCoreEval

let rec from n lst = match lst with
  | [] -> [] (* Throw error? *)
  | l :: lst -> if n <= 0 then l::lst
    else from (n - 1) lst

module Dag =
  struct
    type a = switchId
    type b = portId
    type dag = ((a, (int, b) H.t) H.t) * G.graph
    let install_link (d, g) topo sw sw' k =
      let p1,p2 = G.get_ports topo sw sw' in
      G.add_edge g sw p1 sw' p2;
      let portTbl = (try H.find d sw with _ -> H.create 5) in
      H.add portTbl k p1;
      H.add d sw portTbl

    let install_host_link (d, g) sw port k =
      let portTbl = (try H.find d sw with _ -> H.create 5) in
      H.add portTbl k port;
      H.add d sw portTbl

    let create () : dag = (H.create 5, G.create ())
    let rec insert i b lst = match lst with
      | (j, c) :: lst -> if j < i then
	  (j,c) :: insert i b lst
	else (i,b) :: (j,c) :: lst
      | [] -> [(i,b)]

    let next_hops (d,_) sw = 
      try snd (List.split (H.fold (fun idx port acc -> insert idx port acc) (H.find d sw) [])) with _ -> []
    let next_hop (_,g) sw p =
      let sw' = G.next_hop g sw p in
      let _,p' = G.get_ports g sw sw' in
      (sw',p')
    let intTbl_to_string intTbl =
      String.concat "" (H.fold (fun k p acc -> (Printf.sprintf "\t\t%d -> %ld\n" k p):: acc) intTbl [])
    let dag_to_string (d,_) = String.concat "" (H.fold (fun sw intTbl acc -> (Printf.sprintf "\t%Ld -> \n%s\n" sw (intTbl_to_string intTbl)):: acc) d [])
  end

(* Given an ordered k-resilient DAG, convert it into a k-resilient forwarding policy *)
(* pol = { sw |-> {n |-> (port, [port])} } *)

(* dag = rooted dag. 
   n = maximum resilience
   k = number of current active policy (0 = primary, 1 = secondary, etc)
*)

let sw_pol_to_string swPol = 
  String.concat "" (H.fold (fun k (inport, ports) acc -> (Printf.sprintf "\t%d -> (%ld, [%s])\n" k inport (String.concat ";\n" (List.map (Printf.sprintf "\t\t%ld") ports ))):: acc) swPol [])

let dag_pol_to_string pol =
  String.concat "" (H.fold (fun sw swPol acc -> (Printf.sprintf "%Ld -> \n%s\n" sw (sw_pol_to_string swPol)):: acc) pol [])

let rec dag_to_policy dag pol root inport pred k =
  (* Printf.printf "Entering dag_to_policy %s %s %Ld %ld\n"  *)
  (*   (Dag.dag_to_string dag)  *)
  (*   (dag_pol_to_string pol) *)
  (*   root *)
  (*   inport; *)
  let sw_pol = (try H.find pol root with 
      Not_found -> let foo = H.create 5 in H.add pol root foo; foo) in
  let children = from k (Dag.next_hops dag root) in
  let () = H.add sw_pol k (inport, children) in
  (* FIXME: Too coarse. Should just skip over elements causing exception *)
  let next_hops = try List.map (Dag.next_hop dag root) children with _ -> [] in
  List.iteri (fun idx (sw, port) -> dag_to_policy dag pol sw port pred (k - idx)) next_hops

let del_link topo sw sw' =
  let () = Printf.printf "Deleting link between %Ld and %Ld\n" sw sw' in
  let p1,p2 = G.get_ports topo sw sw' in
  G.del_edge topo sw p1;
  G.del_edge topo sw' p2

let rec k_dag_from_path path n k topo dag = match path with
  | (Hop sw, _) :: [(Host h1,_)] -> (match G.get_host_port topo h1 with
      | Some (s1,p1) -> assert (s1 = sw); Dag.install_host_link dag sw p1 k)
  | (Hop sw, a) :: (Hop sw', b) :: path -> 
    Dag.install_link dag topo sw sw' k;
    k_dag_from_path ((Hop sw',b) :: path) n k (G.copy topo) dag;
    del_link topo sw sw';
    for i = k + 1 to n do
      let path = expand_regex_with_match (snd (List.split path)) sw topo in
      k_dag_from_path path n i (G.copy topo) dag;
      let new_sw' = (match (List.hd (List.tl (fst (List.split path)))) with
	| Hop sw' -> sw) in
      del_link topo sw new_sw'
    done

let rec build_dag n topo regex = 
  let dag = Dag.create() in
  let path = expand_path_with_match regex topo in
  k_dag_from_path path n 0 (G.copy topo) dag;
  dag

let compile_ft_dict_to_nc1 pred swPol sw modif = 
  let groups = ref [] in
  (H.fold (fun k (inport,acts) acc -> 
    let gid = Gensym.next () in
    let acts' = List.mapi (fun i pt -> [To ({modif with NCE.modifyDlVlanPcp=Some(k+i)},pt)]) acts in
    let () = add_group groups gid acts' in
    Par (Pol (And (pred, (And (DlVlanPcp k, And (InPort inport, Switch sw)))), [NetCoreFT.Group gid]), acc)) swPol (Pol(All, [])), groups)

let compile_ft_dict_to_nc pred polTbl src dst vid = 
  let groupTbl = H.create 10 in
  (H.fold (fun sw swPol acc -> 
    let pred' = if sw = src then And (pred, DlVlan None) else And (pred, DlVlan (Some vid)) in
    let modif = if sw = dst then {unmodified with NCE.modifyDlVlan = (Some None)} else unmodified in
    let swPolNc, groups = compile_ft_dict_to_nc1 pred' swPol sw modif in
    H.add groupTbl sw !groups; Par (swPolNc, acc)) polTbl (Pol (All,[])),
   groupTbl)

let first = List.hd
let rec last lst = 
  match lst with
    | [l] -> l
    | a :: lst -> last lst

let rec compile_ft_regex pred vid regex k topo = 
  let Host srcHost = first regex in
  let Host dstHost = last regex in
  let host_expanded_regex = install_hosts regex topo in
  let srcSw,srcPort = (match G.get_host_port topo srcHost with Some (sw,p) -> (sw,p)) in
  let dstSw,dstPort = (match G.get_host_port topo dstHost with Some (sw,p) -> (sw,p)) in
  let dag = build_dag k topo regex in
  let () = Printf.printf "DAG: %s" (Dag.dag_to_string dag) in
  let dag_pol = H.create 10 in
  let () = dag_to_policy dag dag_pol srcSw srcPort pred k in
  let () = Printf.printf "DAG Policy: %s\n" (dag_pol_to_string dag_pol) in
  compile_ft_dict_to_nc pred dag_pol srcSw dstSw vid
    
let join_htbls h1 h2 op =
  let newHtbl = H.create (H.length h1) in
  let keys = (H.fold (fun a b acc -> a :: acc) h1 []) @ (H.fold (fun a b acc -> a :: acc) h2 []) in
  List.iter (fun a -> (match (H.mem h1 a, H.mem h2 a) with 
    | true,true -> H.replace newHtbl a (op (H.find h1 a) (H.find h2 a))
    | false,true -> H.replace newHtbl a (H.find h2 a)
    | true,false -> H.replace newHtbl a (H.find h1 a)
    | false,false -> () (* Impossible *))) keys;
  newHtbl
  
let group_htbl_to_str ghtbl =
    String.concat "" (H.fold (fun sw groups acc -> (Printf.sprintf "%Ld -> [\n%s]\n" sw (groups_to_string groups)):: acc) ghtbl [])

let rec compile_ft_to_nc regpol topo =
  match regpol with
    | RegUnion (p1,p2) -> let nc1,group1 = compile_ft_to_nc p1 topo in
			      Printf.printf "Group1: %s" (group_htbl_to_str group1);
			      let nc2, group2 = compile_ft_to_nc p2 topo in
			      Printf.printf "Group2: %s" (group_htbl_to_str group2);
			      let groups = join_htbls group1 group2 List.append in
			      Printf.printf "Groups: %s" (group_htbl_to_str groups);
			      (Par (nc1, nc2), groups)
    | RegPol (pred, path, k) -> let vid = Int32.to_int (Gensym.next ()) in
				      compile_ft_regex pred vid (flatten_reg path) k topo
