open MessagesDef
open OpenFlow0x04Parser
open OpenFlow0x04Types
open PatternImplDef
open NetCoreEval0x04
open NetCoreCompiler0x04
open Classifier
open List

module W = Wildcard

let rec mapi' idx f lst = match lst with
  | elm :: lst -> f idx elm :: mapi' (idx+1) f lst
  | [] -> []

let mapi f lst = mapi' 0 f lst

let compile_nc = fun p sw -> fst (compile_opt p sw)

module Gensym =
struct
  let count = ref (Int32.of_int 0)
  let next () = count := Int32.succ !count; !count
end

let add_group groups gid a b =
  groups := (gid, FF, [a;b]) :: !groups

let rec compile_pb pri crsovr bak sw =
  let pri_tbl = compile_nc pri sw in
  let bak_tbl = compile_nc bak sw in
  let crsovr_tbl = compile_nc crsovr sw in
  let groups = ref [] in
  let merge pri_acts bak_acts = 
    (let gid = Gensym.next () in
     add_group groups gid pri_acts bak_acts;
     [Group gid ]) in
  let ft_tbl = inter merge pri_tbl crsovr_tbl in
  (ft_tbl @ pri_tbl @ bak_tbl, !groups)

(* Given an ordered k-resilient DAG, convert it into a k-resilient forwarding policy *)

module G = Graph.Graph

(* dag = rooted dag. 
   n = maximum resilience
   k = number of current active policy (0 = primary, 1 = secondary, etc)
*)

let rec from n lst = match lst with
  | [] -> [] (* Throw error? *)
  | l :: lst -> if n <= 0 then l::lst
    else from (n - 1) lst

(* pol = { sw |-> {n |-> (port, [port])} } *)
      
(* Naive algorithm. Computes a shortest path p. Deletes the links in p, then recurses down p, computing another shortest path from there. *)

(* let rec get_legal_path regex src dst topo *)

module Dag =
  struct
    module H = Hashtbl
    type a = switchId
    type b = portId
    type dag = ((a, (int, b) H.t) H.t) * G.graph
    let install_link (d, g) topo sw sw' k =
      match (G.get_ports topo sw sw') with
	| Some (p1, p2) -> G.add_edge g sw p1 sw' p2;
	  let portTbl = (try H.find d sw with _ -> H.create 5) in
	  H.add portTbl k p1;
	  H.add d sw portTbl
    let create () : dag = (H.create 5, G.create ())
  end


let del_link topo sw sw' =
  match G.get_ports topo sw sw' with
  | Some (p1,p2) -> G.del_edge topo sw p1;
    G.del_edge topo sw' p2

let rec k_dag_from_path path dst n k topo dag = match path with
  | [sw] -> ()
  | sw :: sw' :: path -> 
    Dag.install_link dag topo sw sw' k;
    k_dag_from_path path dst n k topo dag;
    del_link topo sw sw';
    for i = k + 1 to n do
      (* path should not include current node *)
      let path = tl (G.shortest_path topo sw dst) in
      k_dag_from_path path dst n i topo dag;
      del_link topo sw (hd path)
    done

let rec build_dag src dst n topo = 
  let dag = Dag.create() in
  let path = G.shortest_path topo src dst in
  k_dag_from_path path dst n 0 topo dag;
  dag

(* Builds an (n-k) resilient dag rooted at src, ending at dst *)  

