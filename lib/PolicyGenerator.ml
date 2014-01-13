open Printf

open Topology

type t = Topology.t

let hosts (g : t) : VInt.t list =
  List.rev
    (List.fold_left
       (fun acc node -> match node with
     | Node.Host _ -> (* TODO(basus): dummy value *) (VInt.Int32 0l)::acc
     | Node.Switch _ -> acc
     | Node.Mbox _ -> acc)
       [] (Topology.get_vertices g))

let switches (g : t) : SDN_Types.switchId list =
  List.rev
    (List.fold_left
       (fun acc node -> match node with
     | Node.Host _ -> acc
     | Node.Switch dpid -> dpid::acc
     | Node.Mbox _ -> acc)
       [] (Topology.get_vertices g))

(* let hop_to_pol (pred : predicate) (hop : node * int * portId * node) : policy = *)
(*   match hop with *)
(*     | (Mininet.Types.Switch swId, _, pt, _) -> *)
(*       Pol (And (pred, Switch swId), [To pt]) *)
(*     | _ -> Empty *)

let all_pairs_shortest_paths (g : t) =
  let open SDN_Types in
  let open NetKAT_Types in
  let all_paths = Topology.floyd_warshall g in
  List.fold_left
    (fun pol p ->
      match p with
        | ((Node.Host (src,_,_), Node.Host (dst,_,_)), []) ->
      pol
        | ((Node.Host (src,_,_), Node.Host (dst,_,_)), v::path) ->
      (* TODO(basus): replace with MAC address *)
      let src = VInt.Int48 0L in
      let dst = VInt.Int48 1L in
      let path_pol,_ =
        List.fold_left
          (fun (pol,v) v' ->
        match v with
          | Node.Switch dpid ->
            let (_,e,_) = Topology.find_edge g v v' in
            let pol' =
              Par(Seq(Filter(And(Test(Switch, dpid),
                     And(Test(Header EthSrc, src),
                                 Test(Header EthDst, dst)))),
                  Mod(Header InPort, e.Link.srcport)),
              pol) in
            (pol',v')
          | _ ->
            (pol,v'))
          (drop,v) path in
          Par(path_pol,pol)
        | _ -> pol)
    drop all_paths

(* generates a policy that 
  a) spans only the hosts in host_list (if empty, uses all hosts)
  b) avoids links in link_list (if empty, uses all links)
  c) is from the spanning tree is spanning_tree is true *)
let generate_policy (g:t) (host_list:Node.t list) (link_list: Link.e list) (spanning_tree: bool) = 
  let ll = Topology.get_edges g in
  let good_links = List.filter (fun x -> not (List.mem x link_list)) ll in
  let g = 
    if host_list = [] then g 
    else 
      let g = Topology.empty in
      List.fold_left (fun graph host -> Topology.add_node graph host) g host_list in
  let g = List.fold_left (fun graph link -> Topology.add_edge_e graph link) g good_links in
  let g = 
    if spanning_tree then 
      Topology.spanningtree g
    else g in
  all_pairs_shortest_paths g

(* (\** [g] must be a tree or this will freeze. *\) *)
(* let broadcast (g : G.t) (src : hostAddr) : policy =  *)
(*   let pred = And (DlSrc src, DlDst 0xFFFFFFFFFFFFL) in *)
(*   let rec loop parent vx = match vx with *)
(*     | Host _ -> Empty *)
(*     | Mininet.Types.Switch swId ->  *)
(*       let succs = List.filter (fun (ch, _) -> ch <> parent) (G.succs g vx) in *)
(*       let ports =  List.map (fun (_, pt) -> To pt) succs in *)
(*       let hop_pol = Pol (And (pred, Switch swId), ports) in *)
(*       par (hop_pol :: List.map (fun (succ_vx, _) ->  loop vx succ_vx) succs) in *)
(*   (\* Host 0L is a dummy value, but does not affect the policy *\) *)
(*   let host = Host src in *)
(*   par (List.map (fun (vx, _) -> loop host vx) (G.succs g host)) *)

(* let all_broadcast_trees (g : G.t) : policy =  *)
(*   let spanning_tree = G.prim g in *)
(*   par (List.map (broadcast spanning_tree) (hosts g)) *)
