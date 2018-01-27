open! Core
open Frenetic.Network
open Syntax
open Symbolic

(*===========================================================================*)
(* TOPOLOGY PARSING & PROCESSING                                             *)
(*===========================================================================*)

(** Certain information about the topology is tedious to extract from the graph
    every time. Thus we compute it once and for all and cache it in a more
    accessible format.
*)
type enriched_topo = {
  graph : Frenetic.Network.Net.Topology.t;
  switch_tbl : Net.Topology.vertex Int.Table.t; (* sw id -> vertex *)
  edge_tbl : Net.Topology.edge Int2.Table.t;    (* src_sw,dst_sw -> edge *)
  hop_tbl : Net.Topology.edge Int2.Table.t;     (* src_sw,src_pt -> out edge *)
}

let enrich_topo (topo : Net.Topology.t) : enriched_topo =

  (* switch id to switch node map *)
  let switch_tbl : Net.Topology.vertex Int.Table.t =
    let open Net.Topology in
    let tbl = Int.Table.create () in
    iter_vertexes (fun v ->
      if Topology.is_switch topo v then
        let id = Topology.sw_val topo v in
        Hashtbl.add_exn tbl ~key:id ~data:v
    )
      topo;
    tbl
  in

  (* (src_sw,dst_sw) |-> edge *)
  let edge_tbl : Net.Topology.edge Int2.Table.t =
    let tbl = Int2.Table.create () in
    Net.Topology.iter_edges (fun edge ->
      let (src,_) = Net.Topology.edge_src edge in
      let (dst,_) = Net.Topology.edge_dst edge in
      if Topology.(is_switch topo src && is_switch topo dst) then
        let key = Topology.(sw_val topo src, sw_val topo dst) in
        Hashtbl.add_exn tbl ~key ~data:edge
    )
      topo;
    tbl
  in

  (* (sw,pt) |-> out edge *)
  let hop_tbl : Net.Topology.edge Int2.Table.t =
    let tbl = Int2.Table.create () in
    Net.Topology.iter_edges (fun edge ->
      let (src_sw, src_pt) = Net.Topology.edge_src edge in
      if Topology.is_switch topo src_sw then
        let key = Topology.(sw_val topo src_sw, pt_val src_pt) in
        Hashtbl.add_exn tbl ~key ~data:edge
      )
      topo;
    tbl
  in

  { graph = topo; switch_tbl; edge_tbl; hop_tbl; }




(*===========================================================================*)
(* ROUTING SCHEME PARSING & PROCESSING                                       *)
(*===========================================================================*)

let parse_sw sw =
  assert (String.get sw 0 = 's');
  String.slice sw 1 (String.length sw)
  |> Int.of_string

(* switch to port mapping *)
let parse_trees topo file : (int list) Int.Table.t =
  let tbl = Int.Table.create () in
  In_channel.(with_file file ~f:(iter_lines ~f:(fun l ->
    let l = String.strip l in
    if not (String.get l 0 = '#') then
    match String.split ~on:' ' l with
    | [src; "--"; dst] ->
      let src = parse_sw src in
      let dst = parse_sw dst in
      let edge = Hashtbl.find_exn topo.edge_tbl (src,dst) in
      let (_, out_port) = Net.Topology.edge_src edge in
      Hashtbl.add_multi tbl ~key:src ~data:(Topology.pt_val out_port)
    | _ ->
      failwith "unexpected format"
  )));
(*   Hashtbl.iteri tbl ~f:(fun ~key:sw ~data:pts ->
    printf "sw %d: %s\n" sw (List.to_string pts ~f:Int.to_string)
  ); *)
  tbl

(* switch to port mapping *)
let parse_nexthops topo file : (int list) Int.Table.t =
  let tbl = Int.Table.create () in
  In_channel.(with_file file ~f:(iter_lines ~f:(fun l ->
    let l = String.strip l in
    if not (String.get l 0 = '#') then
    match String.split ~on:' ' l with
    | src::":"::dsts ->
      let src = parse_sw src in
      List.map dsts ~f:(fun dst ->
        let dst = parse_sw dst in
        let edge = Hashtbl.find_exn topo.edge_tbl (src,dst) in
        let (_, out_port) = Net.Topology.edge_src edge in
        Topology.pt_val out_port
      )
      |> fun data -> Hashtbl.add_exn tbl ~key:src ~data
    | _ ->
      failwith "unexpected format"
  )));
  tbl

open Params

(* switch to distance-to-destination-in-a-tree mapping *)
let mk_distance_in_tree_tbl topo port_tbl : (int list) Int.Table.t =
  let port_list = Hashtbl.to_alist port_tbl in
  (* map from a switch to it's distance from destination in the tree *)
  let dist_map = Int.Table.create () in
  let num_trees = List.length (List.hd_exn (Hashtbl.data port_tbl)) in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  List.iter (range 0 num_trees) ~f:(fun tree_idx ->
    (* map from a switch to it's previous switch in the tree *)
    let prev = Int.Table.create () in
    List.iter ~f:(fun (src_sw, out_pts) ->
      let src_pt = List.nth_exn out_pts tree_idx in
      let edge = Hashtbl.find_exn topo.hop_tbl (src_sw, src_pt) in
      let (dst_sw, _) = Net.Topology.edge_dst edge in
      Hashtbl.add_multi prev ~key:Topology.(sw_val topo.graph dst_sw) ~data:src_sw) port_list;
    let rec traverse nodes level =
      if nodes = [] then () else
      let next_nodes = List.fold_left ~init:[] ~f:(fun acc sw ->
        Hashtbl.add_multi dist_map ~key:sw ~data:level;
        match Hashtbl.find prev sw with
        | Some srcs -> srcs@acc
        | None -> acc) nodes in
      traverse next_nodes (level + 1) in
    traverse [destination] 0);
  (* Hashtbl.iteri dist_map ~f:(fun ~key:sw ~data:dists ->
    printf "sw %d: %s\n" sw (List.to_string dists ~f:Int.to_string)
  ); *)
  dist_map

(* switch to index-of-tree-with-shortest-distance-to-dest mapping *)
let mk_shortest_tree_tbl topo port_tbl : int Int.Table.t =
  let dist_tbl = mk_distance_in_tree_tbl topo port_tbl in
  let tbl = Int.Table.create () in
  let min_idx l =
      let rec findmin start_i start_x i l = match l with
        | [] -> (start_i, start_x)
        | x::xs -> if x < start_x then findmin i x (i+1) xs else findmin start_i start_x (i+1) xs in
      match l with
        | [] -> failwith "Empty list"
        | _ -> findmin 0 Int.max_value 0 l in
  Hashtbl.iteri dist_tbl ~f:(fun ~key:sw ~data:dists ->
    let shortest_tree_idx,_ = min_idx dists in
    Hashtbl.add_exn tbl ~key:sw ~data:shortest_tree_idx);
  (* Hashtbl.iteri tbl ~f:(fun ~key:sw ~data:idx ->
    printf "sw %d: %d\n" sw idx
  ); *)
  tbl

(* am I at a good port? *)
let at_good_pt sw pts = PNK.(
  List.map pts ~f:(fun pt_val -> ???(pt,pt_val) & ???(up sw pt_val, 1))
  |> mk_big_disj
)

(* given a current switch and the inport, what tree are we on? *)
let mk_current_tree_tbl topo (port_tbl : (int list) Int.Table.t)
        (shortest_tree_idx_tbl : int Int.Table.t) : int Int2.Table.t =
  let tbl = Int2.Table.create () in
  (* the port map maps a switch to the out_ports in order of the tree preference *)
  Hashtbl.iteri port_tbl ~f:(fun ~key:src_sw ~data:src_pts ->
    List.iteri src_pts ~f:(fun i src_pt ->
      (* if we are on tree i, we go from src_sw to src_pt across the following edge: *)
      let edge = Hashtbl.find_exn topo.hop_tbl (src_sw, src_pt) in
      (* thus, we would end up at the following switch: *)
      let (dst_sw, dst_pt) = Net.Topology.edge_dst edge in
      (* thus, we can infer from entering switch `dst_sw` at port `dst_pt` that
         we must be on tree i
      *)
      let key = Topology.(sw_val topo.graph dst_sw, Topology.pt_val dst_pt) in
      Hashtbl.add_exn tbl ~key ~data:i
    )
  );
  (* for ingress ports, start at tree with shortest distance to destination *)
  List.iter (Topology.ingress_locs topo.graph ~dst:destination) ~f:(fun (sw, pt_val) ->
    let key = (Topology.sw_val topo.graph sw, pt_val) in
    let data = Hashtbl.find_exn shortest_tree_idx_tbl Topology.(sw_val topo.graph sw) in
    Hashtbl.add_exn tbl ~key ~data
  );
  tbl



(*===========================================================================*)
(* ROUTING SCHEMES                                                           *)
(*===========================================================================*)

let random_walk topo : Net.Topology.vertex -> string policy =
  (* current switch |-> routing program *)
  fun sw ->
    Topology.vertex_to_ports topo.graph sw ~dst_filter:(Topology.is_switch topo.graph)
    |> List.map ~f:(fun out_pt_id -> PNK.( !!(pt, Topology.pt_val out_pt_id) ))
    |> PNK.uniform

let resilient_random_walk topo : Net.Topology.vertex -> string policy =
  fun sw ->
    let pts =
      Topology.(vertex_to_ports topo.graph sw ~dst_filter:(is_switch topo.graph))
      |> List.map ~f:Topology.pt_val
    in
    let choose_port = random_walk topo sw in
    PNK.( do_whl (neg (at_good_pt sw pts)) choose_port )

let shortest_path topo base_name : Net.Topology.vertex -> string policy =
  let port_tbl = parse_trees topo (Params.spf_file base_name) in
  fun sw ->
    let sw_val = Topology.sw_val topo.graph sw in
    match Hashtbl.find port_tbl sw_val with
    | Some (pt_val::_) -> PNK.( !!(pt, pt_val) )
    | _ ->
      eprintf "switch %d cannot reach destination\n" sw_val;
      failwith "network disconnected!"

let ecmp topo base_name : Net.Topology.vertex -> string policy =
  let port_tbl = parse_nexthops topo (Params.ecmp_file base_name) in
  fun sw ->
    let sw_val = Topology.sw_val topo.graph sw in
    match Hashtbl.find port_tbl sw_val with
    | Some pts -> PNK.(
        List.map pts ~f:(fun pt_val -> !!(pt, pt_val))
        |> uniform
      )
    | _ ->
      eprintf "switch %d cannot reach destination\n" sw_val;
      failwith "network disconnected!"

let resilient_ecmp topo base_name : Net.Topology.vertex -> string policy =
  let port_tbl = parse_nexthops topo (Params.ecmp_file base_name) in
  fun sw ->
    let sw_val = Topology.sw_val topo.graph sw in
    match Hashtbl.find port_tbl sw_val with
    | Some pts -> PNK.(
        do_whl (neg (at_good_pt sw pts)) (
          List.map pts ~f:(fun pt_val -> !!(pt, pt_val))
          |> uniform
        )
      )
    | _ ->
      eprintf "switch %d cannot reach destination\n" sw_val;
      failwith "network disconnected!"

let f10 topo base_name : Net.Topology.vertex -> int -> string policy =
  let port_tbl = parse_nexthops topo (Params.ecmp_file base_name) in
  fun sw in_pt ->
    let sw_val = Topology.sw_val topo.graph sw in
    match Hashtbl.find port_tbl sw_val with
    | Some pts -> (
      match pts with
      | [] ->
         eprintf "switch %d doesn't have a forwading entry\n" sw_val;
         failwith "network disconnected!"
      | [dnwd_pt] ->
          (* Going downwards, if this port is up, forward. Else, find all
          downward going ports to other type subtree, and select a random port,
          from this list, which is up. (Scheme 1) *)
        let alt_dnwd_pts = Topology.vertex_to_ports topo.graph sw ~dst_filter:(fun neigh ->
          Topology.(is_switch topo.graph neigh) && Topology.(sw_val topo.graph neigh) < sw_val)
          |> List.map ~f:Topology.pt_val
          |> List.filter ~f:(fun pv -> pv%2 <> dnwd_pt%2) in
        PNK.(Ite (???(up sw_val dnwd_pt, 1), !!(pt, dnwd_pt),
                (List.map alt_dnwd_pts ~f:(fun pt_val ->
                  !!(pt, pt_val)) |> uniform)))
      | hd::tl -> PNK.(
          do_whl (disj (neg (at_good_pt sw pts)) ???(pt, in_pt) ) (
            List.map pts ~f:(fun pt_val -> !!(pt, pt_val))
            |> uniform
          )
        )
      )
    | _ ->
      eprintf "switch %d cannot reach destination\n" sw_val;
      failwith "network disconnected!"


let car topo base_name ~(style: [`Deterministic|`Probabilistic])
  : Net.Topology.vertex -> int -> string policy =
  let port_tbl = parse_trees topo (Params.car_file base_name) in
  let shortest_tree_idx_tbl = mk_shortest_tree_tbl topo port_tbl in
  let current_tree_tbl = mk_current_tree_tbl topo port_tbl shortest_tree_idx_tbl in
  let port_tbl = Int.Table.map port_tbl ~f:Array.of_list in
  (* (current switch, inport) |-> routing policy *)
  fun sw in_pt ->
    let sw_val = Topology.sw_val topo.graph sw in
    match Hashtbl.find current_tree_tbl (sw_val, in_pt) with
    | None ->
      (* eprintf "verify that packets never enter switch %d at port %d\n" sw_val in_pt; *)
      PNK.( drop )
    | Some i ->
      (* ports correspondiong to trees *)
      (* pts.(i) = out port corresponding to tree i *)
      let pts = Hashtbl.find_exn port_tbl sw_val in
      begin match style with
      | `Deterministic ->
        let n = Array.length pts in
        (* the order in which we should try ports, i.e. starting from i *)
        let pts = Array.init n (fun j -> pts.((i+j) mod n)) in
        PNK.(
          Array.to_list pts
          |> ite_cascade ~otherwise:drop ~f:(fun pt_val ->
              let guard = ???(up sw_val pt_val, 1) in
              let body = !!(pt, pt_val) in
              (guard, body)
            )
        )
      | `Probabilistic ->
        failwith "not implemented"
      end

type scheme = [
  | `Switchwise of Net.Topology.vertex -> string policy
  | `Portwise  of Net.Topology.vertex -> int -> string policy
]

let get_all topo base_name : (string * scheme) list =
  let topo = enrich_topo topo in
  let random_walk () = `Switchwise (random_walk topo) in
  let resilient_random_walk () = `Switchwise (resilient_random_walk topo) in
  let shortest_path () = `Switchwise (shortest_path topo base_name) in
  let ecmp () = `Switchwise (ecmp topo base_name) in
  let resilient_ecmp () = `Switchwise (resilient_ecmp topo base_name) in
  let car () = `Portwise (car topo base_name ~style:`Deterministic) in
  [ "random walk",            random_walk;
    "resilient random walk",  resilient_random_walk;
    "shortest path",          shortest_path;
    "ecmp",                   ecmp;
    "resilient ecmp",         resilient_ecmp;
    "car",                    car;
  ]
  |> List.filter_map ~f:(fun (name, make) ->
    match make () with
    | scheme -> Some (name, scheme)
    | exception _ -> None
  )
