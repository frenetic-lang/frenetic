open Core.Std
open Frenetic_Network
open Frenetic_ProbNetKAT_Interpreter
open Net
open Num
open Topology

module PQueue = Core_kernel.Heap.Removable
include Interp(Hist)(PreciseProb)

open Pol

module VertexOrd = struct
  type t = Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module VertexMap = Map.Make(VertexOrd)

module EdgeOrd = struct
  type t = Topology.edge  [@@deriving sexp]
  let compare = Pervasives.compare
end

module EdgeMap = Map.Make(EdgeOrd)

module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex [@@deriving sexp]
  let compare = Pervasives.compare
end

module SrcDstMap = Map.Make(SrcDstOrd)

type path = Topology.edge list [@@deriving sexp]

module PathOrd = struct
  type t = path [@@deriving sexp]
  let compare = Pervasives.compare
end

module PathMap = Map.Make(PathOrd)

type flow_decomp = num PathMap.t
type scheme = flow_decomp SrcDstMap.t

module PortMap = Map.Make(Int)
type out_port_dist = num PortMap.t
type link_state_scheme = out_port_dist SrcDstMap.t

module PktSet = Set.Make(Pkt)


(* helper functions *)

let get_opt_exn v =
  match v with
  | None -> failwith "None"
  | Some x -> x

let string_of_vertex (t:Topology.t) v : string =
  Printf.sprintf "%s" (Node.name (Topology.vertex_to_label t v))

let string_of_edge (t:Topology.t) e : string =
  Printf.sprintf "(%s,%s)"
                (string_of_vertex t (fst (Topology.edge_src e)))
                (string_of_vertex  t (fst (Topology.edge_dst e)))

let intercalate f s = function
  | [] ->
      ""
  | h::t ->
      List.fold_left t ~f:(fun acc x -> acc ^ s ^ f x) ~init:(f h)

let dump_links (t:Topology.t) (es:path) : string =
  intercalate
    (fun e ->
     Printf.sprintf "(%s,%s)"
        (Node.name (Topology.vertex_to_label t (fst (Topology.edge_src e))))
        (Node.name (Topology.vertex_to_label t (fst (Topology.edge_dst e))))) ", "  es

let dump_path_prob_set (t:Topology.t) (pps: flow_decomp) : string =
  let buf = Buffer.create 101 in
  PathMap.iteri
    pps
    ~f:(fun ~key:path ~data:prob ->
      Printf.bprintf buf "[%s] @ %f\n" (dump_links t path) (float_of_num prob));
  Buffer.contents buf

let dump_scheme (t:Topology.t) (s:scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri s
    ~f:(fun ~key:(v1,v2) ~data:pps ->
      Printf.bprintf buf "%s -> %s :\n  %s\n"
          (Node.name (Topology.vertex_to_label t v1))
          (Node.name (Topology.vertex_to_label t v2))
          (dump_path_prob_set t pps));
  Buffer.contents buf

let dump_port_prob_set (opd: out_port_dist) : string =
  let buf = Buffer.create 101 in
  IntMap.iteri
    opd
    ~f:(fun ~key:port ~data:prob ->
      Printf.bprintf buf "[%d] @ %f\n" port (float_of_num prob));
  Buffer.contents buf

let dump_link_state_scheme (t:Topology.t) (s:link_state_scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri s
    ~f:(fun ~key:(v1,v2) ~data:opd ->
      Printf.bprintf buf "%s -> %s :\n  %s\n"
          (Node.name (Topology.vertex_to_label t v1))
          (Node.name (Topology.vertex_to_label t v2))
          (dump_port_prob_set opd));
  Buffer.contents buf

let get_hosts_set (topo : Topology.t) : VertexSet.t =
  VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Host)

let remove_cycles path =
  let link_dst t = fst (Topology.edge_dst t) in
  let link_src t = fst (Topology.edge_src t) in
  let path_segment path v = match path with
        | [] -> []
        | h::t ->
            fst (List.fold_right path ~init:([], false) ~f:(fun next (acc,over) ->
              if over then (acc,over)
              else
                let src = link_src next in
                let dst = link_dst next in
                if src = v then (acc,true)
                else if dst = v then (next::acc, true) else (next::acc, false))) in
  let rec check path acc seen = match path with
        | [] -> acc
        | h::t ->
            let dst = link_dst h in
            if Topology.VertexSet.mem seen dst then
              (* cut out the cycle *)
              let segment = path_segment acc dst in
              let first_node = match segment with
              | [] -> Topology.VertexSet.empty
              | h::t -> Topology.VertexSet.singleton (link_src h) in
              let new_seen = List.fold_left segment ~init:first_node ~f:(fun acc next ->
                Topology.VertexSet.add acc (link_dst next)) in
              check t segment new_seen
                else
                  check t (h::acc) (Topology.VertexSet.add seen dst) in
  match path with
        | [] -> []
        | h::t -> let first_src = (link_src h) in
        List.rev (check path [] (Topology.VertexSet.singleton first_src))


let k_shortest_path (topo : Topology.t) (s : Topology.vertex) (t : Topology.vertex) (k : int) =
  (* k-shortest s,t paths - loops can be present *)
  if s = t then [] else
  let paths = ref [] in
  let count = Hashtbl.Poly.create () in
  Topology.iter_vertexes
    (fun u ->
     Hashtbl.Poly.add_exn count u 0;
    ) topo;

  let bheap = PQueue.create
                  ~min_size:(Topology.num_vertexes topo)
                  ~cmp:(fun (dist1,_) (dist2,_) -> Float.compare dist1 dist2) () in

  let _ = PQueue.add_removable bheap (0.0, [s]) in
  let rec explore () =
    let cost_path_u = PQueue.pop bheap in
    match cost_path_u with
    | None -> ()
    | Some (cost_u, path_u) ->
        match List.hd path_u with (* path_u contains vertices in reverse order *)
        | None -> ()
        | Some u ->
          let count_u = Hashtbl.Poly.find_exn count u in
          let _  = Hashtbl.Poly.set count u (count_u + 1) in
          if u = t then paths := List.append !paths [path_u];
          if count_u < k then
            (* if u = t then explore()
            else *)
            let _ = Topology.iter_succ
            (fun link ->
              let (v,_) = Topology.edge_dst link in
              if (List.mem path_u v) then () else (* consider only simple paths *)
              let path_v = v::path_u in
              let weight = Link.weight (Topology.edge_to_label topo link) in
              let cost_v = cost_u +. weight in
              let _ = PQueue.add_removable bheap (cost_v, path_v) in
              ()) topo u in
            explore ()
          else if u = t then ()
          else explore () in
  explore ();
  List.fold_left
    !paths
    ~init:[]
    ~f:(fun acc path ->
      let link_path = List.fold_left path
      ~init:([], None)
      ~f:(fun links_u v ->
        let links, u = links_u in
        match u with
        | None -> (links, Some v)
        | Some u -> let link = Topology.find_edge topo v u in (link::links, Some v)) in
      let p,_ = link_path in
      (remove_cycles p)::acc)

let all_pair_ksp (topo : Topology.t) (k : int) src_set dst_set =
  VertexSet.fold src_set
  ~init:SrcDstMap.empty
  ~f:(fun acc src ->
        VertexSet.fold dst_set ~init:acc
        ~f:(fun nacc dst ->
          let ksp = k_shortest_path topo src dst k in
          SrcDstMap.add nacc ~key:(src, dst) ~data:ksp))

(******************* All pair multiple shortest paths *************************)
let abs_fl (n:float) =
  if n > 0.0 then n else -.n

let rec dp_calc_numpaths (i:Topology.vertex) (j:Topology.vertex) (topo:Topology.t)
(dist: float SrcDstMap.t) (numpath: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t)
: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
  let  (v, n, l) = SrcDstMap.find_exn numpath (i,j) in
  let n_numpath = SrcDstMap.add numpath ~key:(i,j) ~data:(true, n, l) in
  let neigh = Topology.neighbors topo i in
  (* For each vertex *)
  let n3_numpath = Topology.fold_vertexes
    (fun nextHop acc ->
      if (nextHop=i) then acc
      else if not (VertexSet.mem neigh nextHop) then acc
      else
        (* consider it as next hop if neighbor *)
        let d_ih = SrcDstMap.find_exn dist (i,nextHop) in
        let d_hj = SrcDstMap.find_exn dist (nextHop,j) in
        let d_ij = SrcDstMap.find_exn dist (i,j) in
        if (abs_fl(d_ih +. d_hj -. d_ij) < 0.00001) then
          (* if it is in a shortest i-j path *)
          let t_visited,_,_ = SrcDstMap.find_exn acc  (nextHop,j) in
          let n2_numpath = if t_visited then acc
            else dp_calc_numpaths nextHop j topo dist acc  in
          let _,np_nj,_ = SrcDstMap.find_exn n2_numpath (nextHop,j) in
          let _,np_ij,l_ij = SrcDstMap.find_exn n2_numpath (i,j) in
          let t_np_ij = np_ij + np_nj in
          SrcDstMap.add n2_numpath
            ~key:(i,j)
            ~data:(true, t_np_ij, List.append l_ij [(nextHop, Float.of_int np_nj)])
        else acc)
    topo n_numpath in
  let _,normalizer,l_ij = SrcDstMap.find_exn n3_numpath (i,j) in
  let path_probs,_ = List.fold_left l_ij
    ~init:([], 0.0)
    ~f:(fun acc (nextHop, np_h) ->
      let l, preprob = acc in
      let n_prob = np_h /. Float.of_int normalizer in
      (List.append l [(nextHop, preprob +. n_prob)], preprob +. n_prob)) in
  SrcDstMap.add n3_numpath ~key:(i,j) ~data:(true, normalizer, path_probs)


let all_pairs_multi_shortest_path (topo:Topology.t) : (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
  (* topology -> (visited_bool, normalizer, list(next_hop, prob)) SrcDstMap *)
  let dist_mat = Topology.fold_vertexes
    (fun i dist_mat -> Topology.fold_vertexes
        (fun j dist_mat2 ->
          let ans = if (i=j) then 0.0
          else Float.infinity in
          SrcDstMap.add dist_mat2 ~key:(i,j) ~data:ans)
        topo
        dist_mat)
    topo
    SrcDstMap.empty in
  let dist_mat_init = Topology.fold_edges
  (fun e acc ->
    let src,_ = Topology.edge_src e in
    let dst,_ = Topology.edge_dst e in
    let weight = Link.weight (Topology.edge_to_label topo e) in
    SrcDstMap.add acc ~key:(src, dst) ~data:weight)
  topo dist_mat in
  let dist_mat_sp = Topology.fold_vertexes
    (fun k acc ->
      Topology.fold_vertexes
        (fun i acc_i ->
          Topology.fold_vertexes
          (fun j acc_j ->
            let dij  = SrcDstMap.find_exn acc_j (i,j)  in
            let dik  = SrcDstMap.find_exn acc_j (i,k)  in
            let dkj  = SrcDstMap.find_exn acc_j (k,j)  in
            let upd_val = if (dik +. dkj < dij) then dik +. dkj else dij in
            SrcDstMap.add acc_j ~key:(i,j) ~data:upd_val)
          topo acc_i)
        topo acc)
    topo dist_mat_init in
  (* initialize visited to be true for i,i *)
  let init_vis_npath_pathlist_map = Topology.fold_vertexes
    (fun i acc_i ->
      Topology.fold_vertexes
        (fun j acc_j ->
          let visited = if (i=j) then true else false in
          let num_paths = if (i=j) then 1 else 0 in
          SrcDstMap.add acc_j ~key:(i,j) ~data:(visited,num_paths,[]))
        topo acc_i)
      topo SrcDstMap.empty in
  let numPath_map = Topology.fold_vertexes
    (fun i acc_i ->
      Topology.fold_vertexes
        (fun j acc_j ->
          let visited,np,_ = SrcDstMap.find_exn acc_j (i,j) in
          if (visited) then acc_j
          else dp_calc_numpaths i j topo dist_mat_sp acc_j)
        topo acc_i)
    topo init_vis_npath_pathlist_map in
  numPath_map


(********************************** Topology **********************************)
(* Check if a node is a host *)
let is_host (topo : Topology.t) (node : vertex) =
  Node.device (vertex_to_label topo node) = Node.Host

let is_switch (topo : Topology.t) (node : vertex) =
  Node.device (vertex_to_label topo node) = Node.Switch

let gen_node_pnk_id_map (topo : Topology.t) =
  let host_offset =
    Int.of_float (10. ** (Pervasives.ceil (log10 (Float.of_int (num_vertexes topo))) +. 1.)) in
  VertexSet.fold (Topology.vertexes topo)
    ~init:(VertexMap.empty, IntMap.empty)
    ~f:(fun (v_id, id_v) v ->
      let node_name = Node.name (vertex_to_label topo v) in
      let regexp = Str.regexp "\\([A-Za-z]+\\)\\([0-9]+\\)" in
      let _ = Str.string_match regexp node_name 0 in
      let offset = if is_host topo v then host_offset else 0 in
      let node_id = offset + int_of_string (Str.matched_group 2 node_name) in
      (VertexMap.add v_id ~key:v ~data:node_id, IntMap.add id_v ~key:node_id ~data:v))

let port_to_pnk port =
    match Int32.to_int port with
    | None -> failwith "Invalid port"
    | Some x -> x

(* Topology to ProbNetKAT program *)
let topo_to_pnk (topo : Topology.t) (v_id, id_v) =
    Topology.fold_edges (fun link pol_acc ->
    let src, src_port = Topology.edge_src link in
    let dst, dst_port = Topology.edge_dst link in
    let src_id = VertexMap.find_exn v_id src in
    let dst_id = VertexMap.find_exn v_id dst in
    let link_src_test = ??(Switch src_id) >> ??(Port (port_to_pnk src_port)) in
    let link_dest_mod =  !!(Switch dst_id) >> !!(Port (port_to_pnk dst_port)) in
    let link_prog = link_src_test >> Dup >> link_dest_mod >> Dup in
    if pol_acc = Drop then
      link_prog
    else
      link_prog & pol_acc)
    topo Drop

(* Topology with lossy links to ProbNetKAT program *)
let topo_to_pnk_lossy (topo : Topology.t) (fail_prob : Prob.t) (v_id, id_v) =
    Topology.fold_edges (fun link pol_acc ->
    let src, src_port = Topology.edge_src link in
    let dst, dst_port = Topology.edge_dst link in
    let src_id = VertexMap.find_exn v_id src in
    let dst_id = VertexMap.find_exn v_id dst in
    let link_src_test = ??(Switch src_id) >> ??(Port (port_to_pnk src_port)) in
    let link_dest_mod =  !!(Switch dst_id) >> !!(Port (port_to_pnk dst_port)) in
    let link_prog = link_src_test >> Dup >>
      ?@[(link_dest_mod >> Dup, Prob.(one - fail_prob));
        (Drop, fail_prob)] in
    if pol_acc = Drop then
      link_prog
    else
      link_prog & pol_acc)
    topo Drop


(* ProbNetKAT program to test if a packet is at a host *)
let test_pkt_at_host (topo : Topology.t) (v_id, id_v) =
  let host_set = get_hosts_set topo in
  VertexSet.fold host_set
    ~init:Drop
    ~f:(fun pol_acc v ->
      let host_id = VertexMap.find_exn v_id v in
      pol_acc & ??(Switch host_id))

(* ProbNetKAT program to test if a packet is at network edge (host & port) *)
let test_pkt_at_edge (topo : Topology.t) (v_id, id_v) =
  let host_set = get_hosts_set topo in
  VertexSet.fold host_set
    ~init:Drop
    ~f:(fun acc v ->
      let host_id = VertexMap.find_exn v_id v in
      VertexSet.fold (Topology.neighbors topo v)
        ~init:acc
        ~f:(fun acc neigh ->
          let link = Topology.find_edge topo v neigh in
          let port_id = port_to_pnk (snd (Topology.edge_src link)) in
          let test_pol = (??(Switch host_id) >> ??(Port port_id)) in
          if acc = Drop then test_pol
          else acc & test_pol))


(****************************** Routing Schemes *******************************)
(************ Global Path based *******************)
(* Topology -> SPF scheme *)
let route_spf (topo : Topology.t) : scheme =
  let device v = vertex_to_label topo v
    |> Node.device in
  let apsp = NetPath.all_pairs_shortest_paths
    ~topo:topo
    ~f:(fun x y ->
        match (device x, device y) with
        | (Node.Host,Node.Host) -> true
        | _ -> false) in
  List.fold_left apsp
    ~init:SrcDstMap.empty
    ~f:(fun acc (c,v1,v2,p) ->
      SrcDstMap.add acc ~key:(v1,v2) ~data:(PathMap.singleton p Prob.one))

(* Compute routing scheme with k shortest paths between hosts *)
let route_ksp (topo : Topology.t) (k : int) : scheme =
  let host_set = get_hosts_set topo in
  let all_ksp = all_pair_ksp topo k host_set host_set in
  SrcDstMap.fold all_ksp ~init:SrcDstMap.empty
    ~f:(fun ~key:(v1,v2) ~data:paths acc ->
      if (v1 = v2) then acc
      else
      let path_map = List.fold_left
          paths
          ~init:PathMap.empty
          ~f:(fun acc path ->
              let prob = Prob.(one / of_int (List.length paths)) in
              PathMap.add acc ~key:path ~data:prob) in
      SrcDstMap.add acc ~key:(v1,v2) ~data:path_map)

(************ Local switch based *******************)
(* First outgoing port for a path *)
let get_out_port path =
  match path with
  | link::_ -> (snd (Topology.edge_src link))
  | [] -> failwith "Empty edge list"

(* Topology -> link state scheme *)
let route_link_state_spf (topo : Topology.t) : link_state_scheme =
  let device v = vertex_to_label topo v
    |> Node.device in
  let apsp = NetPath.all_pairs_shortest_paths
    ~topo:topo
    ~f:(fun x y ->
        match (device x, device y) with
        | (_, Node.Host) -> true
        | _ -> false) in
  List.fold_left apsp
    ~init:SrcDstMap.empty
    ~f:(fun acc (c,v1,v2,p) ->
      if v1 = v2 then acc
      else SrcDstMap.add acc ~key:(v1,v2)
        ~data:(PortMap.singleton (port_to_pnk (get_out_port p)) Prob.one))

let route_link_state_ecmp (topo:Topology.t) : link_state_scheme =
  let mpapsp = all_pairs_multi_shortest_path topo in
  SrcDstMap.fold mpapsp
  ~init:SrcDstMap.empty
  ~f:(fun ~key:(src, dst) ~data:(_,n,next_hop_probs) acc ->
    if src = dst then
      acc
    else
      let next_hops = List.fold_left next_hop_probs
        ~init:VertexSet.empty
        ~f:(fun acc (next_hop,_) ->
          VertexSet.add acc next_hop) in
      let prob = Prob.(one / of_int (VertexSet.length next_hops)) in
      let out_ports = VertexSet.fold next_hops
        ~init:PortMap.empty
        ~f:(fun acc next_hop ->
          let link = Topology.find_edge topo src next_hop in
          let out_port = port_to_pnk(snd (Topology.edge_src link)) in
          PortMap.add acc ~key:out_port ~data:prob) in
      SrcDstMap.add acc ~key:(src, dst) ~data:out_ports)

(* Compute link state routing scheme with k shortest paths from a node to a host *)
let route_link_state_ksp (topo : Topology.t) (k : int) : link_state_scheme =
  let host_set = get_hosts_set topo in
  let all_nodes_set = Topology.vertexes topo in
  let all_ksp = all_pair_ksp topo k all_nodes_set host_set in
  SrcDstMap.fold all_ksp ~init:SrcDstMap.empty
    ~f:(fun ~key:(v1,v2) ~data:paths acc ->
      if (v1 = v2) then acc
      else
        let prob = Prob.(one / of_int (List.length paths)) in
        let port_map = List.fold_left
          paths
          ~init:PortMap.empty
          ~f:(fun acc path ->
            let out_port = port_to_pnk (get_out_port path) in
            let existing_prob = match PortMap.find acc out_port with
              | None -> Prob.zero
              | Some x -> x in
            PortMap.add acc ~key:out_port ~data:(existing_prob +/ prob)) in
      SrcDstMap.add acc ~key:(v1,v2) ~data:port_map)


(************** Translate routing schemes to ProbNetKAT policies **************)

(* translate a path as a global probnetkat program that includes switch and link actions *)
let path_to_global_pnk (topo : Topology.t) (path : path) (v_id, id_v) =
  let include_links = true in (* include link transformations *)
  let (_,_), pol = List.fold_left path
    ~init:((0,0), Drop)
    ~f:(fun ((in_sw_id, in_pt_id), pol) link ->
      let src_sw, src_pt = Topology.edge_src link in
      let dst_sw, dst_pt = Topology.edge_dst link in
      let src_sw_id = VertexMap.find_exn v_id src_sw in
      let src_pt_id = port_to_pnk src_pt in
      let dst_sw_id = VertexMap.find_exn v_id dst_sw in
      let dst_pt_id = port_to_pnk dst_pt in
      let link_pol = (* src switch out-port -> dst switch in-port *)
        ??(Switch src_sw_id) >> ??(Port src_pt_id) >> Dup >>
        !!(Switch dst_sw_id) >> !!(Port dst_pt_id) >> Dup in
      let new_pol =
        if in_sw_id = 0 && in_pt_id = 0 then
          if include_links then link_pol
          else pol
        else
          (* policy at switch : src switch in-port -> src switch out-port *)
          let curr_sw_pol =
            ??(Switch  in_sw_id) >> ??(Port  in_pt_id) >>
            !!(Switch src_sw_id) >> !!(Port src_pt_id) in
          if pol = Drop && include_links then curr_sw_pol >> link_pol
          else if pol = Drop && (not include_links) then curr_sw_pol
          else if pol <> Drop && include_links then pol >> curr_sw_pol >> link_pol
          else pol & curr_sw_pol in
      ((dst_sw_id, dst_pt_id), new_pol)) in
      pol

(* translate a routing scheme as a global probnetkat program that includes
 * switch and link actions *)
let routing_scheme_to_global_pnk (topo : Topology.t) (routes : scheme) (v_id, id_v) =
  SrcDstMap.fold routes ~init:Drop
    ~f:(fun ~key:(src,dst) ~data:path_dist pol_acc ->
      if src = dst then
        pol_acc
      else
        let src_id = VertexMap.find_exn v_id src in
        let dst_id = VertexMap.find_exn v_id dst in
        let path_choices =
          PathMap.fold path_dist ~init:[]
            ~f:(fun ~key:path ~data:prob acc ->
              (path_to_global_pnk topo path (v_id, id_v), prob)::acc) in
        let sd_route_pol =
          ??(Src src_id) >> ??(Dst dst_id) >> ?@path_choices in
        if pol_acc = Drop then
          sd_route_pol
        else
          pol_acc & sd_route_pol)



(* translate a link state routing scheme as a local probnetkat program that
 * includes only switch actions *)
let link_state_scheme_to_local_pnk (topo : Topology.t) (routes : link_state_scheme) (v_id, id_v) =
  SrcDstMap.fold routes ~init:Drop
    ~f:(fun ~key:(src,dst) ~data:port_dist pol_acc ->
      if src = dst || (is_host topo src) || (is_switch topo dst)then
        pol_acc
      else
        let src_id = VertexMap.find_exn v_id src in
        let dst_id = VertexMap.find_exn v_id dst in
        let port_choices =
          PortMap.fold port_dist ~init:[]
            ~f:(fun ~key:port ~data:prob acc ->
              (!!(Port port), prob)::acc) in
        let local_node_pol =
          ??(Switch src_id) >> ??(Dst dst_id) >> ?@port_choices in
        if pol_acc = Drop then
          local_node_pol
        else
          pol_acc & local_node_pol)

(******************* Read traffic demand distribution *************************)
(** Assuming all demands are integers in Mbps **)
let read_demands (dem_file : string) (topo : Topology.t) : (Prob.t SrcDstMap.t)=
  (* Output is a src-dst map of demands in Mbps *)
  let str_node_map =
    Topology.fold_vertexes
    (fun v acc ->
      StringMap.add acc ~key:(Node.name (vertex_to_label topo v)) ~data:v
    ) topo StringMap.empty in
  In_channel.with_file dem_file
    ~f:(fun file ->
        In_channel.fold_lines file
          ~init:SrcDstMap.empty
          ~f:(fun acc line ->
            let entries = Array.of_list (String.split line ~on:' ') in
            let src = StringMap.find_exn str_node_map entries.(0) in
            let dst = StringMap.find_exn str_node_map entries.(1) in
            let dem = Prob.((num_of_string entries.(2)) * of_int 1048576) in
            SrcDstMap.add acc ~key:(src,dst) ~data:dem))

(* Convert demands map to a probability distribution, also store the aggregate demand *)
let demands_to_inp_dist_pnk demands (v_id, id_v) =
  let agg_dem = SrcDstMap.fold demands ~init:Prob.zero
    ~f:(fun ~key:_ ~data:dem acc -> Prob.(dem + acc)) in
  let dist = SrcDstMap.fold demands ~init:[]
    ~f:(fun ~key:(s,d) ~data:dem acc ->
      let src_id = VertexMap.find_exn v_id s in
      let dst_id = VertexMap.find_exn v_id d in
      (!!(Switch src_id) >> !!(Port 1) >> !!(Src src_id) >> !!(Dst dst_id), Prob.(dem / agg_dem))::acc) in
  (agg_dem, ?@dist)

let inp_dist_3eq = ?@[ !!(Switch 101) >> !!(Port 1) >> !!(Src 101) >> !!(Dst 102), 1/6
                ;  !!(Switch 101) >> !!(Port 1) >> !!(Src 101) >> !!(Dst 103), 1/6
                ;  !!(Switch 102) >> !!(Port 1) >> !!(Src 102) >> !!(Dst 101), 1/6
                ;  !!(Switch 102) >> !!(Port 1) >> !!(Src 102) >> !!(Dst 103), 1/6
                ;  !!(Switch 103) >> !!(Port 1) >> !!(Src 103) >> !!(Dst 101), 1/6
                ;  !!(Switch 103) >> !!(Port 1) >> !!(Src 103) >> !!(Dst 102), 1/6 ]

(************************ ProbNetKAT queries **********************************)
let path_length (pk,h) =
  Prob.(of_int (List.length h ) / of_int 2)

let num_packets (pk,h) = Prob.one

let flow_on_link agg_dem (link_src_id, link_dst_id) (pk, h) =
  let rec pkt_traverses_link (src, dst) (pk,h) =
    match h with
    | x::(y::_ as t) -> if (x.Pkt.switch = Some dst && y.Pkt.switch = Some src) then Prob.one
      else pkt_traverses_link (src, dst) (pk, t)
    | x::[] -> Prob.zero
    | [] -> Prob.zero in
  let pkt_prob = pkt_traverses_link (link_src_id, link_dst_id) (pk, h) in
  Prob.(pkt_prob * agg_dem)

let link_congestion topo (v_id, id_v) agg_dem link (pk, h) =
  let link_src_id = VertexMap.find_exn v_id (fst (Topology.edge_src link)) in
  let link_dst_id = VertexMap.find_exn v_id (fst (Topology.edge_dst link)) in
  let link_flow = flow_on_link agg_dem (link_src_id, link_dst_id) (pk, h) in
  let link_cap = Topology.edge_to_label topo link
    |> Link.capacity
    |> Int64.to_int
    |> get_opt_exn
    |> Prob.of_int in
  Prob.(link_flow / link_cap)

let sd_tput topo (v_id, id_v) agg_dem (src, dst) (pk, h) =
  let src_id = VertexMap.find_exn v_id src in
  let dst_id = VertexMap.find_exn v_id dst in
  if (pk.Pkt.src = Some src_id && pk.Pkt.dst = Some dst_id) then agg_dem
  else Prob.zero


let test_loop_history (pk, h) =
  let hist_len = List.length h in
  let pkts = List.fold_left h
    ~init:PktSet.empty
    ~f:(fun acc pk ->
      PktSet.add acc pk) in
  let num_pkts = PktSet.length pkts in
  if hist_len = num_pkts then
    Prob.zero
  else
    Prob.one

(* query on a hist ---> query on hist set *)
let lift_query_avg q = fun hset ->
      let n = HSet.length hset in
      if n=0 then Prob.zero else
      let sum = HSet.fold hset ~init:Prob.zero ~f:(fun acc h ->
        Prob.(acc + q h))
      in
      Prob.(sum / of_int n)

let lift_query_sum q = fun hset ->
      let n = HSet.length hset in
      if n=0 then Prob.zero else
      HSet.fold hset ~init:Prob.zero ~f:(fun acc h ->
        Prob.(acc + q h))

(************************************* Tests **********************************)
let analyze_routing_scheme (topo: Topology.t) (vertex_id_bimap) (network_pnk : Pol.t) (input_dist : Prob.t SrcDstMap.t) (final_filter : Pol.t) =
  let agg_dem, input_dist_pnk = demands_to_inp_dist_pnk input_dist vertex_id_bimap in
  let p = (input_dist_pnk >> network_pnk >> final_filter) in
  let out_dist = eval 10 p in
  (*Dist.print out_dist;*)
  expectation_normalized out_dist ~f:(lift_query_avg path_length)
    |> Prob.to_dec_string
    |> Printf.printf "Latency:\t%s\n";
  Dist.expectation out_dist ~f:(lift_query_sum num_packets)
    |> Prob.to_dec_string
    |> Printf.printf "Num packets:\t%s\n";
  let loop_prob = Dist.expectation out_dist ~f:(lift_query_sum test_loop_history) in
  Printf.printf "Loop free:\t%b%!\n" Prob.(loop_prob = zero);
  (* measure link congestions *)
  let link_congestions = Topology.fold_edges
    (fun link acc ->
      let congestion = Dist.expectation out_dist
        ~f:(lift_query_sum (link_congestion topo vertex_id_bimap agg_dem link)) in
      EdgeMap.add acc ~key:link ~data:congestion) topo EdgeMap.empty in
  (* measure tput per src dst pair *)
  let sd_tputs = SrcDstMap.fold ~init:SrcDstMap.empty
    ~f:(fun ~key:(src, dst) ~data:_ acc ->
      let tput = Dist.expectation out_dist
        ~f:(lift_query_sum (sd_tput topo vertex_id_bimap agg_dem (src, dst))) in
      SrcDstMap.add acc ~key:(src, dst) ~data:tput) input_dist in

  SrcDstMap.iteri sd_tputs ~f:(fun ~key:(s,d) ~data:tput ->
    Printf.printf "(%s,%s) : %s\n" (string_of_vertex topo s) (string_of_vertex topo d) (Prob.to_dec_string tput));

  (*EdgeMap.iteri link_congestions ~f:(fun ~key:link ~data:cong ->
    Printf.printf "%s : %s\n" (string_of_edge topo link) (Prob.to_dec_string cong))*)
  let max_cong = EdgeMap.fold ~init:Prob.zero ~f:(fun ~key:link ~data:cong acc -> max_num acc cong) link_congestions in
  Printf.printf "Max Congestion:\t%s\n" (Prob.to_dec_string max_cong);
  Printf.printf "Congestion drop:\t%b\n" (max_cong >/ Prob.one)

let check_loops (network_pnk: Pol.t) (set_ingress_location: Pol.t)=
  (*Dist.print (eval 20 (set_ingress_location >> network_pnk));*)
  expectation' 5 (set_ingress_location >> network_pnk) ~f:test_loop_history
    |> Prob.to_dec_string
    |> Printf.printf "Loops present: %s\n%!"

let test_global topo_file dem_file = begin
  let topo = Parse.from_dotfile topo_file in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  let scheme_spf = route_spf topo in
  let scheme_ksp = route_ksp topo 3 in
  let routing_spf_pol = routing_scheme_to_global_pnk topo scheme_spf vertex_id_bimap in
  let routing_ksp_pol = routing_scheme_to_global_pnk topo scheme_ksp vertex_id_bimap in
  let input_dist = read_demands dem_file topo in
  Printf.printf "==== Global ====\nSPF:\n";
  analyze_routing_scheme topo vertex_id_bimap routing_spf_pol input_dist Id;
  Printf.printf "\nKSP:\n";
  analyze_routing_scheme topo vertex_id_bimap routing_ksp_pol input_dist Id
  end

let test_local topo_file dem_file = begin
  let topo = Parse.from_dotfile topo_file in
  let vertex_id_bimap = gen_node_pnk_id_map topo in
  (*let topo_pnk = topo_to_pnk topo vertex_id_bimap in*)
  let topo_pnk = topo_to_pnk_lossy topo Prob.(of_int 1/ of_int 10) vertex_id_bimap in
  let test_at_host_pnk = test_pkt_at_host topo vertex_id_bimap in
  let spf_routes = route_link_state_spf topo in
  let ecmp_routes = route_link_state_ecmp topo in
  let ksp_routes = route_link_state_ksp topo 2 in
  let spf_pnk = link_state_scheme_to_local_pnk topo spf_routes vertex_id_bimap in
  (*Printf.printf "SPF: %s\n" (Pol.to_string spf_pnk);*)
  let ecmp_pnk = link_state_scheme_to_local_pnk topo ecmp_routes vertex_id_bimap in
  (*Printf.printf "ECMP: %s\n" (Pol.to_string ecmp_pnk);*)
  let ksp_pnk = link_state_scheme_to_local_pnk topo ksp_routes vertex_id_bimap in
  (*Printf.printf "KSP: %s\n" (Pol.to_string ksp_pnk);*)
  let input_dist = read_demands dem_file topo in
  let network_spf_pnk = (Star (topo_pnk >> spf_pnk)) >> topo_pnk in
  let network_ecmp_pnk = (Star (topo_pnk >> ecmp_pnk)) >> topo_pnk in
  let network_ksp_pnk = (Star (topo_pnk >> ksp_pnk)) >> topo_pnk in
  Printf.printf "\n==== Local ====\nSPF:\n";
  analyze_routing_scheme topo vertex_id_bimap network_spf_pnk input_dist test_at_host_pnk;
  Printf.printf "\nECMP:\n";
  analyze_routing_scheme topo vertex_id_bimap network_ecmp_pnk input_dist test_at_host_pnk;
  Printf.printf "\nKSP:\n%!";
  analyze_routing_scheme topo vertex_id_bimap network_ksp_pnk input_dist test_at_host_pnk;
  (*Printf.printf "test at edge: %s\n%!" (Pol.to_string test_at_edge_pnk);
  let test_at_edge_pnk = test_pkt_at_edge topo vertex_id_bimap in
  check_loops ((Star (topo_pnk >> spf_pnk)) >> topo_pnk) test_at_edge_pnk;
  check_loops ((Star (topo_pnk >> ecmp_pnk)) >> topo_pnk) test_at_edge_pnk;
  check_loops ((Star (topo_pnk >> ksp_pnk)) >> topo_pnk) test_at_edge_pnk;*)
end

let () =
  let topo_file = "examples/4cycle.dot" in
  let dem_file = "examples/4cycle.dem" in
  test_global topo_file dem_file;
  test_local topo_file dem_file
