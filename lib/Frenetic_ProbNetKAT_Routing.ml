open Core.Std
open Frenetic_Network
open Frenetic_ProbNetKAT_Interpreter
open Net
open Num
open Topology
open Routing_Types

module Routing (Prob : PROB) = struct
  type flow_decomp = Prob.t PathMap.t
  type source_routes = flow_decomp SrcDstMap.t

  module PortMap = Map.Make(Int)
  type out_port_dist = Prob.t PortMap.t
  type per_hop_routes = out_port_dist SrcDstMap.t

  module PktSet = Set.Make(Pkt)

  module PQueue = Core_kernel.Heap.Removable

  (************************** helper functions *******************************)
  let xcol = "\027[0m"
  let red = "\027[31m"

  let pprintf color str = Printf.printf "%s%s%s%!" color str xcol

  let get_opt_exn v = match v with
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
      Printf.bprintf buf "[%s] @ %s\n" (dump_links t path) (Prob.to_dec_string prob));
  Buffer.contents buf

  let dump_source_routes (t:Topology.t) (s:source_routes) : string =
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
    IntMap.iteri opd ~f:(fun ~key:port ~data:prob ->
      Printf.bprintf buf "[%d] @ %s\n" port (Prob.to_dec_string prob));
    Buffer.contents buf

  let dump_per_hop_routes (t:Topology.t) (s:per_hop_routes) : string =
    let buf = Buffer.create 101 in
    SrcDstMap.iteri s ~f:(fun ~key:(v1,v2) ~data:opd ->
      Printf.bprintf buf "%s -> %s :\n  %s\n"
          (Node.name (Topology.vertex_to_label t v1))
          (Node.name (Topology.vertex_to_label t v2))
          (dump_port_prob_set opd));
    Buffer.contents buf

  let dump_to_file (file:string) (header:string) (data: Prob.t IntMap.t) : unit =
    let oc = Out_channel.create (file ^ ".dat") in
    fprintf oc "%s\n" (IntMap.fold data ~init:(header ^ "\n")
    ~f:(fun ~key:k ~data:d acc ->
      acc ^ (Int.to_string k) ^ "\t" ^ (Prob.to_dec_string d) ^ "\n"));
    Out_channel.close oc

  let get_hosts_set (topo : Topology.t) : VertexSet.t =
    VertexSet.filter (Topology.vertexes topo) ~f:(fun v ->
      let label = Topology.vertex_to_label topo v in
      Node.device label = Node.Host)

  let get_switch_set (topo : Topology.t) : VertexSet.t =
    VertexSet.filter (Topology.vertexes topo)
    ~f:(fun v ->
      let label = Topology.vertex_to_label topo v in
      Node.device label = Node.Switch)

  (* Remove cycles in a path *)
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


  (************************** compute k shortest paths *****************************)
  let k_shortest_path (topo : Topology.t) (s : Topology.vertex) (t : Topology.vertex) (k : int) =
    if s = t then [] else
    let paths = ref [] in
    let count = Hashtbl.Poly.create () in
    Topology.iter_vertexes (fun u -> Hashtbl.Poly.add_exn count u 0;) topo;
    let bheap = PQueue.create ~min_size:(Topology.num_vertexes topo)
                  ~cmp:(fun (dist1,_) (dist2,_) -> Float.compare dist1 dist2) () in
    PQueue.add_removable bheap (0.0, [s]);
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
                begin
                Topology.iter_succ (fun link ->
                  let (v,_) = Topology.edge_dst link in
                  if (List.mem path_u v) then () else (* consider only simple paths *)
                  let path_v = v::path_u in
                  let weight = Link.weight (Topology.edge_to_label topo link) in
                  let cost_v = cost_u +. weight in
                  let _ = PQueue.add_removable bheap (cost_v, path_v) in
                  ()) topo u;
                explore ()
                end
              else
                if u = t then ()
                else explore () in
    explore ();
    List.fold_left !paths ~init:[]
      ~f:(fun acc path ->
        let link_path = List.fold_left path ~init:([], None)
        ~f:(fun links_u v ->
          let links, u = links_u in
          match u with
          | None -> (links, Some v)
          | Some u ->
              let link = Topology.find_edge topo v u in
              (link::links, Some v)) in
        let p,_ = link_path in
        (remove_cycles p)::acc)

  let all_pair_ksp (topo : Topology.t) (k : int) src_set dst_set =
    VertexSet.fold src_set ~init:SrcDstMap.empty ~f:(fun acc src ->
      VertexSet.fold dst_set ~init:acc ~f:(fun nacc dst ->
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
        if nextHop = i then acc
        else if not (VertexSet.mem neigh nextHop) then acc
        else
          (* consider it as next hop if neighbor *)
          let d_ih = SrcDstMap.find_exn dist (i,nextHop) in
          let d_hj = SrcDstMap.find_exn dist (nextHop,j) in
          let d_ij = SrcDstMap.find_exn dist (i,j) in
          if (abs_fl(d_ih +. d_hj -. d_ij) < 0.00001) then
            (* if it is in a shortest i-j path *)
            let t_visited,_,_ = SrcDstMap.find_exn acc (nextHop,j) in
            let n2_numpath =
              if t_visited then acc
              else dp_calc_numpaths nextHop j topo dist acc in
            let _,np_nj,_ = SrcDstMap.find_exn n2_numpath (nextHop,j) in
            let _,np_ij,l_ij = SrcDstMap.find_exn n2_numpath (i,j) in
            let t_np_ij = np_ij + np_nj in
            SrcDstMap.add n2_numpath ~key:(i,j)
              ~data:(true, t_np_ij, List.append l_ij [(nextHop, Float.of_int np_nj)])
          else acc) topo n_numpath in
    let _,normalizer,l_ij = SrcDstMap.find_exn n3_numpath (i,j) in
    let path_probs,_ = List.fold_left l_ij ~init:([], 0.0)
      ~f:(fun acc (nextHop, np_h) ->
        let l, preprob = acc in
        let n_prob = np_h /. Float.of_int normalizer in
        (List.append l [(nextHop, preprob +. n_prob)], preprob +. n_prob)) in
    SrcDstMap.add n3_numpath ~key:(i,j) ~data:(true, normalizer, path_probs)


  let all_pairs_multi_shortest_path (topo:Topology.t) : (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
    (* topology -> (visited_bool, normalizer, list(next_hop, prob)) SrcDstMap *)
    let dist_mat = Topology.fold_vertexes (fun i dist_mat ->
      Topology.fold_vertexes (fun j dist_mat2 ->
        let ans =
          if i=j then 0.0
          else Float.infinity in
        SrcDstMap.add dist_mat2 ~key:(i,j) ~data:ans) topo dist_mat) topo SrcDstMap.empty in
    let dist_mat_init = Topology.fold_edges (fun e acc ->
      let src,_ = Topology.edge_src e in
      let dst,_ = Topology.edge_dst e in
      let weight = Link.weight (Topology.edge_to_label topo e) in
      SrcDstMap.add acc ~key:(src, dst) ~data:weight) topo dist_mat in
    let dist_mat_sp = Topology.fold_vertexes (fun k acc ->
      Topology.fold_vertexes (fun i acc_i ->
        Topology.fold_vertexes (fun j acc_j ->
          let dij  = SrcDstMap.find_exn acc_j (i,j)  in
          let dik  = SrcDstMap.find_exn acc_j (i,k)  in
          let dkj  = SrcDstMap.find_exn acc_j (k,j)  in
          let upd_val = if (dik +. dkj < dij) then dik +. dkj else dij in
          SrcDstMap.add acc_j ~key:(i,j) ~data:upd_val) topo acc_i) topo acc) topo dist_mat_init in
    (* initialize visited to be true for i,i *)
    let init_vis_npath_pathlist_map = Topology.fold_vertexes (fun i acc_i ->
      Topology.fold_vertexes (fun j acc_j ->
        let visited = if i=j then true else false in
        let num_paths = if i=j then 1 else 0 in
        SrcDstMap.add acc_j ~key:(i,j) ~data:(visited,num_paths,[])) topo acc_i) topo SrcDstMap.empty in
    Topology.fold_vertexes (fun i acc_i ->
      Topology.fold_vertexes (fun j acc_j ->
        let visited,np,_ = SrcDstMap.find_exn acc_j (i,j) in
        if visited then acc_j
        else dp_calc_numpaths i j topo dist_mat_sp acc_j) topo acc_i) topo init_vis_npath_pathlist_map

  (* approximate a path (float) probability distribution using Prob.t *)
  let approx_float_normalize ppd =
    let tmp_map = PathMap.fold ppd ~init:PathMap.empty
      ~f:(fun ~key:path ~data:f_prob acc ->
        PathMap.add acc ~key:path ~data:(Prob.of_float f_prob)) in
    let sum_prob = PathMap.fold tmp_map ~init:Prob.zero
      ~f:(fun ~key:_ ~data:p acc -> Prob.(acc + p)) in
    PathMap.fold tmp_map ~init:PathMap.empty
      ~f:(fun ~key:path ~data:p acc -> PathMap.add acc ~key:path ~data:Prob.(p / sum_prob))

  let approx_float_source_routes (s:scheme) : source_routes =
    SrcDstMap.fold s ~init:SrcDstMap.empty
      ~f:(fun ~key:(s,d) ~data:ppd acc ->
        SrcDstMap.add acc ~key:(s,d) ~data:(approx_float_normalize ppd))

  (* Solve using Raecke's algorithm and convert scheme to source_routes type *)
  let solve_raecke (t : Topology.t) (d:demands) : source_routes =
    let open Routing_Raecke in
    solve_raecke t d
    |> approx_float_source_routes

end
