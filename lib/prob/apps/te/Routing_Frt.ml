open Core.Std
open Frenetic_Network
module Topology = Net.Topology

module VertexSet = Topology.VertexSet
module EdgeSet = Topology.EdgeSet

let () = Random.self_init ()

module type FRT_TYPE = sig

  type frt_tree
  type routing_tree

  type routing_edge = Topology.vertex * Topology.vertex
  type routing_path = routing_edge list

  (* Makes an FRT tree decomposition of the given topology.
     This should be called first. *)
  val make_frt_tree : Topology.t -> frt_tree

  (* Makes a routing tree from the given tree decomposition, topology,
     and list of hosts. Typically this is called using the output from
     make_frt_tree and the same topology. *)
  val generate_rt : Topology.t -> frt_tree -> Topology.vertex list ->
    routing_tree

  (* Estimate the usage on every edge of a routing tree. *)
  val usage_of_tree : routing_tree -> (Topology.edge * float) list

  (* Given a routing tree, returns the upward and downward segments of the path
     from the given source node to the given sink node.
     Call this with the output of generate_rrt. *)
  val get_path_halves : routing_tree -> Topology.vertex -> Topology.vertex ->
    routing_path * routing_path

  (* Given a routing tree, returns a routing path from the given source node
     to the given sink node. Call this with the output of generate_rrt. *)
  val get_path : routing_tree -> Topology.vertex -> Topology.vertex ->
    routing_path

  (* Greedy cycle removal routine. *)
  val remove_cycles : NetPath.t -> NetPath.t

  (* TODO(cy): delete this debugging function? *)
  val routing_edges : routing_tree -> routing_edge list

  (* Converts an edge in the routing tree to the corresponding path in the
     physical topology. *)
  val edge_to_physical : routing_tree -> routing_edge -> NetPath.t

  (* Converts an path in the routing tree to the corresponding path in the
     physical topology. *)
  val path_to_physical : routing_tree -> routing_edge list -> NetPath.t

  (* Helper function for visualization. Returns lists of vertices
     where each list corresponds to a level of the tree decomposition. *)
  val get_levels : frt_tree -> Topology.VertexSet.t list list

  (* Helper functions to write frt and rts to .dot files *)
  val write_frt : Topology.t -> frt_tree -> string -> unit
  val write_rt : Topology.t -> routing_tree -> string -> unit

end

module FRT : FRT_TYPE =
struct

  type rt = TreeRTNode of Topology.vertex * Topology.VertexSet.t
    * (rt list)

  (* TODO(cy,jnf): this type is now redundant. Eliminate it? *) 
  type rt_with_paths = RTNode of Topology.vertex * Topology.VertexSet.t
    * rt_with_paths list

  type routing_tree = (Topology.edge * float) list *
      (Topology.vertex * Topology.vertex, NetPath.t) Hashtbl.t *
      rt_with_paths

  type routing_edge = Topology.vertex * Topology.vertex
  type routing_path = routing_edge list

  (* TODO(jnf): eliminate Leaf -- it is only used in constructing the cut decomposition *)
  type cut_decomp =
      Node of Topology.vertex * Topology.VertexSet.t * (cut_decomp list)
    | Leaf of Topology.vertex * Topology.VertexSet.t
    | Single of Topology.VertexSet.t (* TODO(jnf): make this Topology.vertex? *)

  type frt_tree = cut_decomp *
      (Topology.vertex * Topology.vertex,
       NetPath.weight * Topology.edge list) Hashtbl.t

  let permute lst =
    let tagged_lst = List.map lst ~f:(fun v -> (v, Random.bits ())) in
    let sorted = List.sort tagged_lst ~cmp:(fun (_,t1) (_,t2) -> compare t1 t2) in
    List.map sorted ~f:fst

  let make_frt_tree (topo : Topology.t) : frt_tree =
    let open Topology in
        let vertices = Topology.fold_vertexes (fun v acc -> v::acc) topo [] in
        let permuted_vs = permute vertices in
        let permuted_sets = List.map permuted_vs ~f:(fun v -> (v, VertexSet.empty)) in
        let rand_float = Random.float 1.0 in
        let beta = exp (rand_float *. (log 2.0)) in
        let paths_list = NetPath.all_pairs_shortest_paths ~topo:topo
            ~f:(fun x y -> true) in
        let vlist_table = Hashtbl.Poly.create () ~size:8 in
        let max_diameter = List.fold_left paths_list ~init:0. ~f:(fun acc (c,v1,v2,p) ->
          Hashtbl.add_exn vlist_table (v1,v2) (c,p);
          max acc c) in

        let dist v1 v2 =
          let c,_ =
	    try Hashtbl.find_exn vlist_table (v1,v2)
            with Not_found ->
	      failwith (Printf.sprintf "No distance between %s and %s\n"
			  (Node.name (vertex_to_label topo v1))
			  (Node.name (vertex_to_label topo v2))) in
	  c in

          (* Given a cluster of vertices, performs one iteration of
             the cut decomposition.*)
          let rec level_decomp (i : int) (cluster : cut_decomp) : cut_decomp =
            match cluster with
              | Node (_,_,_) -> failwith "shouldn't get here"
              | Leaf (center, cluster_vs) ->
                let beta_i = (2.0 ** (float (i -1))) *. beta in
                let find_first_within radius v permutation =
                  let rec search l acc = match l with
                    | [] -> failwith "find_first_within"
                    | (h, set)::t ->
                      if (dist h v) <= radius then
                        let new_hd = (h, VertexSet.add set v) in
                        List.rev_append t (new_hd::acc)
                      else search t ((h, set)::acc) in
                  List.rev (search permutation []) in

                (* For all nodes in the current cluster, assign them to
                   the first node in the permutation that is closer than
                   beta_i. *)
                let partition = VertexSet.fold cluster_vs ~init:permuted_sets ~f:(fun p_acc next_v ->
                  find_first_within beta_i next_v p_acc) in

                (* Remove all empty clusters *)
                let filtered = List.filter partition ~f:(fun (_,set) ->
                  not (VertexSet.is_empty set)) in

                (* Convert subsets into tree nodes, and catch the case where
                   a cluster is a singleton. *)
                let children = List.map filtered ~f:(fun (v,set) ->
                  if VertexSet.length set = 1 then Single (set)
                  else Leaf (v,set)) in

                (* Recursively make all the lower levels. *)
                let mapped_children = List.map children ~f:(fun c ->
                  level_decomp (i-1) c) in
                Node (center, cluster_vs, mapped_children)
              | Single (v) -> Single (v) in

          let head = match permuted_vs with h::t -> h
            | [] -> failwith "no vertices in topology" in
          let initial_set = List.fold_left vertices ~init:VertexSet.empty
	    ~f:(fun acc x -> VertexSet.add acc x) in
          let initial_tree = Leaf (head, initial_set) in
          let initial_i = snd (Float.frexp max_diameter) - 1 in
          let decomp = level_decomp initial_i initial_tree in
          (decomp, vlist_table)

    let check_tree_no_leaves ((tree,_) : frt_tree) : bool =
      let rec check tr =
        match tr with
          | Leaf (_,_) -> failwith "leaf found"
          | Single (_) -> true
          | Node (_,_,children) ->
            if children = [] then failwith "node with no children" else
              List.fold_left children ~init:true ~f:(fun acc x -> acc && check x) in
      check tree

    let check_tree_laminar ((tree,_) : frt_tree) : bool =
      let rec check tr =
        let open Topology in
            match tr with
              | Single (_) -> true
              | Leaf (_,_) -> true
              | Node (_,set,children) ->
                let result1 = List.fold_left children ~init:true
		  ~f:(fun acc c -> acc && match c with
                  | Single (set2) -> Topology.VertexSet.subset set2 set
                  | Leaf (_,set2) -> Topology.VertexSet.subset set2 set
                  | Node (_,set2,_) -> Topology.VertexSet.subset set2 set) in
                result1 && (List.fold_left children ~init:true ~f:(fun acc x ->
		  acc && check x)) in
        check tree

    let get_cluster (c : cut_decomp) =
      match c with
        | Single (set) -> set
        | Leaf (_,set) -> set
        | Node (_,set,_) -> set

    let check_all_nodes_present ((tree,_) : frt_tree) : bool =
      let rec check tr =
        let open Topology in
            match tr with
              | Single (v) -> true
              | Leaf (_,set) -> true
              | Node (_,set,children) ->
                let child_verts = List.fold_left children ~init:VertexSet.empty
		  ~f:(fun acc next ->
                    let child_set = get_cluster next in
                    VertexSet.union acc child_set) in
                let locally_equal = VertexSet.equal child_verts set in
                locally_equal && (List.fold_left children ~init:true
				    ~f:(fun acc x -> acc && check x)) in
        check tree

    let cluster_contains (c : cut_decomp) (v : Topology.vertex) =
      Topology.VertexSet.mem (get_cluster c) v

    let get_center (c : cut_decomp) =
      match c with
        | Single (set) -> List.hd_exn (Topology.VertexSet.elements set)
        | Leaf (v,_) -> v
        | Node (v,_,_) -> v

    let cluster_subset (vset : Topology.VertexSet.t) (c : cut_decomp) =
      Topology.VertexSet.subset vset (get_cluster c)

    let cluster_inter (vset : Topology.VertexSet.t) (c : cut_decomp) =
      Topology.VertexSet.inter vset (get_cluster c)

    let vset_symmetric_diff s1 s2 =
      let set1 = Topology.VertexSet.diff s1 s2 in
      let set2 = Topology.VertexSet.diff s2 s1 in
      Topology.VertexSet.union set1 set2

    (* Given a tree decomposition as returned by make_frt_tree,
       generates a RRT with given ingress node and egress nodes. *)
    let generate_rt (orig_topo : Topology.t)
        ((tree_metric, vlist_table) : frt_tree)
        (endpoints : Topology.vertex list) : routing_tree =

      let tree_table = Hashtbl.Poly.create () ~size:8 in

      let add_path src dst =
        let _,path = Hashtbl.find_exn vlist_table (src, dst) in
        Hashtbl.add tree_table (src,dst) path in

      let shortest_path src dst =
        Hashtbl.find_exn tree_table (src,dst) in

      (* Makes a tree with the root of the current decomposition as root,
         with paths to all endpoints. *)
      let rec make_tree_downward tree endpts =
        match tree with
          | Single (set) ->
            let elt = VertexSet.min_elt_exn set in
            TreeRTNode (elt, set, [])
          | Leaf (_,_) -> failwith "generate_rrt: no leaves allowed in frt_tree"
          | Node (center, set, children) ->
            let new_children = List.fold_left children ~init:[] ~f:(fun acc subtree ->
              let sub_cluster = get_cluster subtree in
              let new_endpts = VertexSet.inter endpts sub_cluster in
              if VertexSet.is_empty new_endpts then acc else
                let child = make_tree_downward subtree new_endpts in
                child::acc) in
            TreeRTNode (center, endpts, new_children) in

      let endpt_set = List.fold_left endpoints ~init:Topology.VertexSet.empty
	~f:(fun acc x -> Topology.VertexSet.add acc x ) in
      let rt_no_paths = make_tree_downward tree_metric endpt_set in

      let usage_table = Hashtbl.Poly.create () ~size:8 in

      (* Returns the set of all edges that have exactly one endpoint
         in the given vertex set. *)
      (* TODO this could be cleaner if we added fold_succ_e to topology *)
      let edges_in_boundary vertex_set =
        let add_neighbor_edges init_set v =
          let neighbors = Topology.neighbors orig_topo v in
          let add_opt e_opt set =
            match e_opt with Some e -> EdgeSet.add set e
              | None -> set in
          (* Fold examines each neighbor, and if that neighbor is NOT
             in vertex_set, adds the edge adjoining that neighbor. *)
          VertexSet.fold neighbors ~init:init_set ~f:(fun acc neighbor ->
            if VertexSet.mem vertex_set neighbor then acc
            else
              let edge1 = try (Some (Topology.find_edge orig_topo v neighbor))
                with Not_found -> None in
              add_opt edge1 acc) in
        VertexSet.fold  vertex_set ~init:EdgeSet.empty ~f:add_neighbor_edges in

      let rec map_and_compute_usage tree : rt_with_paths =
        let TreeRTNode (center, set, children) = tree in
        let root_children = List.fold_left children ~init:[] ~f:(fun acc next ->
          let TreeRTNode (c_center, c_set, _) = next in
          ignore (add_path c_center center);
          ignore (add_path center c_center);
          let path_up = shortest_path c_center center in
          let boundary_edges = edges_in_boundary c_set in
          let usage = EdgeSet.fold boundary_edges ~init:Int64.zero
	    ~f:(fun acc edge ->
            let cap = Link.capacity (Topology.edge_to_label orig_topo edge) in
            Int64.(+) acc cap)  in

          (* Adds a given amount of usage to both an edge and its inverse. *)
          let add_usage amount edge =
            let add_to edge =
              let old_usage =
                if Hashtbl.mem usage_table edge then
                  Hashtbl.find_exn usage_table edge
                else Int64.zero in
              Hashtbl.replace usage_table edge (Int64.(+) old_usage amount) in
            add_to edge;
            match Topology.inverse_edge orig_topo edge with
              | Some inv -> add_to inv
              | None -> () in

          (* Update usages for all edges on the path *)
          List.iter path_up ~f:(add_usage usage);

          (* Recursively convert the rest of the tree *)
          let subtree = map_and_compute_usage next in
          subtree::acc) in
        let init = if root_children = [] then set else VertexSet.empty in
        let reduced_set = List.fold_left root_children ~init:init
	  ~f:(fun acc child ->
          let RTNode (_,c_set,_) = child in
          Topology.VertexSet.union c_set acc) in
        RTNode (center, reduced_set, root_children) in

      let routing_tree = map_and_compute_usage rt_no_paths in
      let usage_vec = Hashtbl.fold usage_table ~init:[]
	~f:(fun ~key:k ~data:v acc ->
          let cap = Link.capacity (Topology.edge_to_label orig_topo k) in
          (k,(Int64.to_float v) /. (Int64.to_float cap))::acc) in
      (usage_vec, tree_table, routing_tree)

    let usage_of_tree ((lst,_,_) : routing_tree) = lst

    let remove_cycles path =
      let edge_dst t = fst (Topology.edge_dst t) in
      let edge_src t = fst (Topology.edge_src t) in
      let path_segment path v = match path with
        | [] -> []
        | h::t ->
          fst (List.fold_right path ~init:([], false) ~f:(fun next (acc,over) ->
            if over then (acc,over)
            else
              let src = edge_src next in
              let dst = edge_dst next in
              if src = v then (acc,true)
              else if dst = v then (next::acc, true) else (next::acc, false))) in
      let rec check path acc seen = match path with
        | [] -> acc
        | h::t ->
          let dst = edge_dst h in
          if Topology.VertexSet.mem seen dst then
            (* cut out the cycle *)
            let segment = path_segment acc dst in
            let first_node = match segment with
              | [] -> Topology.VertexSet.empty
              | h::t -> Topology.VertexSet.singleton (edge_src h) in
            let new_seen = List.fold_left segment ~init:first_node ~f:(fun acc next ->
              Topology.VertexSet.add acc (edge_dst next)) in
            check t segment new_seen
          else
            check t (h::acc) (Topology.VertexSet.add seen dst) in
      match path with
        | [] -> []
        | h::t -> let first_src = (edge_src h) in
          List.rev (check path [] (Topology.VertexSet.singleton first_src))

    let rec construct_path_down tree dst =
      let RTNode (center,_,children) = tree in
      if children = [] then
        if center = dst then [] else failwith "get_path: dst not in tree"
      else
        let child = try List.find_exn children ~f:(fun c ->
          match c with RTNode(_,set,_) ->
            Topology.VertexSet.mem set dst)
          with x ->
            failwith ("construct_path_down: " ^ (Exn.to_string x)) in
        let RTNode (child_center,_,_) = child in
        (center, child_center)::(construct_path_down child dst)

    let construct_path_up tree src =
      let path_down = construct_path_down tree src in
      let tups_reversed = List.map path_down ~f:(fun (a,b) -> (b,a)) in
      List.rev tups_reversed

    let rec get_path_halves ((u,tbl,tree) : routing_tree) (src : Topology.vertex)
        (dst : Topology.vertex) : routing_path * routing_path =
      let RTNode (_,_,cs) = tree in
      let src_subtree_opt =
        List.fold_left cs ~init:None ~f:(fun acc sub ->
          let RTNode (_,set,_) = sub in
          if Topology.VertexSet.mem set src then Some sub else acc)  in
      let src_subtree =
        match src_subtree_opt with
          | None -> failwith "get_path: src not in tree"
          | Some p -> p in
      let RTNode (_,src_set,_) = src_subtree in
      if Topology.VertexSet.mem src_set dst then
        get_path_halves (u,tbl,src_subtree) src dst
      else
        let path_up = construct_path_up tree src in
	let path_down = construct_path_down tree dst in
        (path_up, path_down)

    let get_path routing_tree src dst =
      let (path_up, path_down) = get_path_halves routing_tree src dst in
      path_up @ path_down

    let routing_edges ((_,tbl,_):routing_tree) : routing_edge list =
      Hashtbl.fold tbl ~init:[] ~f:(fun ~key:e ~data:_ acc -> e::acc)

    let edge_to_physical ((_,tbl,_) : routing_tree) (edge : routing_edge) =
      Hashtbl.find_exn tbl edge

    let path_to_physical (routing_tree : routing_tree)
        (path : routing_edge list) =
      let path_segments = List.map path ~f:(edge_to_physical routing_tree) in
      List.concat path_segments

    let get_levels ((frt_tree,_) : frt_tree) =
      let is_single node = match node with
        | Single (_) -> true
        | _ -> false in
      let all_leaves level =
        List.fold_left level ~init:true ~f:(fun acc node -> acc && (is_single node)) in
      let rec levels (nodes : cut_decomp list) : cut_decomp list list =
        if all_leaves nodes then [] else
          let next_level_lsts = List.map nodes ~f:(fun node -> match node with
            | Node (_,_,children) -> children
            | Leaf (_,_) -> failwith "get_levels: should not have leaves"
            | single -> [single]) in
          let next_level = List.concat next_level_lsts in
          next_level::(levels next_level) in
      let all_levels = [frt_tree]::(levels [frt_tree]) in
      List.map all_levels ~f:(fun level ->
	List.map level ~f:(fun node -> get_cluster node))

    let name_of_node topo vert =
      let vert_label = Topology.vertex_to_label topo vert in
      let prefix = match Node.device vert_label with
        | Node.Switch -> 's'
        | Node.Host -> 'h'
        | Node.Middlebox -> 'm' in
      let name = Node.name vert_label in
      let new_name = String.copy name in
      new_name.[0] <- prefix;
      new_name

    let vertex_to_dot topo vert level highlight =
      let vert_label = Topology.vertex_to_label topo vert in
      let device_type = match Node.device vert_label with
        | Node.Switch -> "switch"
        | Node.Host -> "host"
        | Node.Middlebox -> "middlebox" in
      let subt = max (255 lsr level) 0 in
      let color = 255 - subt in
      let hex_color = Printf.sprintf "%02X" color in
      let name = name_of_node topo vert in
      if highlight then
        Printf.sprintf "%s [type=%s, color=\"#FF0000\", style=filled, fillcolor=\"#FF%s%s\"];\n" name device_type hex_color hex_color
      else
        Printf.sprintf "%s [type=%s];\n" name device_type

    let edge_to_dot topo edge highlight =
      let src,src_port = Topology.edge_src edge in
      let dst,dst_port = Topology.edge_dst edge in
      if highlight then
        Printf.sprintf "%s -> %s [src_port=%ld, dst_port=%ld, color=\"#FF0000\"];\n"
          (name_of_node topo src) (name_of_node topo dst) src_port dst_port
      else
        Printf.sprintf "%s -> %s [src_port=%ld, dst_port=%ld];\n"
          (name_of_node topo src) (name_of_node topo dst) src_port dst_port

    let edges_in_tree (_, table, _) =
      Hashtbl.fold table ~init:EdgeSet.empty ~f:(fun ~key:_ ~data:path acc ->
        List.fold_left path ~init:acc ~f:(fun acc_set edge ->
	  EdgeSet.add acc_set edge))

    let vertices_in_tree (_,_,tr) =
      let level_table = Hashtbl.Poly.create () ~size:8 in
      let rec get_verts tree level =
        match tree with RTNode (center,_,children) ->
          let () = if Hashtbl.mem level_table center then
              let old_level = Hashtbl.find_exn level_table center in
              let new_level = min old_level level in
              Hashtbl.replace level_table center new_level
            else
              Hashtbl.add_exn level_table center level in
          List.fold_left children ~init:(VertexSet.singleton center)
	    ~f:(fun acc next ->
              let child_verts = get_verts next (level + 1) in
              VertexSet.union child_verts acc) in
      let set = get_verts tr 0 in
      (set, level_table)

    let root (_,_,tr) =
      match tr with RTNode (center,_,_) -> center

    (* Write the topology as one graph, with tree edges highlighted *)
    let write_dot1 topo rrt filename =
      let tree_edges = edges_in_tree rrt in
      let tree_verts,levels = vertices_in_tree rrt in
      let out_file = open_out (filename ^ "-rrt1.dot") in
      output_string out_file "strict digraph {\n";
      Topology.iter_vertexes (fun v ->
        let level = if Hashtbl.mem levels v then Hashtbl.find_exn levels v else 16 in
        output_string out_file
          (vertex_to_dot topo v level (VertexSet.mem tree_verts v))) topo;
      Topology.iter_edges (fun e ->
        output_string out_file
          (edge_to_dot topo e (EdgeSet.mem tree_edges e))) topo;
      output_string out_file "}\n";
      Out_channel.close out_file

    let write_dot2 topo rrt filename =
      let tree_edges = edges_in_tree rrt in
      let tree_verts = EdgeSet.fold tree_edges ~init: VertexSet.empty
	~f:(fun acc edge ->
          let src,_ = Topology.edge_src edge in
          let dst,_ = Topology.edge_dst edge in
          let acc2 = VertexSet.add acc src in
          VertexSet.add acc2 dst) in
      (* output original topology *)
      let topo_out = open_out (filename ^ "-rrt2.dot") in
      output_string topo_out "strict digraph {\n";
      Topology.iter_vertexes (fun v -> output_string topo_out
        (vertex_to_dot topo v 0 false)) topo;
      Topology.iter_edges (fun e -> output_string topo_out
        (edge_to_dot topo e false)) topo;
      output_string topo_out "}\n";
      (* output tree topology *)
      output_string topo_out "strict digraph {\n";
      VertexSet.iter tree_verts ~f:(fun v -> output_string topo_out
        (vertex_to_dot topo v 0 false));
      EdgeSet.iter tree_edges ~f:(fun e -> output_string topo_out
        (edge_to_dot topo e false)) ;
      output_string topo_out "}\n";
      Out_channel.close topo_out;
      ()

    let write_frt topo frt filename =
      let levels = get_levels frt in
      let all_edges = Topology.edges topo in
      let out_file = open_out (filename ^ "-frt.dot") in
      let write_level level =
        output_string out_file "strict digraph {\n";
        let edge_in_cluster cluster e =
          let src,_ = Topology.edge_src e in
          let dst,_ = Topology.edge_dst e in
          (VertexSet.mem cluster src) && (VertexSet.mem cluster dst) in
        let write_cluster cluster =
          let cl_edges = EdgeSet.filter all_edges ~f:(edge_in_cluster cluster) in
          VertexSet.iter cluster ~f:(fun v -> output_string out_file
            (vertex_to_dot topo v 0 false)) ;
          EdgeSet.iter cl_edges ~f:(fun e -> output_string out_file
            (edge_to_dot topo e false))  in
        List.iter level ~f:write_cluster ;
        output_string out_file "}\n" in
      List.iter levels ~f:write_level;
      Out_channel.close out_file

    let write_rt topo rrt filename =
      write_dot1 topo rrt filename;
      write_dot2 topo rrt filename

  end
