open Graph
(* open Sexplib.Conv *)
open Core_kernel.Std

module type VERTEX = sig
  type t [@@deriving sexp]

  val compare : t -> t -> int
  val to_string : t -> string
  val to_dot : t -> string
  val to_mininet : t -> string
  val parse_dot : Graph.Dot_ast.node_id -> Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
 end

module type EDGE = sig
  type t [@@deriving sexp]

  val compare : t -> t -> int
  val to_string : t -> string
  val to_dot : t -> string
  val parse_dot : Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
  val default : t
end

module type WEIGHT = sig
  type t [@@deriving sexp]
  type edge [@@deriving sexp]

  val weight : edge -> t
  val compare : t -> t -> int
  val add : t -> t -> t
  val zero : t
end

module type NETWORK = sig
  module Topology : sig
    type t

    type vertex [@@deriving sexp]
    type edge [@@deriving sexp]
    type port = int32 [@@deriving sexp]

    module Vertex : VERTEX
    module Edge : EDGE

    module UnitWeight : WEIGHT
      with type t = int
      and type edge = Edge.t

    module EdgeSet : Set.S
      with type Elt.t = edge

    module VertexSet : Set.S
      with type Elt.t = vertex

    module VertexHash : Hashtbl.S
      with type key = vertex

    module PortSet : Set.S
      with type Elt.t = port

    (* Constructors *)
    val copy : t -> t
    val empty : unit -> t
    val add_vertex : t -> Vertex.t -> (t * vertex)
    val add_port : t -> vertex -> port -> t
    val add_edge : t -> vertex -> port -> Edge.t -> vertex -> port -> (t * edge)

    (* Special Accessors *)
    val num_vertexes : t -> int
    val num_edges : t -> int
    val vertexes : t -> VertexSet.t
    val edges : t -> EdgeSet.t
    val neighbors : t -> vertex -> VertexSet.t
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> EdgeSet.t
    val vertex_to_ports : t -> vertex -> PortSet.t
    val next_hop : t -> vertex -> port -> edge option
    val edge_src : edge -> (vertex * port)
    val edge_dst : edge -> (vertex * port)
    val inverse_edge : t -> edge -> edge option

    (* Label Accessors *)
    val vertex_to_string : t -> vertex -> string
    val vertex_to_label : t -> vertex -> Vertex.t
    val vertex_of_label : t -> Vertex.t -> vertex
    val edge_to_string : t -> edge -> string
    val edge_to_label : t -> edge -> Edge.t

    (* Iterators *)
    val iter_succ : (edge -> unit) -> t -> vertex -> unit
    val iter_vertexes : (vertex -> unit) -> t -> unit
    val iter_edges : (edge -> unit) -> t -> unit
    val fold_vertexes : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_edges : (edge -> 'a -> 'a) -> t -> 'a -> 'a

    (* Mutators *)
    val remove_vertex : t -> vertex -> t
    val remove_port : t -> vertex -> port -> t
    val remove_edge : t -> edge -> t
    val remove_endpoint : t -> (vertex * port) -> t
  end


  (* Traversals *)
  module Traverse : sig
    val bfs : (Topology.vertex -> unit) -> Topology.t -> unit
    val dfs : (Topology.vertex -> unit) -> Topology.t -> unit
  end

  val spanningtree_from : (Topology.vertex -> 'a list -> 'a) -> Topology.t -> Topology.vertex -> 'a

  (* Paths *)
  module type PATH = sig
    type weight
    type t = Topology.edge list
    exception NegativeCycle of t

    val shortest_path : Topology.t -> Topology.vertex -> Topology.vertex -> t option
    val all_shortest_paths : Topology.t -> Topology.vertex -> Topology.vertex Topology.VertexHash.t
    val all_pairs_shortest_paths :
        topo:Topology.t ->
        f:(Topology.vertex -> Topology.vertex -> bool) ->
       (weight * Topology.vertex * Topology.vertex * Topology.edge list) list
  end

  module Path (Weight : WEIGHT with type edge = Topology.Edge.t) :
    PATH with type weight = Weight.t

  module UnitPath : PATH
    with type weight = int

  (* Parsing *)
  module Parse : sig
    val from_dotfile : string -> Topology.t
    val from_gmlfile : string -> Topology.t
  end

  (* Pretty Printing *)
  module Pretty : sig
    val to_string : Topology.t -> string
    val to_dot : Topology.t -> string
    val to_mininet : ?prologue_file:string -> ?epilogue_file:string ->
      Topology.t -> string
  end
end

module type MAKE = functor (Vertex:VERTEX) -> functor (Edge:EDGE) -> NETWORK
  with module Topology.Vertex = Vertex
   and module Topology.Edge = Edge

module Make : MAKE =
  functor (Vertex:VERTEX) ->
    functor (Edge:EDGE) ->
struct
  module Topology = struct
    type port = int32 [@@deriving sexp]
    module PortSet = Set.Make(Int32)
    module PortMap = Map.Make(Int32)

    module Vertex = Vertex
    module Edge = Edge

    module VL = struct
      type t = {
        id : int;
        label : Vertex.t
      } [@@deriving sexp]

      let compare n1 n2 = Int.compare n1.id n2.id
      let hash n1 = Hashtbl.hash n1.id
      let equal n1 n2 = n1.id = n2.id
      let to_string n = string_of_int n.id
      (*
      let sexp_of_t v = Sexp.List [Sexp.Atom "id"; sexp_of_int v.id ]
      let t_of_sexp s = { id = int_of_sexp (Sexp.Atom "1"); label = vertex_t_of_sexp s }
      *)
    end

    module VertexSet = Set.Make(VL)
    module VertexMap = Map.Make(Vertex)
    module VertexHash = Hashtbl.Make(VL)
    module EL = struct
      type t = { id : int;
                 label : Edge.t;
                 src : port;
                 dst : port } [@@deriving sexp]
      let compare e1 e2 = Int.compare e1.id e2.id
      let hash e1 = Hashtbl.hash e1.id
      let equal e1 e2 = e1.id = e2.id
      let to_string e = string_of_int e.id
      let default =
        { id = 0;
          label = Edge.default;
          src = 0l;
          dst = 0l }
    end

    module UnitWeight = struct
      type edge = Edge.t [@@deriving sexp]
      type t = int [@@deriving sexp, compare]
      type label = EL.t
      let weight _ = 1
      let add = (+)
      let zero = 0
    end

    type vertex = VL.t [@@deriving sexp]

    type edge = vertex * EL.t * vertex [@@deriving sexp]

    module EdgeSet = Set.Make(struct
      type t = VL.t * EL.t * VL.t  [@@deriving sexp]
      let compare (e1:t) (e2:t) : int =
        let (_,l1,_) = e1 in
        let (_,l2,_) = e2 in
        EL.compare l1 l2
    end)

    module P = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(VL)(EL)


    type t =
        { graph : P.t;
          node_info : (vertex * PortSet.t) VertexMap.t;
          next_node : int;
          next_edge : int }

    (* Constructors *)
    let copy (t:t) : t =
      t

    let empty () : t =
      { graph = P.empty;
        node_info = VertexMap.empty;
        next_node = 0;
        next_edge = 0 }

    let _node_vertex (t:t) (l:Vertex.t) : vertex =
      fst (VertexMap.find_exn t.node_info l)

    let _node_ports (t:t) (l:Vertex.t) : PortSet.t =
      snd (VertexMap.find_exn t.node_info l)

    let add_vertex (t:t) (l:Vertex.t) : t * vertex =
      let open VL in
      try (t, _node_vertex t l)
      with Not_found ->
        let id = t.next_node + 1 in
        let v = { id = id; label = l } in
        let g = P.add_vertex t.graph v in
        let nl = VertexMap.set t.node_info l (v, PortSet.empty) in
        ({ t with graph=g; node_info=nl; next_node = id}, v)

    let add_port (t:t) (v:vertex) (p:port) : t =
      let l = v.VL.label in
      let v, ps = VertexMap.find_exn t.node_info l in
      let node_info = VertexMap.set t.node_info l (v, PortSet.add ps p) in
      { t with node_info }

    let add_edge (t:t) (v1:vertex) (p1:port) (l:Edge.t) (v2:vertex) (p2:port) : t * edge =
      let open EL in
      let aux t =
        let id = t.next_edge + 1 in
        let l = { id = id; label = l; src = p1; dst = p2 } in
        let e = (v1,l,v2) in
        let t = add_port t v1 p1 in
        let t = add_port t v2 p2 in
        ({ t with graph = P.add_edge_e t.graph e; next_edge = id }, e) in

      try
        let es = P.find_all_edges t.graph v1 v2 in
        let es' = List.filter es ( fun (s,l,d) ->
          l.src = p1 && l.dst = p2 ) in
        match es' with
          | [] -> aux t
          | es ->
            let graph' = List.fold_left es ~init:t.graph ~f:(fun acc e ->
            P.remove_edge_e acc e) in
            let t' = {t with graph = graph'} in
            aux t'
      with Not_found -> aux t

    (* Special Accessors *)
    let num_vertexes (t:t) : int =
      P.nb_vertex t.graph

    let num_edges (t:t) : int =
      P.nb_edges t.graph

    let edges (t:t) : EdgeSet.t =
      P.fold_edges_e (fun e acc -> EdgeSet.add acc e) t.graph EdgeSet.empty

    let vertexes (t:t) : VertexSet.t =
      P.fold_vertex (fun v acc -> VertexSet.add acc v) t.graph VertexSet.empty

    let neighbors (t:t) (v:vertex) : VertexSet.t =
      P.fold_succ (fun v acc -> VertexSet.add acc v) t.graph v VertexSet.empty

    let find_edge (t:t) (src:vertex) (dst:vertex) : edge =
      P.find_edge t.graph src dst

    let find_all_edges (t:t) (src:vertex) (dst:vertex) : EdgeSet.t =
      List.fold_left (P.find_all_edges t.graph src dst)  ~init:EdgeSet.empty
	~f:EdgeSet.add

    let vertex_to_string (t:t) (v:vertex) : string =
      VL.to_string v

    let vertex_to_label (t:t) (v:vertex) : Vertex.t =
      v.VL.label

    let vertex_of_label (t:t) (l:Vertex.t) : vertex =
      _node_vertex t l

    let edge_to_label (t:t) (e:edge) : Edge.t =
      let (_,l,_) = e in
      l.EL.label

    let edge_to_string (t:t) (e:edge) : string =
      let (_,e,_) = e in
      EL.to_string e

    let edge_src (e:edge) : (vertex * port) =
      let (v1,l,_) = e in
      (v1, l.EL.src)

    let edge_dst (e:edge) : (vertex * port) =
      let (_,l,v2) = e in
      (v2, l.EL.dst)

    let inverse_edge (t:t) (e:edge) : edge option =
      let src_vertex, src_port = edge_src e in
      let dst_vertex, dst_port = edge_dst e in
      try
        let inv_e = find_edge t dst_vertex src_vertex in
        if dst_port = snd (edge_src inv_e) && src_port = snd (edge_dst inv_e)
        then Some(inv_e)
        else None
      with _ -> None

    let next_hop (t:t) (v1:vertex) (p:port) : edge option =
      let rec loop es = match es with
        | [] -> None
        | ((_,l,v2) as e)::es' ->
          if l.EL.src = p
            then Some e
            else (loop es') in
      loop (P.succ_e t.graph v1)

    let vertex_to_ports (t:t) (v1:vertex) : PortSet.t =
      _node_ports t v1.VL.label

    (* Iterators *)
    let fold_vertexes (f:vertex -> 'a -> 'a) (t:t) (init:'a) : 'a =
      P.fold_vertex f t.graph init

    let fold_edges (f:edge -> 'a -> 'a) (t:t) (init:'a) : 'a =
      P.fold_edges_e f t.graph init

    let iter_vertexes (f:vertex -> unit) (t:t) : unit =
      P.iter_vertex f t.graph

    let iter_edges (f:edge -> unit) (t:t) : unit =
      P.iter_edges_e f t.graph

    let iter_succ (f:edge -> unit) (t:t) (v:vertex) : unit =
      P.iter_succ_e f t.graph v

    (* Mutators *)
    let remove_vertex (t:t) (v:vertex) : t =
      let graph = P.remove_vertex t.graph v in
      let node_info = VertexMap.remove t.node_info v.VL.label in
      { t with graph; node_info }

    let remove_port (t:t) (v:vertex) (p:port) : t =
      let v, ps = VertexMap.find_exn t.node_info v.VL.label in
      let ps = PortSet.remove ps p in
      let node_info = VertexMap.set t.node_info v.VL.label (v, ps) in
      { t with node_info }

    let remove_edge (t:t) (e:edge) : t =
      { t with graph = P.remove_edge_e t.graph e }

    let remove_endpoint (t:t) (ep : vertex * port) : t =
      let t = fold_edges (fun e acc ->
        if edge_src e = ep || edge_dst e = ep
          then remove_edge acc e
          else acc)
        t t
      in
      let v, p = ep in
      let v, ps = VertexMap.find_exn t.node_info v.VL.label in
      let ps = PortSet.remove ps p in
      let node_info = VertexMap.set t.node_info v.VL.label (v, ps) in
      { t with node_info }

   let remove_port (t:t) (v:vertex) (p:port) =
     remove_endpoint t (v, p)

  end


  (* Traversals *)
  module Traverse = struct
    open Topology
    module Bfs = Graph.Traverse.Bfs(P)
    module Dfs = Graph.Traverse.Dfs(P)

    let bfs (f:vertex -> unit) (t:t) =
      Bfs.iter f t.graph

    let dfs (f:vertex -> unit) (t:t) =
      Dfs.prefix f t.graph
  end

  module Prim = Graph.Prim.Make (Topology.P) (struct
    type edge = Topology.P.edge
    type t = int [@@deriving compare]
    type label = Topology.EL.t
    let weight _ = 1
    let add = (+)
    let zero = 0
  end)

  let spanningtree_from f graph vertex =
    let open Topology.P in
    let edges = Prim.spanningtree_from graph.Topology.graph vertex in
    let tree = List.fold_left edges ~init:empty ~f:add_edge_e in
    let rec loop vx = f vx (List.map (succ tree vx) ~f:loop) in
    loop vertex

  (* Paths *)
  module type PATH = sig
    type weight
    type t = Topology.edge list
    exception NegativeCycle of t

    val shortest_path : Topology.t -> Topology.vertex -> Topology.vertex -> t option
    val all_shortest_paths : Topology.t -> Topology.vertex -> Topology.vertex Topology.VertexHash.t
    val all_pairs_shortest_paths :
        topo:Topology.t ->
        f:(Topology.vertex -> Topology.vertex -> bool) ->
       (weight * Topology.vertex * Topology.vertex * Topology.edge list) list
  end

  module Path = functor (Weight : WEIGHT with type edge = Topology.Edge.t) ->
  struct
    open Topology

    module WL = struct
      type t = Weight.t
      type edge = P.E.t

      let weight e = Weight.weight ((P.E.label e).EL.label)
      let compare = Weight.compare
      let add = Weight.add
      let zero = Weight.zero
    end

    module Dijkstra = Graph.Path.Dijkstra(P)(WL)

    type weight = Weight.t
    type t = edge list

    let shortest_path (t:Topology.t) (v1:vertex) (v2:vertex) : t option =
      try
        let pth,_ = Dijkstra.shortest_path t.graph v1 v2 in
        Some pth
      with Not_found ->
        None

    exception NegativeCycle of edge list
    (* Implementation of Bellman-Ford algorithm, based on that in ocamlgraph's
       Path library. Returns a hashtable mapping each node to its predecessor in
       the path *)
    let all_shortest_paths (t:Topology.t) (src:vertex) : (vertex VertexHash.t) =
      let size = P.nb_vertex t.graph in
      let dist = VertexHash.create () ~size:size in
      let prev = VertexHash.create () ~size:size in
      let admissible = VertexHash.create () ~size:size in
      VertexHash.set dist src Weight.zero;
      let build_cycle_from x0 =
        let rec traverse_parent x ret =
          let e = VertexHash.find_exn admissible x in
          let s,_ = edge_src e in
          if s = x0 then e :: ret else traverse_parent s (e :: ret) in
        traverse_parent x0 [] in
      let find_cycle x0 =
        let rec visit x visited =
          if VertexSet.mem visited x then
            build_cycle_from x
          else begin
            let e = VertexHash.find_exn admissible x in
            let s,_ = edge_src e in
            visit s (VertexSet.add visited x)
          end
        in
        visit x0 (VertexSet.empty)
      in
      let rec relax (i:int) =
        let update = P.fold_edges_e
          (fun e x ->
            let ev1,_ = edge_src e in
            let ev2,_ = edge_dst e in
            try begin
              let dev1 = VertexHash.find_exn dist ev1 in
              let dev2 = Weight.add dev1 (Weight.weight (Topology.edge_to_label t e)) in
              let improvement =
                try Weight.compare dev2 (VertexHash.find_exn dist ev2) < 0
                with Not_found -> true
              in
              if improvement then begin
                VertexHash.set prev ev2 ev1;
                VertexHash.set dist ev2 dev2;
                VertexHash.set admissible ev2 e;
                Some ev2
              end else x
            end with Not_found -> x) t.graph None in
        match update with
          | Some x ->
            if (phys_equal i (P.nb_vertex t.graph)) then raise (NegativeCycle (find_cycle x))
            else relax (i + 1)
          | None -> prev in
      let r = relax 0 in
      r

    let all_pairs_shortest_paths
      ~(topo:Topology.t)
      ~(f:Topology.vertex -> Topology.vertex -> bool) : (Weight.t * vertex * vertex * edge list) list =
      (* Because Weight does not provide infinity, we lift Weight.t
         using an option: None corresponds to infinity, and Some w
         corresponds to a finite weight. *)
      let add_opt o1 o2 =
        match o1, o2 with
          | Some w1, Some w2 -> Some (Weight.add w1 w2)
          | _ -> None in
      let lt_opt o1 o2 =
        match o1, o2 with
          | Some w1, Some w2 -> Weight.compare w1 w2 < 0
          | Some _, None -> true
          | None, Some _ -> false
          | None, None -> false in
      let make_matrix (g:Topology.t) =
        let n = P.nb_vertex g.graph in
        let vs = vertexes g in
        let nodes = Array.create ~len:n (VertexSet.choose_exn vs) in
        let _ = VertexSet.fold vs ~init:0 ~f:(fun i v -> Array.set nodes i v; i+1) in
        (Array.init n
           (fun i -> Array.init n
             (fun j -> if i = j then (Some Weight.zero, lazy [])
               else
                 try
                   let e = find_edge g nodes.(i) nodes.(j) in
                   let w = Weight.weight (Topology.edge_to_label g e) in
                   (Some w, lazy [e])
                 with Not_found -> (None,lazy []))),
         nodes)
      in
      let matrix,vxs = make_matrix topo in
      let n = P.nb_vertex topo.graph in
      let dist i j = fst (matrix.(i).(j)) in
      (* let path i j = Lazy.force (snd (matrix.(i).(j))) in *)
      (* assumes that !(start = mid && stop = mid) *)
      let path (start : int) (mid : int) (stop : int) =
        if start = mid then
          lazy (find_edge topo vxs.(start) vxs.(stop) ::
                Lazy.force (snd (matrix.(mid).(stop))))
        else if stop = mid then
          lazy (Lazy.force (snd (matrix.(start).(mid))) @
                [find_edge topo vxs.(start) vxs.(stop)])
        else
          lazy (Lazy.force (snd matrix.(start).(mid)) @
                Lazy.force (snd matrix.(mid).(stop))) in
      for k = 0 to n - 1 do
        for i = 0 to n - 1 do
          for j = 0 to n - 1 do
            let dist_ikj = add_opt (dist i k) (dist k j) in
            if lt_opt dist_ikj (dist i j) then
              matrix.(i).(j) <- (dist_ikj, path i k j)
          done
        done
      done;
      let paths = ref [] in
      Array.iteri matrix ~f:(fun i array ->
        Array.iteri array ~f:(fun j elt ->
          match elt with
          | Some w, p when f (vxs.(i)) (vxs.(j)) ->
            paths := (w, vxs.(i), vxs.(j),Lazy.force p) :: !paths
          | _ -> ()
        )
      );
      !paths
  end

  module UnitPath = Path(Topology.UnitWeight)

  (* Parsing *)
  module Parse = struct
    open Topology
    (* TODO(jnf): this could be refactored into a functor that wraps a
       G.t in an arbitrary type and lifts all other G operations over
       that type. *)
    module Build = struct
      module G = struct
        module V = P.V
        module E = P.E
        type vertex = V.t
        type edge = E.t
        type t = Topology.t
        let empty () =
          empty ()
        let remove_vertex t v =
          { t with graph = P.remove_vertex t.graph v }
        let remove_edge t v1 v2 =
          { t with graph = P.remove_edge t.graph v1 v2 }
        let remove_edge_e t e =
          { t with graph = P.remove_edge_e t.graph e }
        let add_vertex t v =
          { t with graph = P.add_vertex t.graph v ;
            node_info = VertexMap.set t.node_info v.Topology.VL.label (v, PortSet.empty) ;
            next_node = v.Topology.VL.id + 1}
        let add_edge t v1 v2 =
          { t with graph = P.add_edge t.graph v1 v2 ; next_edge = t.next_edge + 1}
        let add_edge_e t e =
          let (_,l,_) = e in
          { t with graph = P.add_edge_e t.graph e ;
            next_edge = l.Topology.EL.id + 1}
        let fold_pred_e f t i =
          P.fold_pred_e f t.graph i
        let iter_pred_e f t =
          P.iter_pred_e f t.graph
        let fold_succ_e f t i =
          P.fold_succ_e f t.graph i
        let iter_succ f t v =
          P.iter_succ f t.graph v
        let iter_succ_e f t v =
          P.iter_succ_e f t.graph v
        let iter_edges f t =
          P.iter_edges f t.graph
        let fold_pred f t v i =
          P.fold_pred f t.graph v i
        let fold_succ f t v i =
          P.fold_succ f t.graph v i
        let iter_pred f t v =
          P.iter_pred f t.graph v
        let map_vertex f t =
          { t with graph = P.map_vertex f t.graph }
        let fold_edges_e f t i =
          P.fold_edges_e f t.graph i
        let iter_edges_e f t =
          P.iter_edges_e f t.graph
        let fold_vertex f t i =
          P.fold_vertex f t.graph i
        let fold_edges f t i =
          P.fold_edges f t.graph i
        let iter_vertex f t =
          P.iter_vertex f t.graph
        let pred_e t v  =
          P.pred_e t.graph v
        let succ_e t v =
          P.succ_e t.graph v
        let pred t v =
          P.pred t.graph v
        let succ t v =
          P.succ t.graph v
        let find_all_edges t v1 v2 =
          P.find_all_edges t.graph v1 v2
        let find_edge t v1 v2 =
          P.find_edge t.graph v1 v2
        let mem_edge_e t e =
          P.mem_edge_e t.graph e
        let mem_edge t v1 v2 =
          P.mem_edge t.graph v1 v2
        let mem_vertex t v =
          P.mem_vertex t.graph v
        let in_degree t v =
          P.in_degree t.graph v
        let out_degree t v =
          P.out_degree t.graph v
        let nb_edges t =
          P.nb_edges t.graph
        let nb_vertex t =
          P.nb_vertex t.graph
        let is_empty t =
          P.is_empty t.graph
        let is_directed =
          P.is_directed
      end
      let empty = G.empty
      let remove_vertex = G.remove_vertex
      let remove_edge = G.remove_edge
      let remove_edge_e = G.remove_edge_e
      let add_vertex = G.add_vertex
      let add_edge = G.add_edge
      let add_edge_e = G.add_edge_e
      let copy t = t
    end
    module Dot = Graph.Dot.Parse(Build)(struct
      let get_port o = match o with
        | Some(s) -> begin match s with
            | Graph.Dot_ast.Number(i) -> Scanf.sscanf i "%lu" (fun i -> i)
            | _ -> failwith "Requires number" end
        | None -> failwith "Requires value"
      let next_node = let r = ref 0 in fun _ -> incr r; !r
      let next_edge = let r = ref 0 in fun _ -> incr r; !r
      let node id attrs =
        let open VL in
            { id = next_node ();
              label = Vertex.parse_dot id attrs }
      let edge attrs =
        (* This is a bit of a hack because we only look at the first list of attrs *)
        let ats = List.hd_exn attrs in
        let src,dst,rest = List.fold_left ats ~init:(0l,0l,[])
	  ~f:(fun (src,dst,acc) (k,v) -> match k with
          | Graph.Dot_ast.Ident("src_port") -> (get_port v,dst,acc)
          | Graph.Dot_ast.Ident("dst_port") -> (src, get_port v, acc)
          | _ -> (src,dst,(k,v)::acc)) in
        let attrs' = rest::(List.tl_exn attrs) in
        let open EL in
            { id = next_edge ();
              label = Edge.parse_dot attrs';
              src = src;
              dst = dst }
    end)
    module Gml = Graph.Gml.Parse(Build)(struct
      let next_node = let r = ref 0 in fun _ -> incr r; !r
      let next_edge = let r = ref 0 in fun _ -> incr r; !r
      let node vs =
        let open VL in
            { id = next_node ();
              label = Vertex.parse_gml vs }
      let edge vs =
        let open EL in
            { id = next_edge ();
              label = Edge.parse_gml vs;
              src = 0l;
              dst = 0l }
    end)

    let from_dotfile = Dot.parse
    let from_gmlfile = Gml.parse
  end

  (* Pretty Printing *)
  module Pretty = struct
    open Topology

    let load_file fn =
      In_channel.(with_file fn ~f:input_all)

    let to_dot (t:t) =
      let es = (EdgeSet.fold (edges t) ~init:"" ~f:(fun acc (s,l,d) ->
        let _,src_port = edge_src (s,l,d) in
        let _,dst_port = edge_dst (s,l,d) in
        Printf.sprintf "%s%s%s -> %s {src_port=%lu; dst_port=%lu; %s};"
          acc
          (if acc = "" then "" else "\n")
          (Vertex.to_string s.VL.label)
          (Vertex.to_string d.VL.label)
          src_port
          dst_port
          (Edge.to_dot l.EL.label))) in
      let vs = (VertexSet.fold (vertexes t) ~init:"" ~f:(fun acc v ->
        Printf.sprintf "%s%s\n%s;"
          acc
          (if acc = "" then "" else "\n")
          (Vertex.to_dot v.VL.label)
      )) in
      Printf.sprintf "digraph G {\n%s\n%s\n}\n" vs es

    let to_string (t:t) : string =
      to_dot t

    (* Produce a Mininet script that implements the given topology *)
    let to_mininet
        ?(prologue_file = "static/mn_prologue.txt")
        ?(epilogue_file = "static/mn_epilogue.txt")
        (t:t) : string =
      (* Load static strings (maybe there's a better way to do this?) *)
      let prologue = load_file prologue_file in
      let epilogue = load_file epilogue_file in

      (* Check if an edge or its reverse has been added already *)
      let seen = ref EdgeSet.empty in
      let not_printable e =
        let (src,edge,dst) = e in
        let inverse = match inverse_edge t e with
          | None -> false
          | Some e -> EdgeSet.mem !seen e in
        src = dst ||
        EdgeSet.mem !seen e ||
        inverse
      in

      (* Add the hosts and switches *)
      let add_hosts = fold_vertexes
        (fun v acc ->
          let label = vertex_to_label t v in
          let add = Vertex.to_mininet label in
          acc ^ "    " ^ add
        )
        t "" in

      (* Add links between them *)
      let links = fold_edges
        (fun e acc ->
          let add =
            if (not_printable e) then "" (* Mininet links are bidirectional *)
            else
              let src_vertex,src_port = edge_src e in
              let dst_vertex,dst_port = edge_dst e in
              let src_label = vertex_to_label t src_vertex in
              let dst_label = vertex_to_label t dst_vertex in
              let src = Str.global_replace (Str.regexp "[ ,]") ""
                (Vertex.to_string src_label) in
              let dst = Str.global_replace (Str.regexp "[ ,]") ""
                (Vertex.to_string dst_label) in
              Printf.sprintf "    net.addLink(%s, %s, %ld, %ld)\n"
                src dst src_port dst_port
          in
          seen := EdgeSet.add !seen e;
          acc ^ add
        )
        t "" in
      prologue ^ add_hosts ^ links ^ epilogue

  end
end


(* Utility functions *)
let parse_rate (r:string) : Int64.t =
  let a = Str.search_forward (Str.regexp "\\([0-9]+\\)") r 0 in
  let amt = Str.matched_group 0 r in
  let _ = Str.search_forward (Str.regexp "\\([A-Za-z]+\\)") r a in
  let rate = Str.matched_group 0 r in
  let n = Int64.of_string amt in

  let m = match rate with
    | "bps" -> 1L
    | "Bps" -> 8L
    | "kbps" -> 1024L
    | "kBps" -> 8192L
    | "Mbps" -> 1048576L
    | "MBps" -> 8388608L
    | "Gbps" -> 1073741824L
    | "GBps" -> 8589934592L
    | _ -> failwith "Invalid rate specifier" in
  Int64.(n * m)

let maybe o = match o with
  | Some(s) -> s
  | None -> failwith "Requires value"


(* Convert the generic id type to more specialized types *)

let string_of_id id = match id with
  | Dot_ast.Ident(s) -> s
  | Dot_ast.Number(s) -> "n" ^ s
  | Dot_ast.String(s) -> s
  | Dot_ast.Html(s) -> s

let int32_of_id vo = match maybe vo with
  | Dot_ast.Number(n) -> Int32.of_string n
  | _ -> failwith "Need a number to get int32\n"

let int64_of_id vo = match maybe vo with
  | Dot_ast.Number(n) -> Int64.of_string n
  | _ -> failwith "Need a number to get id\n"

let capacity_of_id vo = match maybe vo with
  | Dot_ast.String(s) -> parse_rate s
  | _ -> failwith "Need a string to get capacity\n"

module Node = struct

  type device = Switch | Host | Middlebox [@@deriving sexp, compare]

  type t = { dev_type : device ;
             dev_id : int64 ;
             ip : int32 ;
             mac : int64 ;
             name : string } [@@deriving sexp, compare]

  type partial_t = { partial_dev_type : device option ;
                     partial_dev_id : int64 option ;
                     partial_ip : int32 option ;
                     partial_mac : int64 option ;
                     partial_name : string option }

  let default = { dev_type = Host ;
                  dev_id = 0L ;
                  name   = "" ;
                  ip     = 0l ;
                  mac    = 0L }

  let partial_default = { partial_dev_type = None ;
                            partial_dev_id = None ;
                            partial_ip     = None ;
                            partial_mac    = None ;
                            partial_name   = None }


  let create (n:string) (i:int64) (d:device) (ip:int32) (mac:int64) : t =
    { dev_type = d ;
      name = n ;
      ip = ip ;
      mac = mac ;
      dev_id = i }

  let name (n:t) : string = n.name
  let id (n:t) : int64 = n.dev_id
  let device (n:t) : device = n.dev_type
  let mac (n:t) : int64 = n.mac
  let ip (n:t) : int32 = n.ip

  let to_string n = n.name

  let to_dot n =
    let devstr = match n.dev_type with
      | Switch -> "switch"
      | Host -> "host"
      | Middlebox -> "middlebox" in
    Printf.sprintf "%s [type=%s, ip=\"%s\", mac=\"%s\", id=%Ld]"
      n.name
      devstr
      (Packet.string_of_ip n.ip)
      (Packet.string_of_mac n.mac)
      (n.dev_id)

  let to_mininet n = match n.dev_type with
    | Host ->
      (* Mininet doesn't like underscores in host names *)
      let mnname = Str.global_replace (Str.regexp "_") "" n.name in
      Printf.sprintf "%s = net.addHost(\'%s\', mac=\'%s\', ip=\'%s\')\n"
        n.name mnname
        (Packet.string_of_mac n.mac) (Packet.string_of_ip n.ip)
    | _ ->
      Printf.sprintf
        "%s = net.addSwitch(\'s%Ld\')\n" n.name n.dev_id


  (* Update the record for a node *)
  let update_dot_attr n (k,vo) =
    let dev_type_of vo = match string_of_id (maybe vo) with
      | "host" -> Host
      | "switch" -> Switch
      | "middlebox" -> Middlebox
      | s -> failwith (Printf.sprintf "Unknown node type: %s\n" s)
    in
    let ip_of vo = match maybe vo with
      | Dot_ast.String(s) -> Packet.ip_of_string s
      | _ -> failwith "IPs must be represented as a string (in quotes)\n" in
    let mac_of vo = match maybe vo with
      | Dot_ast.String(s) -> Packet.mac_of_string s
      | _ -> failwith "MAC must be represented as a string (in quotes)\n" in
    match k with
      | Dot_ast.Ident("type") -> {n with partial_dev_type = Some (dev_type_of vo)}
      | Dot_ast.Ident("id") -> {n with partial_dev_id = Some (int64_of_id vo)}
      | Dot_ast.Ident("ip") -> {n with partial_ip = Some (ip_of vo)}
      | Dot_ast.Ident("mac") -> {n with partial_mac = Some (mac_of vo)}
      | _ -> failwith "Unknown node attribute\n"

  (* Take the partial node record and remove the option types, or
     raise an error if it is not fully filled *)
  let unbox (p:partial_t) : t =
    let unbox_host (p:partial_t) =
      let i = match p.partial_ip with
        | Some i -> i
        | None -> failwith "Host must have an IP address" in
      let m = match p.partial_mac with
        | Some m -> m
        | None -> failwith "Host must have a MAC address" in
      let n = match p.partial_name with
        | Some n -> n
        | None -> failwith "Host must have a name" in
      let id = match p.partial_dev_id with
        | Some i -> i
        | None -> m in
      { dev_type = Host ; dev_id = id ; ip = i ; mac = m ; name = n} in

    let unbox_switch (p:partial_t) =
      let id = match p.partial_dev_id with
        | Some i -> i
        | None -> failwith "Switches must have a unique id" in
      let n = match p.partial_name with
        | Some n -> n
        | None -> failwith "Switch must have a name" in
      let m = match p.partial_mac with
        | Some m -> m
        | None -> 0L in
      let i = match p.partial_ip with
        | Some i -> i
        | None -> 0l in
      { dev_type = Switch ; dev_id = id ; ip = i ; mac = m ; name = n} in

    match p.partial_dev_type with
    | Some Host -> unbox_host p
    | Some Switch -> unbox_switch p
    | Some Middlebox -> unbox_switch p
    | _ -> failwith "Must provide valid devide type for all nodes"


  let parse_dot (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : t =
    let (id, popt) = i in
    let name = string_of_id id in
    let at = List.hd_exn ats in
    let partial = List.fold_left at
      ~init:{partial_default with partial_name = Some name}
      ~f:update_dot_attr in
    unbox partial

  let int64_of_value v = match v with
    | Gml.Int(i) -> Int64.of_int i
    | _ -> failwith "Id requires int value\n"

  let string_of_value v = match v with
    | Gml.String(s) -> s
    | _ -> failwith "Label requires int value\n"

  let update_gml_attr n (key, value) =
    match key with
      | "id" -> {n with dev_id = int64_of_value value}
      | "label" -> {n with name = string_of_value value}
      | "mac" -> {n with mac = Packet.mac_of_string (string_of_value value)}
      | "ip" -> {n with ip = Packet.ip_of_string (string_of_value value)}
      | _ -> n

  let parse_gml (vs:Gml.value_list) : t =
    List.fold_left vs ~init:default ~f:update_gml_attr
end

module Link = struct

  type t = { cost : int64 ;
             capacity : int64 ;
             mutable weight : float } [@@deriving sexp, compare]

  let default = { cost = 1L;
                  capacity = Int64.of_int64 0x7FFFFFFFFFFFFFFFL;
                  weight = 1. }

  let create (cost:int64) (cap:int64) : t =
    { default with cost = cost; capacity = cap }

  let cost (l:t) = l.cost
  let capacity (l:t) = l.capacity

  let weight (l:t) = l.weight
  let set_weight (l:t) (w:float) = l.weight <- w

  let to_string (l:t) : string =
    Printf.sprintf " cost = %s; capacity = %s; "
      (Int64.to_string l.cost)
      (Int64.to_string l.capacity)

  let to_dot = to_string

  let update_dot_attr edge (key,valopt) =
    match key with
      | Dot_ast.Ident("cost") -> {edge with cost = int64_of_id valopt }
      | Dot_ast.Ident("capacity") -> {edge with capacity = capacity_of_id valopt }
      | Dot_ast.Ident(s) -> edge
      | _ -> failwith ("Unknown edge attribute\n")

  let update_gml_attr edge (key, value) = match key with
    | _ -> edge

  let parse_dot (ats:Dot_ast.attr list) : t =
    let at = List.hd_exn ats in
    let link = List.fold_left at ~init:default ~f:update_dot_attr in
    link

  let parse_gml (vs:Gml.value_list) : t =
    let link = List.fold_left vs ~init:default ~f:update_gml_attr in
    link
end

module Weight = struct
  type edge = Link.t [@@deriving sexp]
  type t = float [@@deriving sexp]
  let weight l =
    let open Link in
    l.weight
  let compare = compare
  let add = (+.)
  let zero = 0.
end

module Net = Make(Node)(Link)

module NetPath = Net.Path(Weight)
