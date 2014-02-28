module type VERTEX = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
  val to_dot : t -> string
  val parse_dot : Graph.Dot_ast.node_id -> Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
end

module type EDGE = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
  val to_dot : t -> string
  val parse_dot : Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
  val default : t
end

module type NETWORK = sig
  module Topology : sig
    type t

    type vertex
    type edge
    type port = int64

    module Vertex : VERTEX
    module Edge : EDGE

    module EdgeSet : Set.S
      with type elt = edge

    module VertexSet : Set.S
      with type elt = vertex

    module VertexHash : Hashtbl.S
      with type key = vertex

    module PortSet : Set.S
      with type elt = port

    (* Constructors *)
    val copy : t -> t
    val empty : unit -> t
    val add_vertex : t -> Vertex.t -> (t * vertex)
    val add_edge : t -> vertex -> port -> Edge.t -> vertex -> port -> (t * edge)

    (* Special Accessors *)
    val vertexes : t -> VertexSet.t
    val edges : t -> EdgeSet.t
    val neighbors : t -> vertex -> VertexSet.t
    val vertex_to_ports : t -> vertex -> PortSet.t
    val next_hop : t -> vertex -> port -> edge option
    val edge_src : edge -> (vertex * port)
    val edge_dst : edge -> (vertex * port)

    (* Label Accessors *)
    val vertex_to_label : t -> vertex -> Vertex.t
    val vertex_of_label : t -> Vertex.t -> vertex
    val edge_to_label : t -> edge -> Edge.t

    (* Iterators *)
    val iter_succ : (edge -> unit) -> t -> vertex -> unit
    val iter_vertexes : (vertex -> unit) -> t -> unit
    val iter_edges : (edge -> unit) -> t -> unit
    val fold_vertexes : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_edges : (edge -> 'a -> 'a) -> t -> 'a -> 'a

    (* Mutators *)
    val remove_vertex : t -> vertex -> t
    val remove_edge : t -> edge -> t
    val remove_endpoint : t -> (vertex * port) -> t
  end

  (* Traversals *)
  module Traverse : sig
    val bfs : (Topology.vertex -> unit) -> Topology.t -> unit
    val dfs : (Topology.vertex -> unit) -> Topology.t -> unit
  end

  (* Paths *)
  module Path : sig
    type t = Topology.edge list

    val shortest_path : Topology.t -> Topology.vertex -> Topology.vertex -> t option
  end

  (* Parsing *)
  module Parse : sig
    val from_dotfile : string -> Topology.t
    val from_gmlfile : string -> Topology.t
  end

  (* Pretty Printing *)
  module Pretty : sig
    val to_string : Topology.t -> string
    val to_dot : Topology.t -> string
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
    type port = int64
    module PortSet = Set.Make(Int64)
    module PortMap = Map.Make(Int64)

    module Vertex = Vertex
    module Edge = Edge

    module VL = struct
      type t =
          { id : int;
            label : Vertex.t }
      let compare n1 n2 = Pervasives.compare n1.id n2.id
      let hash n1 = Hashtbl.hash n1.id
      let equal n1 n2 = n1.id = n2.id
    end
    module VertexSet = Set.Make(VL)
    module VertexMap = Map.Make(Vertex)
    module VertexHash = Hashtbl.Make(VL)

    module EL = struct
      type t = { id : int;
                 label : Edge.t;
                 src : port;
                 dst : port }
      let compare e1 e2 = Pervasives.compare e1.id e2.id
      let hash e1 = Hashtbl.hash e1.id
      let equal e1 e2 = e1.id = e2.id
      let default =
        { id = 0;
          label = Edge.default;
          src = 0L;
          dst = 0L }
    end
    module EdgeSet = Set.Make(struct
      type t = VL.t * EL.t * VL.t
      let compare (e1:t) (e2:t) : int =
        let (_,l1,_) = e1 in
        let (_,l2,_) = e2 in
        EL.compare l1 l2
    end)

    module P = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(VL)(EL)

    type vertex = P.vertex

    type edge = P.edge

    type t =
        { graph : P.t;
          node_labels : vertex VertexMap.t;
          next_node : int;
          next_edge : int }

    (* Constructors *)
    let copy (t:t) : t =
      t

    let empty () : t =
      { graph = P.empty;
        node_labels = VertexMap.empty;
        next_node = 0;
        next_edge = 0 }

    let add_vertex (t:t) (l:Vertex.t) : t * vertex =
      let open VL in
      try (t, VertexMap.find l t.node_labels)
      with Not_found ->
        let id = t.next_node + 1 in
        let v = { id = id; label = l } in
        let graph = P.add_vertex t.graph v in
        let node_labels = VertexMap.add l v t.node_labels in
        ({ t with graph; node_labels; next_node = id}, v)

    let add_edge (t:t) (v1:vertex) (p1:port) (l:Edge.t) (v2:vertex) (p2:port) : t * edge =
      let open EL in
      let id = t.next_edge + 1 in
      let l = { id = id; label = l; src = p1; dst = p2 } in
      let e = (v1,l,v2) in
      ({ t with graph = P.add_edge_e t.graph e; next_edge = id }, e)

    (* Special Accessors *)
    let edges (t:t) : EdgeSet.t =
      P.fold_edges_e EdgeSet.add t.graph EdgeSet.empty

    let vertexes (t:t) : VertexSet.t =
      P.fold_vertex VertexSet.add t.graph VertexSet.empty

    let neighbors (t:t) (v:vertex) : VertexSet.t =
      P.fold_succ VertexSet.add t.graph v VertexSet.empty

    let edge_to_label (t:t) (e:edge) : Edge.t =
      let (_,l,_) = e in
      l.EL.label

    let vertex_to_label (t:t) (v:vertex) : Vertex.t =
      v.VL.label

    let vertex_of_label (t:t) (l:Vertex.t) : vertex =
      VertexMap.find l t.node_labels

    let edge_src (e:edge) : (vertex * port) =
      let (v1,l,_) = e in
      (v1, l.EL.src)

    let edge_dst (e:edge) : (vertex * port) =
      let (_,l,v2) = e in
      (v2, l.EL.dst)

    let next_hop (t:t) (v1:vertex) (p:port) : edge option =
      let rec loop es = match es with
        | [] -> None
        | ((_,l,v2) as e)::es' ->
          if l.EL.src = p
            then Some e
            else (loop es') in
      loop (P.succ_e t.graph v1)

    let vertex_to_ports (t:t) (v1:vertex) : PortSet.t =
      List.fold_left
        (fun a e ->
          let _,l,_ = e in
          PortSet.add l.EL.src a)
        PortSet.empty
        (P.succ_e t.graph v1)

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
      let node_labels = VertexMap.remove v.VL.label t.node_labels in
      { t with graph; node_labels }

    let remove_edge (t:t) (e:edge) : t =
      { t with graph = P.remove_edge_e t.graph e }

    let remove_endpoint (t:t) (ep : vertex * port) : t =
      fold_edges (fun e acc ->
        if edge_src e = ep || edge_dst e = ep
          then remove_edge acc e
          else acc)
        t t

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

  (* Paths *)
  module Path = struct
    open Topology
    module UnitWeight = struct
      type label = EL.t
      type t = int
      let weight _ = 1
      let compare = Pervasives.compare
      let add = (+)
      let zero = 0
    end

    module Dijkstra = Graph.Path.Dijkstra(P)(UnitWeight)
    type t = edge list

    let shortest_path (t:Topology.t) (v1:vertex) (v2:vertex) : t option =
      try
        let pth,_ = Dijkstra.shortest_path t.graph v1 v2 in
        Some pth
      with Not_found ->
        None
  end

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
          { t with graph = P.add_vertex t.graph v }
        let add_edge t v1 v2 =
          { t with graph = P.add_edge t.graph v1 v2 }
        let add_edge_e t e =
          { t with graph = P.add_edge_e t.graph e }
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
            | Graph.Dot_ast.Number(i) -> Int64.of_string i
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
        let ats = List.hd attrs in
        let src,dst,rest = List.fold_left (fun (src,dst,acc) (k,v) -> match k with
          | Graph.Dot_ast.Ident("sport") -> (get_port v,dst,acc)
          | Graph.Dot_ast.Ident("dport") -> (src, get_port v, acc)
          | _ -> (src,dst,(k,v)::acc)
        ) (0L,0L,[]) ats in
        let attrs' = rest::(List.tl attrs) in
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
              src = 0L;
              dst = 0L }
    end)

    let from_dotfile = Dot.parse
    let from_gmlfile = Gml.parse
  end

  (* Pretty Printing *)
  module Pretty = struct
    open Topology
    let to_dot (t:t) =
      let es = (EdgeSet.fold (fun (s,l,d) acc ->
        let _,sport = edge_src (s,l,d) in
        let _,dport = edge_dst (s,l,d) in
        Printf.sprintf "%s%s%s -> %s {sport=%Ld; dport=%Ld; %s}"
          acc
          (if acc = "" then "" else "\n")
          (Vertex.to_string s.VL.label)
          (Vertex.to_string d.VL.label)
          sport
          dport
          (Edge.to_dot l.EL.label))
                  (edges t) "") in
      let vs = (VertexSet.fold (fun v acc ->
        Printf.sprintf "%s%s\n%s"
          acc
          (if acc = "" then "" else "\n")
          (Vertex.to_dot v.VL.label)
      ) (vertexes t) "") in
      Printf.sprintf "digraph G {\n%s\n%s\n}" vs es

    let to_string (t:t) : string =
      to_dot t
  end
end

