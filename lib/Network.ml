module type VERTEX = sig
  type t

  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
  val parse_dot : Graph.Dot_ast.node_id -> Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
end

module type EDGE = sig
  type t

  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
  val parse_dot : Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
  val default : t
end

module type MAKE = functor (Vertex:VERTEX) -> functor (Edge:EDGE) ->
sig
  module Topology : sig 
    type t

    type vertex
    type edge
    type port = int32

    module EdgeSet : Set.S
      with type elt = edge
      
    module VertexSet : Set.S
      with type elt = vertex
      
    module PortSet : Set.S
      with type elt = port
    
    module G : Graph.Sig.G


    (* Constructors *)
    val copy : t -> t
    val empty : unit -> t
    val add_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_vertex : t -> vertex -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t
    (* Special Constructors *)


    (* Special Accessors *)
    val vertexes : t -> VertexSet.t
    val edges : t -> EdgeSet.t
    val vertex_to_ports : vertex -> PortSet.t
    val next_hop : t -> vertex -> port -> vertex option
    val endpoints : t -> edge -> (vertex * port * vertex * port)

    (* Label Accessors *)
    val vertex_to_label : t -> vertex -> Vertex.t
    val edge_to_label : t -> edge -> Edge.t

    (* Iterators *)
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val iter_vertex : (vertex -> unit) -> t -> unit
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a

    (* Traversals *)
    val bfs : (vertex -> unit) -> t -> unit
    val dfs : ?pre:(vertex -> unit) -> ?post:(vertex -> unit) -> t -> unit
  end
  
  (* String Representations *)
  module Pretty : sig
    val to_string : Topology.t -> string
    val to_dot : Topology.t -> string
  end
    
  module Path : sig
    type t = Topology.edge list
        
    val shortest_path : Topology.t -> Topology.vertex -> Topology.vertex -> t option
  end

  module Parse : sig
    val from_dotfile : string -> Topology.t
    val from_gmlfile : string -> Topology.t
  end
end

module Make : MAKE = 
  functor (Vertex:VERTEX) -> 
    functor (Edge:EDGE) -> 
struct
  module Topology = struct
      
    type port = int32 
    module PortSet = Set.Make(Int32)
    module PortMap = Map.Make(Int32)
      
    module VL = struct
      type t = 
          { id : int;
            label : Vertex.t }
      let compare n1 n2 = Pervasives.compare n1.id n2.id 
      let hash n1 = Hashtbl.hash n1.id
      let equal n1 n2 = n1.label = n2.label
    end
    module VertexSet = Set.Make(VL)
      
    module EL = struct
      type t = { id : int;
                 label : Edge.t; 
                 src : port;
                 dst : port } 
      let compare e1 e2 = 
        Pervasives.compare e1.id e2.id 
      let hash e1 = 
        Hashtbl.hash e1.id 
      let equal e1 e2 = 
        e1.label = e2.label
      let default = 
        { id = 0;
          label = Edge.default; 
          src = 0l; 
          dst = 0l }
    end
    module EdgeSet = Set.Make(struct 
      type t = VL.t * EL.t * VL.t
      let compare (e1:t) (e2:t) : int = 
        let (_,l1,_) = e1 in 
        let (_,l2,_) = e2 in 
        EL.compare l1 l2
    end)
      
    module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(VL)(EL)
      
    type vertex = G.vertex

    type edge = G.edge

    type t = G.t

    (* Constructors *)
    let copy (t:t) : t = 
      t
        
    let empty () : t = 
      G.empty
                    
    let add_vertex (t:t) (v:vertex) : t = 
      G.add_vertex t v
        
    let add_edge (t:t) (v1:vertex) (v2:vertex) : t = 
      G.add_edge t v1 v2

    let add_edge_e (t:t) (e:edge) : t = 
      G.add_edge_e t e

    let remove_vertex (t:t) (v:vertex) : t = 
      G.remove_vertex t v

    let remove_edge (t:t) (v1:vertex) (v2:vertex) : t = 
      G.remove_edge t v1 v2 

    let remove_edge_e (t:t) (e:edge) : t = 
      G.remove_edge_e t e

    (* Special Constructors *)
    let add_edge_p (t:t) (v1:vertex) (p1:port) (v2:vertex) (p2:port) : t = 
      let open EL in 
      let l = { default with id = G.nb_edges t; src = p1; dst = p2 } in 
      let e = (v1,l,v2) in 
      add_edge_e t e

    (* Accessors *)
    let fold_vertex (f:vertex -> 'a -> 'a) (t:t) (init:'a) : 'a = 
      G.fold_vertex f t init

    let fold_edges_e (f:edge -> 'a -> 'a) (t:t) (init:'a) : 'a = 
      G.fold_edges_e f t init

    let iter_vertex (f:vertex -> unit) (t:t) : unit =
      G.iter_vertex f t

    let iter_edges_e (f:edge -> unit) (t:t) : unit = 
      G.iter_edges_e f t

    let iter_succ_e (f:edge -> unit) (t:t) (v:vertex) : unit = 
      G.iter_succ_e f t v

    (* Special Accessors *)
    let edges (t:t) : EdgeSet.t = 
      G.fold_edges_e EdgeSet.add t EdgeSet.empty

    let vertexes (t:t) : VertexSet.t = 
      G.fold_vertex VertexSet.add t VertexSet.empty
        
    let edge_to_label (t:t) (e:edge) : Edge.t = 
      let (_,l,_) = e in 
      l.EL.label 
        
    let vertex_to_label (t:t) (v:vertex) : Vertex.t = 
      v.VL.label 
        
    let endpoints (t:t) (e:edge) : (vertex * port * vertex * port) = 
      let (v1,l,v2) = e in 
      let p1 = l.EL.src in 
      let p2 = l.EL.dst in 
      (v1,p1,v2,p2)

    let next_hop (t:t) (v1:vertex) (p:port) : vertex option = 
      List.fold_left
        (fun a e -> 
          match a,e with 
            | Some _,_ -> a
            | None,(_,l,v2) -> 
              if l.EL.src = p then Some v2
              else a)
        None
        (G.succ_e t v1)

    let vertex_to_ports (v:vertex) : PortSet.t = 
      v.VL.ports 

    (* Traversals *)
    module Bfs = Graph.Traverse.Bfs(G)
    module Dfs = Graph.Traverse.Dfs(G)
      
    let bfs = Bfs.iter      
    let dfs = Dfs.iter
  end

  module Pretty = struct 
    open Topology
    let to_dot (t:t) =
      Printf.sprintf "digraph G {\n%s\n}"
        (EdgeSet.fold
           (fun (v1,l,v2) acc -> 
             Printf.sprintf "%s%s%d -> %d" 
               acc 
               (if acc = "" then "" else "\n") 
               v1.VL.id 
               v2.VL.id)
           (edges t)
           "")
    
    let to_string (t:t) : string = 
      to_dot t
  end      

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

    module Dijkstra = Graph.Path.Dijkstra(G)(UnitWeight)
      
    type t = edge list
      
    let shortest_path (t:Topology.t) (v1:vertex) (v2:vertex) : t option = 
      try 
        let pth,_ = Dijkstra.shortest_path t v1 v2 in 
        Some pth
      with Not_found -> 
        None
  end

  module Parse = struct
    module Dot = Graph.Dot.Parse(Topology)(struct
      let next_node = let r = ref 0 in fun _ -> incr r; !r 
      let next_edge = let r = ref 0 in fun _ -> incr r; !r         
      let node id attrs = 
        let open Topology.VL in 
        { id = next_node ();
          label = Vertex.parse_dot id attrs }
      let edge attrs = 
        let open Topology.EL in 
        { id = next_edge ();
          label = Edge.parse_dot attrs;
          src = 0l;
          dst = 0l }
    end)
    module Gml = Graph.Gml.Parse(Topology)(struct
      let next_node = let r = ref 0 in fun _ -> incr r; !r 
      let next_edge = let r = ref 0 in fun _ -> incr r; !r         
      let node vs = 
        let open Topology.VL in   
        { id = next_node ();
          label = Vertex.parse_gml vs }
      let edge vs = 
        let open Topology.EL in 
        { id = next_edge ();
          label = Edge.parse_gml vs;
          src = 0l; 
          dst = 0l }
    end)
      
    let from_dotfile = Dot.parse
    let from_gmlfile = Gml.parse
  end
end
