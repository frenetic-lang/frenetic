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

    val copy : t -> t

    (* Constructors *)
    val empty : unit -> t
    val add_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_vertex : t -> vertex -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t

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

module Make : MAKE
