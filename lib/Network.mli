module type VERTEX = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
  val parse_dot : Graph.Dot_ast.node_id -> Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
end

module type EDGE = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
  val parse_dot : Graph.Dot_ast.attr list -> t
  val parse_gml : Graph.Gml.value_list -> t
  val default : t
end

module type NETWORK = sig
  module Topology : sig
    type t

    type vertex
    type edge
    type port = int32

    module Vertex : VERTEX
    module Edge : EDGE

    module EdgeSet : Set.S
      with type elt = edge
      
    module VertexSet : Set.S
      with type elt = vertex
      
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
    val endpoints :  edge -> (vertex * port * vertex * port)

    (* Label Accessors *)
    val vertex_to_label : t -> vertex -> Vertex.t
    val edge_to_label : t -> edge -> Edge.t

    (* Iterators *)
    val iter_succ : (edge -> unit) -> t -> vertex -> unit
    val iter_vertexes : (vertex -> unit) -> t -> unit
    val iter_edges : (edge -> unit) -> t -> unit
    val fold_vertexes : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_edges : (edge -> 'a -> 'a) -> t -> 'a -> 'a
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

module Make : MAKE
