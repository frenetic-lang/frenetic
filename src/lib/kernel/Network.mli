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


(* Concrete instantations of above functors and module signatures *)
module Make : MAKE


(* Instances *)

module Node : sig
  include VERTEX
  type device = Switch | Host | Middlebox [@@deriving sexp]

  val default : t
  val create : string -> int64 -> device -> int32 -> int64 -> t
  val name : t -> string
  val id : t -> int64
  val device : t -> device
  val mac : t -> int64
  val ip : t -> int32

end

module Link : sig
  include EDGE

  val create : int64 -> int64 -> t
  val cost : t -> int64
  val capacity : t -> int64

  val weight : t -> float
  val set_weight : t -> float -> unit

end

module Net : NETWORK 
  with type Topology.Vertex.t = Node.t
  and type Topology.Edge.t = Link.t

module NetPath : Net.PATH 
  with type weight = float
