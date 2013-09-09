open NetCore_Util

module type Vertex =
sig
  type t
  val compare : t -> t -> int
  val to_dot : t -> string
  val to_string : t -> string
end

module type DirectedEdge =
sig
  module V : Vertex
  type t
  type l
  type v = V.t
  val compare : t -> t -> int
  val src : t -> v
  val dst : t -> v
  val label : t -> l
  val mk_edge : v -> v -> l -> t
  val to_dot : t -> string
  val to_string : t -> string
end

module type DirectedGraph =
sig
  module type S =
  sig
    type t
    type v
    type e
    type l

    val create : unit -> t
    val add_vertex : t -> v -> t
    val add_vertices : t -> v list -> t
    val add_edge : t -> e -> t
    val add_edges : t -> e list -> t
    val merge : t -> t -> t

    (* Accessors *)
    val num_vertices : t -> int
    val has_vertex : t -> v -> bool
    val get_vertices : t -> v list
    val num_edges : t -> int
    val has_edge : t -> e -> bool
    val get_edges : t -> e list
    val get_nbrs : t -> v -> v list
    val incoming : t -> v -> e list
    val outgoing : t -> v -> e list

    (* Deletion functions *)
    val del_edge : t -> v -> v -> t
    val del_edges : t -> (v * v) list -> t
    val del_vertex : t -> v -> t

    (* Utility functions *)
    val copy : t -> t
    val to_dot : t -> string
    val to_string : t -> string
  end
  module Make (E:DirectedEdge) : S with type v = E.V.t with type e = E.t
end

(* Polymorphic directed graph implementation *)
module Digraph : DirectedGraph =
struct
  module type S =
  sig
    type t                                (* Abstract graph type *)
    type v
    type e
    type l

    (* Constructors *)
    val create : unit -> t
    val add_vertex : t -> v -> t
    val add_vertices : t -> v list -> t
    val add_edge : t -> e -> t
    val add_edges : t -> e list -> t
    val merge : t -> t -> t

    (* Accessors *)
    val num_vertices : t -> int
    val has_vertex : t -> v -> bool
    val get_vertices : t -> v list
    val num_edges : t -> int
    val has_edge : t -> e -> bool
    val get_edges : t -> e list
    val get_nbrs : t -> v -> v list
    val incoming : t -> v -> e list
    val outgoing : t -> v -> e list

    (* Deletion functions *)
    val del_edge : t -> v -> v -> t
    val del_edges : t -> (v * v) list -> t
    val del_vertex : t -> v -> t

    (* Utility functions *)
    val copy : t -> t
    val to_dot : t -> string
    val to_string : t -> string
  end

  module Make (E:DirectedEdge)  =
  struct
    module type S = S
    type v = E.V.t
    type e = E.t
    type l = E.l

    module VertexOrd = struct
      type t = v
      let compare = Pervasives.compare
    end

    module VertexSet = Setplus.Make(VertexOrd)

    module EdgeOrd = struct
      type t = e
      let compare = Pervasives.compare
    end

    module EdgeSet = Setplus.Make(EdgeOrd)

    type t = Graph of VertexSet.t * EdgeSet.t

    (* Constructor functions *)
    let create _ = Graph(VertexSet.empty, EdgeSet.empty)

    let add_vertex g v =
      match g with Graph(vs,es) -> let vs' = VertexSet.add v vs in Graph(vs',es)

    let add_vertices g vl =
      List.fold_left add_vertex g vl

    let add_edge g e =
      match g with Graph(vs,es) ->
        let v1 = E.src e in
        let v2 = E.dst e in
        let vs' = VertexSet.add v2 (VertexSet.add v1 vs) in
        let es' = EdgeSet.add e es in
        Graph(vs',es')

    let add_edges g el =
      List.fold_left add_edge g el

    let merge g1 g2 =
      let Graph(vs1,es1) = g1 in
      let Graph(vs2,es2) = g2 in
      Graph(VertexSet.union vs1 vs2, EdgeSet.union es1 es2)

    (* Accessors *)
    let get_vertices g =
      match g with Graph(vs,es) -> VertexSet.elements vs
    let has_vertex g v =
      match g with Graph(vs,es) -> VertexSet.mem v vs
    let num_vertices g =
      match g with Graph(vs,es) -> VertexSet.cardinal vs

    let get_edges g =
      match g with Graph(vs,es) -> EdgeSet.elements es
    let has_edge g e =
      match g with Graph(vs,es) -> EdgeSet.mem e es
    let num_edges g =
      match g with Graph(vs,es) -> EdgeSet.cardinal es

    let get_nbrs g v =
      match g with Graph(vs,es) ->
        EdgeSet.fold (fun e vs' -> if E.src e = v then (E.dst e)::vs' else vs')
          es []

    let incoming g v =
      match g with Graph(_, es) ->
        let es' = EdgeSet.filter (fun e -> E.dst e = v) es in
        EdgeSet.elements es'
    let outgoing g v =
      match g with Graph(_, es) ->
        let es' = EdgeSet.filter (fun e -> E.src e = v) es in
        EdgeSet.elements es'

    (* Deletion functions *)
    let del_edge g v1 v2 =
      match g with Graph(vs,es) ->
        let es' = EdgeSet.filter (fun e -> E.src e <> v1 && E.dst e <> v2) es in
        Graph(vs, es')

    let del_edges g vvl =
      List.fold_left
        (fun g' (v1,v2) -> del_edge g' v1 v2) g vvl

    let del_vertex g v =
      match g with Graph(vs,es) ->
        let vs' = VertexSet.remove v vs in
        let es' = EdgeSet.filter
          (fun e -> E.src e <> v && E.dst e <> v) es in
        Graph(vs',es')


    (* Utility functions *)
    let copy g = g
    let to_dot g =
      match g with Graph(vertices,edges) ->
        let es = EdgeSet.intercalate E.to_dot "\n" edges in
        Printf.sprintf "digraph G {\n%s\n}" es
    let to_string = to_dot
  (* exception NoPath of string * string *)
  (* exception NotFound of string *)
  end
end
