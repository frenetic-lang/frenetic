open Graph

module G : Sig.P with type V.t = Node.t and type E.t = Link.e
                 and type V.label = Node.label and type E.label = Link.t
module V : Sig.VERTEX with type t = Node.t
module E : Sig.EDGE with type t = Link.e and type vertex = Node.t
type t = G.t * Node.attr_tbl

(* Constructors *)
val empty : unit -> t
val add_node : t -> V.t -> t
val add_host : t -> string -> Packet.dlAddr -> Packet.nwAddr -> int -> t
val add_switch : t -> Node.switchId -> int -> t
val add_switch_edge : t -> V.t -> Node.portId -> V.t -> Node.portId -> t
val add_edge_e : t -> E.t -> t

(* Accessors *)
val get_vertices : t -> V.t list
val get_edges : t -> E.t list
val get_ports : t -> V.t -> V.t -> (Node.portId * Node.portId)
val get_hosts : t -> V.t list
val get_switches : t -> V.t list
val get_switchids : t -> Node.switchId list
val unit_cost : t -> t
val ports_of_switch : t -> V.t -> Node.portId list
val next_hop : t -> V.t -> Node.portId -> V.t
val find_edge : t -> V.t -> V.t -> E.t

(* Iterators *)
val iter_succ : (V.t -> unit) -> t -> V.t -> unit
val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a

(* Utility functions *)
val nb_vertex : t -> int
val spanningtree : t -> G.t
val all_paths :t -> Node.t -> Node.t Node.NodeHash.t
val shortest_path : t -> V.t -> V.t -> E.t list
val all_shortest_paths : t -> Node.t
  -> (int64 Node.NodeHash.t * Node.t Node.NodeHash.t)

val stitch : E.t list -> (Node.portId option * V.t * Node.portId option) list
val floyd_warshall : t -> ((V.t * V.t) * V.t list) list
val to_dot : t -> string
val to_string : t -> string
val to_mininet : t -> string

(* Exceptions *)
exception NotFound of string
exception NoPath of string * string
