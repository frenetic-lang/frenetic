open Graph

type switchId = int64
type portId = int64
type rate = Rate of int64 * int64

val string_of_rate : rate -> string


module type NODE =
sig
  type t = Host of string * Packet.dlAddr * Packet.nwAddr
           | Switch of switchId
           | Mbox of string * string list

  type label = t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_dot : t -> string
  val to_string : t -> string
  val id_of_switch : t -> switchId
end

module type LINK =
sig
  type v
  type t = {
    srcport : portId;
    dstport : portId;
    cost : int64;
    capacity : int64;
  }
  type e = (v * t * v)
  val default : t
  val compare : t -> t -> int

  (* Constructors *)
  val mk_edge : v -> v -> t -> e
  val mk_link : v -> portId -> v -> portId -> int64 -> int64 -> e
  val reverse : e -> e

  (* Accesssors *)
  val src : e -> v
  val dst : e -> v
  val label : e -> t

  val capacity : e -> int64
  val cost : e -> int64
  val srcport : e -> portId
  val dstport : e -> portId

  (* Utilities *)
  val name : e -> string
  val string_of_label : e -> string
  val to_dot : e -> string
  val to_string : e -> string
end

module type TOPO =
sig
  include Sig.P

  (* Constructors *)
  val add_node : t -> V.t -> t
  val add_host : t -> string -> Packet.dlAddr -> Packet.nwAddr -> t
  val add_switch : t -> switchId -> t
  val add_switch_edge : t -> V.t -> portId -> V.t -> portId -> t

  (* Accessors *)
  val get_vertices : t -> V.t list
  val get_edges : t -> E.t list
  val get_ports : t -> V.t -> V.t -> (portId * portId)
  val get_hosts : t -> V.t list
  val get_switches : t -> V.t list
  val get_switchids : t -> switchId list
  val unit_cost : t -> t
  val ports_of_switch : t -> V.t -> portId list
  (* TODO(basus): remove this? *)
  (* val edge_ports_of_switch : t -> V.t -> portId list *)
  val next_hop : t -> V.t -> portId -> V.t

  (* Utility functions *)
  val spanningtree : t -> t
  val shortest_path : t -> V.t -> V.t -> E.t list
  val stitch : E.t list -> (portId option * V.t * portId option) list
  val floyd_warshall : t -> ((V.t * V.t) * V.t list) list
  val to_dot : t -> string
  val to_string : t -> string
  val to_mininet : t -> string

  (* Exceptions *)
  exception NotFound of string
  exception NoPath of string * string
end

module Node : NODE
module Link : LINK with type v = Node.t
module Topology : TOPO with type V.t = Node.t and type E.t = Link.e and type V.label
  = Node.label and type E.label = Link.t

module EdgeOrd : Set.OrderedType with type t = Link.e
module EdgeSet : Set.S
module EdgeMap : Map.S with type key = Link.e
