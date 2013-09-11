open NetCore_Util
open NetCore_Types
open Graph

module type NODE =
sig
  type t = Host of string
           | Switch of string * switchId
           | Mbox of string * string list

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
  val mk_link : v -> int -> v -> int -> int64 -> int64 -> e
  val reverse : e -> e

  (* Accesssors *)
  val src : e -> v
  val dst : e -> v
  val label : e -> t

  val capacity : e -> Int64.t
  val cost : e -> Int64.t
  val srcport : e -> Int32.t
  val dstport : e -> Int32.t

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
  val add_host : t -> string -> t
  val add_switch : t -> string -> switchId -> t
  val add_switch_edge : t -> V.t -> portId -> V.t -> portId -> t

  (* Accessors *)
  val get_vertices : t -> V.t list
  val get_edges : t -> E.t list
  val get_ports : t -> V.t -> V.t -> (portId * portId)
  val get_hosts : t -> V.t list
  val get_switches : t -> V.t list
  val get_switchids : t -> switchId list
  val ports_of_switch : t -> V.t -> portId list
  val edge_ports_of_switch : t -> V.t -> portId list
  val next_hop : t -> V.t -> portId -> V.t

  (* Utility functions *)
  val shortest_path : t -> V.t -> V.t -> E.t list
  val to_dot : t -> string
  val to_string : t -> string
  val to_mininet : t -> string

  (* Exceptions *)
  exception NotFound of string
  exception NoPath of string * string
end

module Node : NODE
module Link : LINK with type v = Node.t
module Topology : TOPO with type V.t = Node.t and type E.t = Link.e

module EdgeOrd : OrderedType with type t = Link.e
module EdgeSet : Setplus.S
module EdgeMap : Mapplus.S with type key = Link.e
