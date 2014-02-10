open Types
open Graph

module G : Sig.P with type V.t = Node.t and type E.t = Link.e
                 and type V.label = Node.label and type E.label = Link.t
module V : Sig.VERTEX
module E : Sig.EDGE
type t= G.t * Node.attr_tbl

(* Constructors *)
val add_node : t -> V.t -> t
val add_host : t -> string -> Packet.dlAddr -> Packet.nwAddr -> int -> t
val add_switch : t -> switchId -> int -> t
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
val next_hop : t -> V.t -> portId -> V.t

  (* Utility functions *)
val spanningtree : t -> G.t
val shortest_path : t -> V.t -> V.t -> E.t list
(* val shortest_path_v : t -> V.t -> V.t -> V.t list *)
val stitch : E.t list -> (portId option * V.t * portId option) list
val floyd_warshall : t -> ((V.t * V.t) * V.t list) list
val to_dot : t -> string
val to_string : t -> string
val to_mininet : t -> string

(* Parsers *)

(* val from_dotfile_tbl : string -> (Topology.t * *)
(*                                     (string, attribute) Hashtbl.t * *)
(*                                     (switchId, attribute) Hashtbl.t) *)
(* val from_dotfile : string -> Topology.t *)
(* val from_gmlfile : string -> Topology.t *)

(* Exceptions *)
exception NotFound of string
exception NoPath of string * string
