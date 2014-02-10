open Types
open Node

type v = Node.t
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
val name : e -> Node.attr_tbl -> string
val string_of_label : e -> string
val to_dot : e -> Node.attr_tbl -> string
val to_string : e -> Node.attr_tbl -> string
