open Types

type t
type label = t

type node_record = {
  mutable hash : int option ;
  mutable visited : bool ;
  id : int
}

module NodeHash : Hashtbl.S with type key = t

type attr_tbl = attributes NodeHash.t

val create : int -> t
val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val to_dot : t -> attr_tbl -> string
val to_string : t -> attr_tbl ->  string

val id_of_switch : t -> attr_tbl -> switchId
