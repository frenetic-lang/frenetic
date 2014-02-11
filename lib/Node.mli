open Packet

type switchId = int64
type portId = int64

type node = Switch | Host | Middlebox

type attributes = {
  node_type : node
  ; name : string
  ; ip : nwAddr
  ; mac : dlAddr
  ; dev_id : int64
}

val default : attributes

type node_record = {
  mutable hash : int option ;
  mutable visited : bool ;
  id : int
}

type t
type label = t

module NodeHash : Hashtbl.S with type key = t

type attr_tbl = attributes NodeHash.t

val create : int -> t
val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val to_dot : t -> attr_tbl -> string
val to_string : t -> attr_tbl ->  string
val visited : t -> bool
val visit : t -> unit
val leave : t -> unit

val id_of_switch : t -> attr_tbl -> switchId
