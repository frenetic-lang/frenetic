type info = (int * int) * (int * int)

val string_of_info : info -> string

module StringSet : Set.S with type elt = string 
module StringMap : Map.S with type key = string

module Int32Set : Set.S with type elt = Int32.t
module Int32Map : Map.S with type key = Int32.t
module Int64Map : Map.S with type key = Int64.t

type switchId = int64
type portId = int32

type rate = Rate of int64 * int64

val string_of_rate : rate -> string

(* AST for DOT format prior to turning it to a graph *)
type name = string

type eattr_t =
  | SPort of portId
  | DPort of portId
  | Label of string
  | Cost of int64
  | Capacity of int64

type nattr_t =
  | Kind of string
  | Id of int64

type eattr = {
  sport : portId
  ; dport : portId
  ; label : string
  ; cost : int64
  ; capacity : int64
}

type nattr = {
  kind: string
  ; id : int64
}
type dotstmt =
| DotNode of name * nattr
| DotEdge of name * name * eattr
| DotDiedge of name * name * eattr

type dotgraph =
  | DotGraph of name * dotstmt list
  | DotDigraph of name * dotstmt list
