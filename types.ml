open Util
type info = (int * int) * (int * int)

let string_of_info ((l1,c1),(l2,c2)) =
  if l2=l1 then
    Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
  else
    Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2

module StringOrd = struct
  type t = string
  let compare = Pervasives.compare
end

module StringSet = Setplus.Make(StringOrd)
module StringMap = Mapplus.Make(StringOrd)

module Int32Set = Setplus.Make(Int32)
module Int32Map = Mapplus.Make(Int32)
module Int64Map = Mapplus.Make(Int64)

type switchId = int64
type portId = int32

type rate = Rate of int64 * int64

let string_of_rate r =
  let Rate(min,max) = r in
  Printf.sprintf "min(%Ld Bps) max(%Ld Bps)" min max

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
