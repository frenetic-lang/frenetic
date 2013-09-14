type info = (int * int) * (int * int)

let string_of_info f ((l1,c1),(l2,c2)) =
  if l2=l1 then
    Printf.sprintf "File \"%s\", line %d, characters %d-%d" f l1 c1 c2
  else
    Printf.sprintf "File \"%s\", line %d, character %d, to line %d, character %d" f l1 c1 l2 c2


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

type dotgraph = DotGraph of name * dotstmt list
