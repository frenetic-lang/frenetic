open Util
type info = (int * int) * (int * int)

let string_of_info ((l1,c1),(l2,c2)) =
  if l2=l1 then
    Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
  else
    Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module Int32Set = Set.Make(Int32)
module Int32Map = Map.Make(Int32)
module Int64Map = Map.Make(Int64)

type switchId = int64
type portId = int32

type rate = Rate of int64 * int64

let string_of_rate r =
  let Rate(min,max) = r in
  Printf.sprintf "min(%Ld Bps) max(%Ld Bps)" min max

type nattr = {
  ntype: string
  ; id : int64
  ; ip : string
}

let defnattr = {ntype = "host"; id = 0L; ip = "0.0.0.0"}
