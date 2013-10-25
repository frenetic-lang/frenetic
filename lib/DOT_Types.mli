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

type nattr = {
  ntype: string
  ; name : string
  ; id : int64
  ; ip : string
}

val defnattr : nattr
