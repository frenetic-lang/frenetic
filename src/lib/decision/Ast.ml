open Core

type 'field  pol =
  | Skip
  | Drop
  | TestEq of 'field * int
  | TestNeq of 'field * int
  | Modify of 'field * int
  | Dup
  | Seq of 'field pol * 'field pol
  | Union of 'field pol * 'field pol
  | Star of 'field pol
  [@@deriving sexp, compare, hash, show]

type 'field formula =
  | Equiv of 'field pol * 'field pol
  | Nequiv of 'field pol * 'field pol
  | Leq of 'field pol * 'field pol
  | Geq of 'field pol * 'field pol
  [@@deriving sexp, compare, hash, show]

let negate (p : 'field pol) : 'field pol = failwith "Not implemented"
