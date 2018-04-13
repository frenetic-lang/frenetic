open Core

type 'field pred =
  | True
  | False
  | Test of 'field * int
  | And of 'field pred * 'field pred
  | Or of 'field pred * 'field pred
  | Neg of 'field pred
  [@@deriving sexp, compare, hash]

type 'field  pol =
  | Dup
  | Filter of 'field pred
  | Modify of 'field * int
  | Seq of 'field pol * 'field pol
  | Union of 'field pol * 'field pol
  | Star of 'field pol
  [@@deriving sexp, compare, hash]
