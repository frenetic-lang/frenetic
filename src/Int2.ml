(** Pairs of integers  *)
open! Core

module T = struct
  type t = int*int [@@deriving sexp, hash, compare]
end
include T
module Table = Hashtbl.Make(T)
