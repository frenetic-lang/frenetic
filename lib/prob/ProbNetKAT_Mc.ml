open Core.Std
(* open Lacaml.D *)
open ProbNetKAT

module Dense = Owl.Dense.Matrix.D
module Sparse = Owl.Sparse.Matrix.D

module Make(Nom : ProbNetKAT_Packet_Repr.S) = struct
  include Nom
  let (n, empty) = (Index.max.i + 1, Index.max.i + 1)

  (*   let rec of_pol p : Mat.t =
    (* FIXME: allocate matrices statically *)
    match p with
    | Skip -> Mat.identity n
    | Drop -> Mat.init_cols n n (fun _ c -> if c = empty then 1.0 else 0.0)
    | TestEq (f, v) ->
      Mat.init_rows n n (fun r c ->
        (** FIXME: recomputing test every time is inefficient *)
        if test f v r then
          if c = r then 1.0 else 0.0
        else
          if c = empty then 1.0 else 0.0)
    | TestNeq (f, v) ->
      Mat.init_rows n n (fun r c ->
        (** FIXME: recomputing test every time is inefficient *)
        if test f v r then
          if c = empty then 1.0 else 0.0
        else
          if c = r then 1.0 else 0.0)
    | Modify (f, v) ->
      (** FIXME: recomputing modify every time is inefficient *)
      Mat.init_rows n n (fun r c -> if modify f v r = c then 1.0 else 0.0)
    | Seq (p, q) -> gemm (of_pol p) (of_pol q)
    | Ite (a, p, q) ->
      let (a, p, q) = (of_pol a, of_pol p, of_pol q) in
      (* FIXME: incorrect  *)
      Mat.add (gemm a p) (gemm a q)
    | While (a,p) -> loop (of_pol a) (of_pol p)

  and loop a p =
    (* reorder indices such that exactly the first k packets satisfy a *)
    (* let (k, swaps) = normal_form a p in *)
    (* let fundamental = Mat.sub (Mat.identity k) p *)
    (* let absorption_matrix = gemm ~m:n ~n  *)
    p *)


(*   let rec of_pol p : Mat.t =
    (* FIXME: allocate matrices statically *)
    match p with
    | Skip -> Mat.identity n
    | Drop -> Mat.init_cols n n (fun _ c -> if c = empty then 1.0 else 0.0)
    | TestEq (f, v) ->
      Mat.init_rows n n (fun r c ->
        (** FIXME: recomputing test every time is inefficient *)
        if test f v r then
          if c = r then 1.0 else 0.0
        else
          if c = empty then 1.0 else 0.0)
    | TestNeq (f, v) ->
      Mat.init_rows n n (fun r c ->
        (** FIXME: recomputing test every time is inefficient *)
        if test f v r then
          if c = empty then 1.0 else 0.0
        else
          if c = r then 1.0 else 0.0)
    | Modify (f, v) ->
      (** FIXME: recomputing modify every time is inefficient *)
      Mat.init_rows n n (fun r c -> if modify f v r = c then 1.0 else 0.0)
    | Seq (p, q) -> gemm (of_pol p) (of_pol q)
    | Ite (a, p, q) ->
      let (a, p, q) = (of_pol a, of_pol p, of_pol q) in
      (* FIXME: incorrect  *)
      Mat.add (gemm a p) (gemm a q)
    | While (a,p) -> loop (of_pol a) (of_pol p)

  and loop a p =
    (* reorder indices such that exactly the first k packets satisfy a *)
    (* let (k, swaps) = normal_form a p in *)
    (* let fundamental = Mat.sub (Mat.identity k) p *)
    (* let absorption_matrix = gemm ~m:n ~n  *)
    p *)

end
