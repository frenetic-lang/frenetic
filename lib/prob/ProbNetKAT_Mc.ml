open Core
(* open Lacaml.D *)
open ProbNetKAT
open Owl

module Dense = Dense.Matrix.D
module Sparse = Sparse.Matrix.D

module Make(Repr : ProbNetKAT_Packet_Repr.S) = struct
  include Repr

  (* empty is not index of matrix *)
  let (n, empty) = (Index.max.i, Index.max.i + 1)

  let dirac ?(n=n) (f : int -> int) : Sparse.mat =
    let mat = Sparse.zeros n n in
    for row = 1 to n do
      let col = f row in
      if col <> empty then Sparse.set mat row col 1.0
    done;
    mat

  let rec of_pol p : Sparse.mat =
    match p with
    | Skip ->
      Sparse.eye n
    | Drop ->
      dirac (fun _ -> empty)
    | TestEq (f,n) ->
      dirac (fun row -> if Index.test' f n row then row else empty)
    | TestNeq (f,n) ->
      dirac (fun row -> if Index.test' f n row then empty else row)
    | Modify (f,n) ->
      dirac Index.(modify' f n)
    | Seq (p,q) ->
      Sparse.dot (of_pol p) (of_pol q)
    | Choice ps ->
      List.map ps ~f:(fun (p,w) -> Sparse.mul_scalar (of_pol p) (Prob.to_float w))
      |> List.fold ~init:(Sparse.zeros n n) ~f:Sparse.add
    | Ite (a,p,q) ->
      let (a,p,q) = (of_pol a, of_pol p, of_pol q) in
      let p' = Sparse.mapi_nz 
        (fun row _ v -> if Sparse.get a row row <> 0.0 then v else 0.0)
        p
      in
      let q' = Sparse.mapi_nz
        (fun row _ v -> if Sparse.get a row row <> 0.0 then 0.0 else v)
        q
      in
      Sparse.add p' q'
    | While(a,p) ->
      let (a,p) = (of_pol a, of_pol p |> Sparse.to_dense) in
      let transient = Sparse.nnz_rows a in
      let recurrent = transient in
      let tn = Array.length transient in
      (* let rn = n - tn in *)
      let qr = Dense.rows p transient in
      let (q,r) = (Dense.cols qr transient, Dense.cols qr recurrent) in
      let n = Linalg.inv Dense.(sub (eye tn) q) in
      Dense.(dot n r)
      |> Sparse.of_dense
      




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
