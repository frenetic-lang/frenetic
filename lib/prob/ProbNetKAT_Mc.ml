open Core
open ProbNetKAT

module MakeOwl(Repr : ProbNetKAT_Packet_Repr.S) = struct
  include Repr
  open Owl
  module Dense = Dense.Matrix.D
  module Sparse = Sparse.Matrix.D


  (* empty is not index of matrix *)
  let (n, empty) = (Index0.max.i + 1, Index0.max.i + 1)

  let dirac ?(n=n) (f : int -> int) : Sparse.mat =
    let mat = Sparse.zeros n n in
    for row = 0 to n-1 do
      let col = f row in
      if col < n then begin
        Sparse.set mat row col 1.0
      end
    done;
    mat

  let rec of_pol p : Sparse.mat =
    match p.p with
    | Skip ->
      Sparse.eye n
    | Drop ->
      dirac (fun _ -> empty)
    | Test (f,n) ->
      dirac (fun row -> if Index0.test' f n row then row else empty)
    | Neg a ->
      Sparse.(sub (eye n) (of_pol a))
    | Or (a,b) ->
      let a,b = (of_pol a, of_pol b) in
      Sparse.iteri_nz (fun i j v -> Sparse.set b i j v) a;
      b
    | Modify (f,n) ->
      dirac Index0.(modify' f n)
    | Seq (p,q) ->
      Sparse.dot (of_pol p) (of_pol q)
    | Choice ps ->
      List.map ps ~f:(fun (p,w) -> Sparse.mul_scalar (of_pol p) (Prob.to_float w))
      |> List.fold ~init:(Sparse.zeros n n) ~f:Sparse.add
    | Ite (a,p,q) ->
      let (a,p,q) = (of_pol a, of_pol p, of_pol q) in
      let a = Sparse.(diag a |> to_dense) in
      let p' = Sparse.mapi_nz 
        (fun row _ v -> if Dense.get a row 0 <> 0.0 then v else 0.0)
        p
      in
      let q' = Sparse.mapi_nz
        (fun row _ v -> if Dense.get a row 0 <> 0.0 then 0.0 else v)
        q
      in
      Sparse.add p' q'
    | While(a,p) ->
      let (a,p) = (of_pol a |> Sparse.diag |> Sparse.to_dense, of_pol p) in

      (* rearrange indices so that packets 1 to nq satisfy a (these are the
         transient states of the while loop); and packets nq+1 to n do not
         satisfy a (these are the absorbing states).

         swap.(i) = new index of ith packet
      *)
      let swap = Array.init n (fun i -> i) in
      (* Scan packets from left and right ends of [0, ..., n-1]. If we find packets
         left < right such that left does not satisfy a, but right does, we
         swap them. Thus, when the loop terminates packets 0 to nq-1 will satisfy
         a, and packets nq to n-1 will not satisfy a.
      *)
      let left = ref 0 and right = ref (n-1) in
      while !left < !right do
        (* increment left until it corresponds to a packet not satisfying a *)
        while !left < !right && Dense.get a (!left) 0 = 1.0 do
          incr left
        done;
        (* decrement right until it corresponds to a packet satisfying a *)
        while !left < !right && Dense.get a (!right) 0 = 0.0 do
          decr right
        done;
        if !left < !right then begin
          swap.(!left) <- !right;
          swap.(!right) <- !left;
          incr left;
          decr right;
        end
      done;
      let nq = if Dense.get a (!left) 0 = 1.0 then !left + 1 else !left in
      let nr = n - nq in

      (* extract q and r from p *)
      let q = Dense.zeros nq nq and r = Dense.zeros nq nr in
      let () = Sparse.iteri_nz (fun i j v ->
        let i = swap.(i) and j = swap.(j) in
        if i < nq && j < nq then
          Dense.set q i j v
        else if i < nq && j >= nq then
          Dense.set r i (j-nq) v)
        p
      in

      (* calculate absorption probabilities *)
      let qstar = Linalg.inv Dense.(sub (eye nq) q) in
      let absorption = Dense.(dot qstar r) in
      let pstar = Sparse.reset p; p in
      for i = 0 to n-1 do
        let i = swap.(i) in
        if i >= nq then
          Sparse.set pstar i i 1.0
        else
          for j = 0 to n-1 do
            let j = swap.(j) in
            if j >= nq then
              Dense.get absorption i (j-nq)
              |> Sparse.set pstar i j
          done
      done;
      pstar

end

module MakeLacaml(Repr : ProbNetKAT_Packet_Repr.S) = struct
  include Repr
  open Lacaml.D
  (** ATTENTION: Fortran uses column-major layout!!! Therefore, use left-stochastic
      matrices *)

  type v_or_m =
    | V of vec
    | M of mat

  (* empty is not index of matrix *)
  let (n, empty) = (Index.max.i, Index.max.i + 1)

  let dirac ?(n=n) (f : int -> int) : v_or_m =
    M (Mat.of_col_vecs (Array.init n ~f:(fun i0 ->
      let v = Vec.make0 n in
      v.{f (i0+1)} <- 1.0;
      v)))

  let one = Vec.make n 1.0
  let zero = Vec.make0 n

  let rec of_pol p : v_or_m =
    match p.p with
    | Skip ->
      V one
    | Drop ->
      V zero
    | Test (f,n) ->
      V (Vec.init n (fun i -> if Index.test' f n i then 1.0 else 0.0))
    | Neg a ->
      begin match of_pol a with
      | V v -> V Vec.(sub one v)
      | _ -> assert false
      end
    | Or (a,b) ->
      begin match of_pol a, of_pol b with
      | V a, V b -> V Vec.(max2 a b)
      | _ -> assert false
      end
    | Modify (f,n) ->
      dirac (fun i -> Index.modify' f n i)
    | Seq (p,q) ->
      (* Sparse.dot (of_pol p) (of_pol q) *)
      failwith "todo"
    | Choice ps ->
(*       List.map ps ~f:(fun (p,w) -> Sparse.mul_scalar (of_pol p) (Prob.to_float w))
      |> List.fold ~init:(Sparse.zeros n n) ~f:Sparse.add *)
      failwith "todo"
    | Ite (a,p,q) ->
      (* let (a,p,q) = (of_pol a, of_pol p, of_pol q) in
      let a = Sparse.(diag a |> to_dense) in
      let p' = Sparse.mapi_nz 
        (fun row _ v -> if Dense.get a row 0 <> 0.0 then v else 0.0)
        p
      in
      let q' = Sparse.mapi_nz
        (fun row _ v -> if Dense.get a row 0 <> 0.0 then 0.0 else v)
        q
      in
      Sparse.add p' q' *)
      failwith "todo"
    | While(a,p) ->
      failwith "todo"

end

let of_pol p =
  let domain = ProbNetKAT.domain p in
  let module Domain = struct let domain = domain end in
  let module Repr = ProbNetKAT_Packet_Repr.Make(Domain) in
  let module Mc = MakeOwl(Repr) in
  printf "n = %d\n%!" Mc.n;
  Mc.of_pol p
