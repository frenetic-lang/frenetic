open Core
open Probnetkat
open Util

module MakeOwl(Repr : Packet_Repr.S) = struct
  include Repr
  open Owl
  module Dense = Dense.Matrix.D
  module Sparse = Sparse.Matrix.D

  let show ?(label=true) mc = ()
(*     Sparse.to_dense (mc) |>
    Format.printf "@[MATRIX:@\n%a@\n@]@.%!"
      (Dense.print
          ~pp_left:(if label then Some (fun fmt -> fprintf fmt "%a|" Repr.Index.pp') else None)
          ~pp_head:None
          ~pp_foot:None
          ~pp_right:None ()) *)


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

  let of_pol ?(debug=false) ?(verbose=false) p =
    let rec of_pol p : Sparse.mat =
      let (t, mc) = time of_pol' p in
      if debug then begin
        fprintf (Format.std_formatter) "%a\n%!" pp_policy p;
        if verbose then show mc;
        print_time t;
        let nnz = Sparse.nnz mc and nnzc = Array.length (Sparse.nnz_cols mc) in
        printf "sparsety = (%.2fn, %.2f*nlog(n), %.2f*n^2)\n\n%!"
          Float.(of_int nnz / of_int n)
          Float.(of_int nnz / (of_int n |> fun n -> n * log n))
          Float.(of_int nnz / of_int Int.(n * n));
      end;
      mc

    and of_pol' p : Sparse.mat =
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
        let a, p = of_pol a, of_pol p in

        (* rearrange indices so that packets 0 to nq-1 satisfy a (these are the
           transient states of the while loop); and packets nq to n-1 do not
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
        let nq = ref 0 in (* number of transient states *)
        while !left < !right do
          (* increment left until it corresponds to a packet not satisfying a *)
          while !left < !right && Sparse.get a (!left) (!left) = 1.0 do
            incr left;
            incr nq;
          done;
          (* decrement right until it corresponds to a packet satisfying a *)
          while !left < !right && Sparse.get a (!right) (!right) = 0.0 do
            decr right
          done;
          if !left < !right then begin
            swap.(!left) <- !right;
            swap.(!right) <- !left;
            incr left;
            decr right;
            incr nq;
          end
        done;

        (* number of transient states *)
        let nq = !nq in
        (* number of absorbing states *)
        let nr = n - nq in
        if debug then begin
            printf "n = %d, nq = %d, nr = %d\n\n" n nq nr;
            printf "swap vector:\n";
            Array.iter swap ~f:(printf "%d "); printf "\n\n%!";
        end;

        (* There may not be any *proper* absorbing states, i.e. the only absorbing
           state is the empty set. In this case, the while loop is equvialent to
           drop.
         *)
        if nr = 0 then of_pol Probnetkat.Syntax.Smart.drop else begin

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

        if debug then begin
          printf "full matrix p:\n";
          show p;
          printf "transient matrix Q:\n%!";
          show ~label:false (Sparse.of_dense q);
          printf "trans-to-absorbing matrix R:\n%!";
          show ~label:false (Sparse.of_dense r);
        end;

        (* calculate absorption probabilities *)
        let qstar = Dense.(inv (sub (eye nq) q)) in
        let absorption = Dense.(dot qstar r) in
        if debug then begin
          printf "infinite sum (I-Q)^-1:\n%!";
          show ~label:false (Sparse.of_dense qstar);
          printf "absorption matrix (I-Q)^-1 * R:\n%!";
          show ~label:false (Sparse.of_dense absorption);
        end;

        let pstar = Sparse.reset p; p in

        (* transient states... *)
        for i = 0 to nq-1 do
          (* ... transitient to absorbing states... *)
          for j = nq to n-1 do
            (* ... with a certain absorption probabilitiy *)
            Dense.get absorption i (j-nq)
            |> Sparse.set pstar swap.(i) swap.(j)
          done
        done;

        (* absorbing states transitien to themselves with probability 1 *)
        for i = nq to n-1 do
          Sparse.set pstar swap.(i) swap.(i) 1.0
        done;

        pstar
        end
      in
      of_pol p

end

(**
TODO:
  * use more efficient m * diag(v)
  * more efficent choice (don't allocate zero-matrix)
  * less wastefulallocation
*)

module MakeLacaml(Repr : Packet_Repr.S) = struct
  include Repr
  open Lacaml.D
  (** ATTENTION: Fortran uses column-major layout!!! Therefore, use left-stochastic
      matrices *)

  type v_or_m =
    | V of vec
    | M of mat

  (* empty is not index of matrix *)
  let (n, empty) = (Index.max.i, Index.max.i + 1)

  let show ?(label=true) (mc : v_or_m) : unit =
    begin match mc with
      | M m -> m
      | V v -> Mat.of_diag v
    end
    |> Format.printf "@[<2>MATRIX:@\n%a@\n@]@.%!" (Lacaml.Io.pp_lfmat
        ?row_labels:
          (if label then
              Some (Array.init n (fun i -> Format.asprintf "%a%!" Repr.Index0.pp' i))
           else
              None)
        ~ellipsis:"*"
        ~print_right:false
        ~print_left:label
        ~print_foot:false ())

  let dirac ?(n=n) (f : int -> int) : v_or_m =
    M (Mat.of_col_vecs (Array.init n ~f:(fun i0 ->
      let v = Vec.make0 n in
      v.{f (i0+1)} <- 1.0;
      v)))

  let one = Vec.make n 1.0
  let zero = Vec.make0 n

  let of_pol ?(debug=false) ?(verbose=false) p : v_or_m =
    let rec of_pol p =
      let (t, mc) = time of_pol' p in
      if debug then begin
        fprintf (Format.std_formatter) "%a\n%!" pp_policy p;
        if verbose then show mc;
        print_time t;
      end;
      mc

    and of_pol' p =
      match p.p with
      | Skip ->
        V one
      | Drop ->
        V zero
      | Test (f,v) ->
        V (Vec.init n (fun i -> if Index.test' f v i then 1.0 else 0.0))
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
      | Modify (f,v) ->
        dirac (fun i -> Index.modify' f v i)
      | Seq (p,q) ->
        (** we're using *left*-stochastic matrices, so we need to swap before multiplying *)
        begin match of_pol p, of_pol q with
        | V p, V q -> V (Vec.mul q p)
        | M p, M q -> M (gemm q p)
        | (M p as m), V q -> Mat.scal_rows q p; m
        | V p, (M q as m) -> Mat.scal_cols q p; m
        end
      | Choice ps ->
        let y = Mat.make0 n n in
        List.iter ps ~f:(fun (p,r) -> match of_pol p with
          | V x ->
            for i = 1 to n do
              y.{i,i} <- y.{i,i} +. (Prob.to_float r) *. x.{i}
            done
          | M x -> Mat.axpy ~alpha:(Prob.to_float r) x y);
        M y
      | Ite (a,p,q) ->
        begin match of_pol a, of_pol p, of_pol q with
        | V a, M p, (M q as m) ->
          Mat.scal_rows a p;
          Mat.scal_rows (Vec.sub one a) q;
          Mat.axpy p q;
          m
        | V a, V p, (V q as v) ->
          axpy (Vec.mul a p) Vec.(mul (sub one a) q);
          v
        | V a, (M p as m), V q ->
          Mat.scal_rows a p;
          for i = 1 to n do
            if a.{i} = 0.0 then
              p.{i,i} <- q.{i}
          done;
          m
        | V a, V p, (M q as m) ->
          Mat.scal_rows (Vec.sub one a) q;
          for i = 1 to n do
            if a.{i} = 1.0 then
              q.{i,i} <- p.{i}
          done;
          m
        | _ -> assert false
        end
      | While(a,p) ->
        let V a, (M p as m) = of_pol a, of_pol p in
        (* rearrange indices so that packets 1 to nq satisfy a (these are the
           transient states of the while loop); and packets nq+1 to n do not
           satisfy a (these are the absorbing states).

           swap.(i) = new index of ith packet
        *)
        let swap = Lacaml.Common.create_int32_vec n in
        for i = 1 to n do
          swap.{i} <- Int32.of_int_exn i
        done;
        (* Scan packets from left and right ends of [1, ..., n]. If we find packets
           left < right such that left does not satisfy a, but right does, we
           swap them. Thus, when the loop terminates packets 1 to nq will satisfy
           a, and packets nq+1 to n will not satisfy a.
        *)
        let left = ref 1 and right = ref n in
        let nq = ref 0 in (* number of transient states *)
        while !left < !right do
          (* increment left until it corresponds to a packet not satisfying a *)
          while !left < !right && a.{!left} = 1.0 do
            incr left;
            incr nq;
          done;
          (* decrement right until it corresponds to a packet satisfying a *)
          while !left < !right && a.{!right} = 0.0 do
            decr right
          done;
          if !left < !right then begin
            swap.{!left} <- Int32.of_int_exn (!right);
            swap.{!right} <- Int32.of_int_exn (!left);
            incr left;
            decr right;
            incr nq;
          end
        done;
        (* number of transient states *)
        let nq = !nq in
        (* number of absorbing states *)
        let nr = n - nq in

        if debug then begin
          printf "n = %d, nq = %d, nr = %d\n\n" n nq nr;
          printf "swap vector:\n";
          fprintf (Format.std_formatter) "%a\n\n%!" Lacaml.Io.pp_rivec swap;
        end;

        (* There may not be any *proper* absorbing states, i.e. the only absorbing
           state is the empty set. In this case, the while loop is equvialent to
           drop.
         *)
        if nr = 0 then of_pol Probnetkat.Syntax.Smart.drop else begin

        (* forward and backward swapping is actually the same for our permutation;
           in particular, swap o swap = id *)
        if debug then begin
          printf "full matrix P:\n%!";
          show ~label:false (M p);
        end;

        let () = laswp ~n ~k1:1 ~k2:nq p swap in
        if debug then begin
          printf "P (rows swapped):\n%!";
          show ~label:false (M p);
        end;

        let () = lapmt p swap in

        if debug then begin
          printf "P (rows & cols swapped):\n%!";
          show ~label:false (M p);
          printf "transient matrix Q:\n%!";
          show ~label:false (M (lacpy ~m:nq ~n:nq ~ar:1      ~ac:1 p));
          printf "trans-to-absorbing matrix R:\n%!";
          show ~label:false (M (lacpy ~m:nr ~n:nq ~ar:(nq+1) ~ac:1 p));
        end;

        let absorption = Mat.make0 n n in
        for i = nq+1 to n do
          absorption.{i,i} <- 1.0
        done;

        (* calculate pstar *)
        let () = Mat.scal ~m:nq ~n:nq ~ar:1 ~ac:1 (-1.) p in
        for i = 1 to nq do
          p.{i,i} <- p.{i,i} +. 1.0;
        done;
        getri ~n:nq ~ar:1 ~ac:1 p;
        if debug then begin
          printf "infinite sum (I-Q)^-1:\n%!";
          show ~label:false (M (lacpy ~m:nq ~n:nq ~ar:1      ~ac:1 p));
        end;
        let _ = gemm ~m:nr ~n:nq ~k:nq ~c:absorption ~cr:(nq+1) ~cc:1 p ~ar:(nq+1) ~ac:1 p ~br:1 ~bc:1 in

        (* calculate absorption probabilities *)
        if debug then begin
          printf "absorption matrix (I-Q)^-1 * R:\n%!";
          show ~label:false (M absorption);
        end;

        (* unswap *)
        laswp ~k1:1 ~k2:nq absorption swap;
        lapmt absorption swap;

        M absorption
        end
        [@@warning "-8"] (* accept inexhaustive pattern match *)

      in
      of_pol p

end

let of_pol p =
  let domain = Probnetkat.domain p in
  let module Domain = struct let domain = domain end in
  let module Repr = Packet_Repr.Make(Domain) in
  let module Mc = MakeOwl(Repr) in
  printf "n = %d\n%!" Mc.n;
  Mc.of_pol p
