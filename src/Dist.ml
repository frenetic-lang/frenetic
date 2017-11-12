(*==========================================================================*)
(* Distributions & Kernels                                                  *)
(*==========================================================================*)

open Core

module Make (Dom : Vlr.HashCmp) = struct

  module Dom = Dom

  module T = struct
    include Map.Make(Dom) (* Dom.t -> 'a *)

    let hash_fold_t = Map.hash_fold_direct Dom.hash_fold_t
  end

  type t = Prob.t T.t [@@deriving sexp, compare, eq, hash] (* Dom.t -> Prob.t *)

  let support = T.keys

  let to_string t =
    if T.is_empty t then "⊥" else
    T.sexp_of_t Prob.sexp_of_t t
    |> Sexp.to_string

  let empty : t = T.empty
  let is_empty = T.is_empty
  let fold = T.fold
  let filter_map = T.filter_map
  let filter_keys = T.filter_keys
  let to_alist = T.to_alist ~key_order:`Increasing

  (* point mass distribution *)
  let dirac ?(weight=Prob.one) (p : Dom.t) : t = T.singleton p weight

  (* pointwise sum of distributions *)
  let sum t1 t2 : t =
    Map.merge t1 t2 ~f:(fun ~key vals ->
      Some (match vals with
      | `Both (v1, v2) -> Prob.(v1 + v2)
      | `Left v | `Right v -> v))

  let pushforward t ~(f: Dom.t -> Dom.t) : t =
    T.fold t ~init:empty ~f:(fun ~key:x ~data:p dist ->
      T.update dist (f x) ~f:(function
        | None -> p
        | Some p' -> Prob.(p' + p)
      )
    )

  let prod_with t1 t2 ~(f: Dom.t -> Dom.t -> Dom.t) =
    T.fold t1 ~init:empty ~f:(fun ~key:x ~data:p1 init ->
      T.fold t2 ~init ~f:(fun ~key:y ~data:p2 dist ->
        let p = Prob.(p1 * p2) in
        T.update dist (f x y) ~f:(function
          | None -> p
          | Some p' -> Prob.(p' + p)
        )
      )
    )

  let scale t ~(scalar : Prob.t) : t =
    T.map t ~f:(fun p -> Prob.(p * scalar))

  let add t p x : t =
    T.update t x (function
      | None -> p
      | Some p' -> Prob.(p + p'))

  let weighted_sum (list : (t * Prob.t) list) : t =
    List.map list ~f:(fun (t, p) -> scale t ~scalar:p)
    |> List.fold ~init:empty ~f:sum

  (* expectation of random variable [f] w.r.t distribution [t] *)
  let expectation t ~(f : Dom.t -> Prob.t) : Prob.t =
    T.fold t ~init:Prob.zero ~f:(fun ~key:point ~data:prob acc ->
      Prob.(acc + prob * f point))

  (* variance of random variable [f] w.r.t distribution [t] *)
  let variance t ~(f : Dom.t -> Prob.t) : Prob.t =
    let mean = expectation t f in
    expectation t ~f:(fun point -> Prob.((f point - mean) * (f point - mean)))

  (* Markov Kernel *)
  module Kernel = struct
    type kernel = Dom.t -> t

    (* lift Markov Kernel to measure transformer *)
    let lift (k : kernel) (dist : t) : t =
      T.fold dist ~init:empty ~f:(fun ~key:point ~data:prob acc ->
        k point (* output distribution for that intermediate result *)
        |> scale ~scalar:prob
        |> sum acc)

    (* sequential composition of kernels *)
    let seq (k1 : kernel) (k2 : kernel) : kernel = fun input_point ->
      k1 input_point
      |> lift k2

    type t = kernel

  end

  (* probability monad *)
  module Monad = struct
    module Let_syntax = struct
      let bind a b = Kernel.lift b a
      let (>>=) a b = bind a b
      let return = dirac
    end
    include Let_syntax
  end

end
