(*==========================================================================*)
(* Distributions & Kernels                                                  *)
(*==========================================================================*)

open Core

module Make (Domain : Map.Key) = struct

  module T = Map.Make(Domain) (* Domain.t -> 'a *)

  type t = Prob.t T.t [@@deriving sexp] (* Domain.t -> Prob.t *)

  let empty : t = T.empty
  let fold = T.fold
  let filter_map = T.filter_map
  let filter_keys = T.filter_keys

  (* point mass distribution *)
  let dirac (p : Domain.t) : t = T.singleton p Prob.one

  (* pointwise sum of distributions *)
  let sum t1 t2 : t =
    Map.merge t1 t2 ~f:(fun ~key vals ->
      Some (match vals with
      | `Both (v1, v2) -> Prob.(v1 + v2)
      | `Left v | `Right v -> v))

  let scale t ~(scalar : Prob.t) : t =
    T.map t ~f:(fun p -> Prob.(p * scalar))

  let weighted_sum (list : (t * Prob.t) list) : t =
    List.map list ~f:(fun (t, p) -> scale t ~scalar:p)
    |> List.fold ~init:empty ~f:sum

  (* expectation of random variable [f] w.r.t distribution [t] *)
  let expectation t ~(f : Domain.t -> Prob.t) : Prob.t =
    T.fold t ~init:Prob.zero ~f:(fun ~key:point ~data:prob acc ->
      Prob.(acc + prob * f point))

  (* variance of random variable [f] w.r.t distribution [t] *)
  let variance t ~(f : Domain.t -> Prob.t) : Prob.t =
    let mean = expectation t f in
    expectation t ~f:(fun point -> Prob.((f point - mean) * (f point - mean)))

  (* Markov Kernel *)
  module Kernel = struct
    type kernel = Domain.t -> t

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
