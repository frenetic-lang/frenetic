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

  let mass = T.fold ~init:Prob.zero ~f:(fun ~key:_ ~data:p acc -> Prob.(p + acc))

  let to_string t =
    if T.is_empty t then "⊥" else
    T.to_alist t
    |> List.map ~f:(fun (x, p) ->
      sprintf "%s\t@ %s" (Dom.to_string x) (Prob.to_string p)
    )
    |> String.concat ~sep:";\n  "
    |> sprintf "{\n  %s;\n}"

  let empty : t = T.empty
  let is_empty = T.is_empty
  let fold = T.fold
  let filter_map = T.filter_map
  let filter_keys = T.filter_keys
  let to_alist = T.to_alist ~key_order:`Increasing

  (* point mass distribution *)
  let dirac (p : Dom.t) : t = T.singleton p Prob.one

  let is_dirac (t : t) : Dom.t option =
    if T.length t = 1 then
      match T.to_alist t with
      | [(d, p)] when Prob.(equal p one) -> Some d
      | [_] -> None
      | _ -> assert false
    else
      None

  let uniform xs =
    let n = List.length xs in
    let p = Prob.(1 // n) in
    List.map xs ~f:(fun x -> (x, p))
    |> T.of_alist_exn

  (* pointwise sum of distributions *)
  let sum t1 t2 : t =
    Map.merge t1 t2 ~f:(fun ~key vals ->
      Some (match vals with
      | `Both (v1, v2) -> Prob.(v1 + v2)
      | `Left v | `Right v -> v))

  let convex_sum t1 p t2 : t =
    Map.merge t1 t2 ~f:(fun ~key vals ->
      Some (match vals with
      | `Both (v1, v2) -> Prob.(p*v1 + (one-p)*v2)
      | `Left v1 -> Prob.(p*v1)
      | `Right v2 -> Prob.((one-p)*v2)
    ))

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

  let unsafe_add t p x : t =
    T.update t x (function
      | None -> p
      | Some p' -> Prob.(p + p'))

  let weighted_sum (list : (t * Prob.t) list) : t =
    List.map list ~f:(fun (t, p) -> scale t ~scalar:p)
    |> List.fold ~init:empty ~f:sum

  let unsafe_normalize t =
    let open Prob in
    let mass = T.fold t ~init:zero ~f:(fun ~key ~data:p acc -> p + acc) in
    T.map t ~f:(fun p -> p / mass)

  let prob t ~(f : Dom.t -> bool) : Prob.t =
    T.fold t ~init:Prob.zero ~f:(fun ~key:x ~data:p acc ->
      if f x then Prob.(p + acc) else acc
    )

  let prob_of t x =
    match T.find t x with
    | None -> Prob.zero
    | Some p -> p

  let observe t ~(bot : Dom.t) ~(f: Dom.t -> bool) : t =
    let t = T.filter_keys t ~f in
    if T.is_empty t then
      dirac bot
    else
      unsafe_normalize t


  let expectation t ~(f : Dom.t -> Q.t) : Q.t =
    T.fold t ~init:Q.zero ~f:(fun ~key:x ~data:p acc ->
      Q.(acc + Prob.to_q p * f(x))
    )

  (* variance of random variable [f] w.r.t distribution [t] *)
  let variance t ~(f : Dom.t -> Q.t) : Q.t =
    let mean = expectation t f in
    expectation t ~f:(fun point -> Q.((f point - mean) * (f point - mean)))

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
  include Monad

  (* UNSAFE *)
  let to_alist = T.to_alist ~key_order:`Increasing

  let unchecked_of_alist_exn l =
    List.filter l ~f:(fun (x,p) -> Prob.(p > zero))
    |> T.of_alist_exn

  let of_alist_exn l =
    unchecked_of_alist_exn l
    |> Util.tap ~f:(fun t -> assert Prob.(equal one (mass t )))

end
