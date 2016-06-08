open Core.Std


(*==========================================================================*)
(* Packets & Histories                                                      *)
(*==========================================================================*)

type headerval =
  | Switch of int
  | Port of int
  | Id of int
  | Dst of int
  [@@ deriving sexp, compare, show]


module type PSEUDOHISTORY = sig
  include Map.Key
  val dup : t -> t
  val test : t -> hv:headerval -> bool
  val modify : t -> hv:headerval -> t
end


module Pkt = struct
  type t = { switch : int [@default 1];
             port : int [@default 1];
             id : int [@default 1];
             dst : int [@default 1];
           } [@@deriving sexp, compare, show]

  let dup pk = pk

  let test pk ~(hv:headerval) : bool =
    match hv with
    | Switch sw -> pk.switch = sw
    | Port pt -> pk.port = pt
    | Id id -> pk.id = id
    | Dst d -> pk.dst = d

  let modify pk ~(hv:headerval) : t =
    match hv with
    | Switch sw -> { pk with switch = sw }
    | Port pt -> { pk with port = pt }
    | Id id -> { pk with id = id }
    | Dst d -> { pk with dst = d }
end


module Hist = struct
  type t = Pkt.t * Pkt.t list [@@deriving sexp, compare, show]

  let dup (pk,h) = (pk, pk::h)

  let test (pk,h) ~hv = Pkt.test pk ~hv

  let modify (pk,h) ~hv = (Pkt.modify pk ~hv, h)
end



(*==========================================================================*)
(* Probability & ProbNetKAT Policies                                        *)
(*==========================================================================*)

module type PROB = sig
  type t [@@deriving sexp, compare, show]
  val one : t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
end


module Pol (Prob : PROB) = struct
  type t =
    | Id
    | Drop
    | Test of headerval
    | Mod of headerval
    | Union of t * t
    | Seq of t * t
    | Choice of (t * Prob.t) list
    | Star of t
    | Dup
    [@@deriving sexp, compare, show]
end



(*==========================================================================*)
(* Distributions & Kernels                                                  *)
(*==========================================================================*)

module Dist (Point : Map.Key) (Prob : PROB) = struct

  module T = Map.Make(Point) (* Point.t -> 'a *)

  type t = Prob.t T.t (* Point.t -> Prob.t *)

  let empty : t = T.empty

  (* point mass distribution *)
  let dirac (p : Point.t) : t = T.singleton p Prob.one

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

  (* Markov Kernel *)
  module Kernel = struct
    type kernel = Point.t -> t

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
  module Let_syntax = struct
    let bind a b = Kernel.lift b a
    let (>>=) a b = bind a b
    let return = dirac
  end

end



(*==========================================================================*)
(* ProbNetKAT Interpreter                                                   *)
(*==========================================================================*)

module Interp (Hist : PSEUDOHISTORY) (Prob : PROB) = struct

  module HSet = Set.Make(Hist)
  module Dist = Dist(HSet)(Prob)
  module Pol = Pol(Prob)

  let rec eval (p : Pol.t) : Dist.Kernel.t = fun inp ->
    let open Dist in
    let open Dist.Let_syntax in
    match p with
    | Id ->
      dirac inp
    | Drop ->
      dirac HSet.empty
    | Dup ->
      HSet.map inp ~f:Hist.dup
      |> dirac
    | Test hv ->
      HSet.filter inp ~f:(Hist.test ~hv)
      |> dirac
    | Mod hv ->
      HSet.map inp ~f:(Hist.modify ~hv)
      |> dirac
    | Union (q,r)->
      let%bind a1 = eval q inp in
      let%bind a2 = eval r inp in
      return (HSet.union a1 a2)
      (* SJS: union (eval q inp) (eval r inp) *)
    | Seq (q,r)->
      eval q inp >>= eval r
      (* SJS: Kernel.seq (eval q) (eval r) inp *)
    | Choice dist ->
      List.map dist ~f:(fun (pol, prob) -> (eval p inp, prob))
      |> Dist.weighted_sum
    | Star _ ->
      failwith "star"

end
