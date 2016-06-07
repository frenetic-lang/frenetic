open Core.Std
open Num

module Pkt = struct
  module T = struct
    type t = { switch : int [@default 1];
               port : int [@default 1];
               id : int [@default 1];
               dst : int [@default 1];
             } [@@deriving sexp, compare, show, make]
  end
  include T
  module Set = Set.Make(T) (* Pkt Set *)
  module SetMap = Map.Make(Set) (* Pkt Set -> 'a *)
end

module Hist = struct
  module T = struct
    type t = Pkt.t list [@@deriving sexp, compare, show]
  end
  include T

  module Set = Set.Make(T) (* Hist Set *)
  module SetMap = Map.Make(Set) (* Hist Set -> 'a *)
  let to_string t =
    Set.elements t
    |> List.map ~f:(fun pkts ->
        List.map pkts ~f:Pkt.show
        |> String.concat ~sep:"; "
        |> Printf.sprintf "[%s]")
    |> String.concat ~sep:", "
    |> Printf.sprintf "{%s}"
end

module type PROB = sig
  type t [@@deriving sexp, compare, show]
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
end

module Pol (Prob : PROB) = struct
  type t =
    | Id
    | Drop
    | Test of [`Switch of int
              | `Id of int
              | `Port of int
              | `Dst of int]
    | Set of [`Switch of int
             | `Id of int
             | `Port of int
             | `Dst of int]
    | Union of t * t
    | Seq of t * t
    | Choice of (t * Prob.t) list
    | Star of t
    | Dup
    [@@deriving sexp, compare, show]
end

module SetDist (Key : Map.Key) (Prob : PROB) = struct

  module Set = Set.Make(Key) (* Set Key.t *)
  module T = Map.Make(Set) (* point -> 'a *)

  type point = Set.t
  type t = Prob.t T.t (* point -> Prob.t *)

  let empty : t = T.empty

  (* point mass distribution *)
  let dirac (p : point) = T.singleton p Prob.one

  (* nonstandard operation: given distributions [t1] and [t2],
     sample independently from both to get values point1 and point2 and take
     union of the results *)
  let union t1 t2 : t =
    T.fold t1 ~init:T.empty ~f:(fun ~key:point1 ~data:p1 acc ->
      T.fold t2 ~init:acc ~f:(fun ~key:point2 ~data:p2 acc ->
        let point = Set.union point1 point2 in
        let p = Prob.(p1 * p2) in
        T.change acc point (function
          | None -> Some p
          | Some p' -> Some Prob.(p' + p))))

  let sum t1 t2 : t =
    Map.merge t1 t2 ~f:(fun ~key vals ->
      Some (match vals with
      | `Both (v1, v2) -> Prob.(v1 + v2)
      | `Left v | `Right v -> v))

  let scale t ~(scalar : Prob.t) =
    T.map t ~f:(fun p -> Prob.(p * scalar))


  (* Markov Kernel *)
  module Kernel = struct
    type kernel = point -> t

    (* lift Markov Kernel to measure transformer *)
    let lift (k : kernel) (dist : t) : t =
      T.fold dist ~init:empty ~f:(fun ~key:point ~data:prob acc ->
        k point (* output distribution for that intermediate result *)
        |> scale ~scalar:prob
        |> sum acc)

    (* sequential composition of kernels *)
    let seq (k1 : kernel) (k2 : kernel) : kernel = fun input_point ->
      let intermediate_dist = k1 input_point in
      (lift k2) intermediate_dist

  end


end
