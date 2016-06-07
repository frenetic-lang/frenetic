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
  module T = Map.Make(Set)

  type point = Set.t
  type t = Prob.t T.t (* Set Key.t -> Prob.t *)

  let empty : t = T.empty
  let dirac (p : point) = T.singleton p Prob.one

  let union t1 t2 : t =
    T.fold t1 ~init:T.empty ~f:(fun ~key:point1 ~data:p1 acc ->
      T.fold t2 ~init:acc ~f:(fun ~key:point2 ~data:p2 acc ->
        let point = Set.union point1 point2 in
        let p = Prob.(p1 * p2) in
        T.change acc point (function
          | None -> Some p
          | Some p' -> Some Prob.(p' + p))))


end
