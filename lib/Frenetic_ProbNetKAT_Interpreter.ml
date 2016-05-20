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
  module Set = Set.Make(T)
  module SetMap = Map.Make(Set)
end

module Hist = struct
  module T = struct
    type t = Pkt.t list [@@deriving sexp, compare, show]
  end
  include T

  module Set = Set.Make(T)
  module SetMap = Map.Make(Set)
  let to_string t =
    Set.elements t
    |> List.map ~f:(fun pkts ->
        List.map pkts ~f:Pkt.show
        |> String.concat ~sep:"; "
        |> Printf.sprintf "[%s]")
    |> String.concat ~sep:", "
    |> Printf.sprintf "{%s}"
end

module Pol = struct
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
    | Choice of (t * float) list
    | Star of t
    | Dup
    [@@deriving sexp, compare, show]
end
