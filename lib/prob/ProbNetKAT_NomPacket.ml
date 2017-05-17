(** nominal packet encoding  *)
open Core.Std
open ProbNetKAT

type t = value Field.Map.t
type pk = t

type 'domain_witness hyperpoint = int list
type 'domain_witness codepoint = int

module type S = sig
  val domain : domain
  type domain_witness
  val fields : Field.t list

  module rec Hyperpoint : sig
    type t = domain_witness hyperpoint
    val dimension : int list
    val to_codepoint : t -> Codepoint.t
    val of_codepoint : Codepoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
  end

  and Codepoint : sig
    type t = domain_witness codepoint
    val to_hyperpoint : t -> Hyperpoint.t
    val of_hyperpoint : Hyperpoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
  end
end


module Make(D : sig 
  val domain : domain
end) : S = struct

  let domain = D.domain
  type domain_witness
  let fields = Map.keys domain

  module Hyperpoint = struct
    type t = domain_witness hyperpoint

    let dimension =
      List.map fields ~f:(fun f -> Set.length (Map.find_exn domain f))

    let values = []

    let inv = []

    let to_codepoint t =
      List.fold2_exn t dimension ~init:0 ~f:(fun cp v n -> v + n * cp)

    let of_codepoint cp =
      List.fold_right dimension ~init:(cp,[]) ~f:(fun n (cp, hp) ->
        let (cp, v) = Int.(cp /% n, cp % n) in
        (cp, v::hp))
      |> snd

    let to_pk t =
      List.fold2_exn t inv ~init:Field.Map.empty ~f:(fun pk vidx (f, vinv) ->
        Field.Map.add pk f vinv.(vidx))

    let of_pk pk =
      List.map2_exn fields values ~f:(fun f vinj -> vinj (Field.Map.find_exn pk f))
  end

  module Codepoint = struct
    type t = domain_witness codepoint
    let to_hyperpoint = Hyperpoint.of_codepoint
    let of_hyperpoint = Hyperpoint.to_codepoint
    let to_pk = Fn.compose Hyperpoint.to_pk to_hyperpoint
    let of_pk = Fn.compose of_hyperpoint Hyperpoint.of_pk
  end

end
