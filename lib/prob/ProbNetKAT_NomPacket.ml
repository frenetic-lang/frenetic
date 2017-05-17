(** nominal packet encoding  *)
open Core.Std
open ProbNetKAT

type t = value Field.Map.t
type pk = t

type 'domain_witness hyperpoint = private int list
type 'domain_witness codepoint = private int

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

    let to_codepoint _ = failwith "todo"
    let of_codepoint _ = failwith "todo"
    let to_pk _ = failwith "todo"
    let of_pk _ = failwith "todo"
  end

  module Codepoint = struct
    type t = domain_witness codepoint
    let to_hyperpoint = Hyperpoint.of_codepoint
    let of_hyperpoint = Hyperpoint.to_codepoint
    let to_pk = Fn.compose Hyperpoint.to_pk to_hyperpoint
    let of_pk = Fn.compose of_hyperpoint Hyperpoint.of_pk
  end

end


(* module NomPk = struct
  module type S = sig
    type t
    val projection : projection
    val bot : t

    val of_index : index -> t
    val to_index : t -> index

    val to_pk : t -> Pk.t
    val of_pk : Pk.t -> t

    val modify : field -> value -> t -> t
    val test : field -> value -> t -> bool
  end

  module Make (I : sig val domain : domain end) = struct
    type t = int
    
    let fields = Array.Permissioned.of_list (Map.keys I.domain)
    let values = Array.init (Array.length fields) ~f:(fun i -> 
      Map.find_exn I.domain fields.(i)
      |> Set.to_array)
    let bot = 0

    let of_index x = x
    let to_index x = x

    let inject (pk : Pk.t) =
      List.fold_left fields ~init:0 ~f:(fun code (f, n, vinj, _vej) ->
        vinj (Pk.get f) + code * n)

    let eject t =
      List.fold_right fields ~init:(t, Pk.dontcare) ~f:(fun (t, pk) (f, n, _vinj, vej) ->
        let (v, t) = Value.(t % n, t /% n) in

    )


    let eject =
      Field.Map.map projection.projf ~f:(fun { projf; ...} ->
        Value.Map.to_alist projf
        |> List.map ~f:(fun (v, idx) -> (idx, v))
        |> Index.Map.of_alist_exn)

    let to_pk t =
      List.fold fields ~init:(t, Pk.dontcare) ~f:(fun (code, pk) f ->
        let size = (Map.find_exn projection.projf f).
        let v = code % 
        )
      Field.Map.mapi eject ~f:(fun ~key:f ~data:ejectv ->

        )

    let of_pk pk = failwith "not implemented"

    let modify f v t =
      to_pk t |> Pk.modify f v |> of_pk

    let test f v t =
      to_pk t |> Pk.test f v |> of_pk
  end

end *)
