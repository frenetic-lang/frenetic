(** nominal packet encoding  *)
open Core.Std
open ProbNetKAT

type t = value Field.Map.t
type pk = t

type 'domain_witness hyperpoint = int list
type 'domain_witness codepoint = int

module type S = sig
  type domain_witness

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

  let domain : (Field.t * Value.t list) list =
    Map.to_alist (Map.map D.domain ~f:Value.Set.to_list)

  type domain_witness

  module Hyperpoint = struct
    type t = domain_witness hyperpoint

    let dimension =
      List.map domain ~f:(Fn.compose List.length snd)

    let injection : (Field.t * (Value.t -> int)) list =
      List.Assoc.map domain ~f:(fun vs ->
        List.mapi vs ~f:(fun i v -> (v, i+1))
        |> Value.Map.of_alist_exn
        |> (fun m v -> Option.value (Map.find m v) ~default:0))

    let ejection : (Field.t * (int -> Value.t)) list =
      List.Assoc.map domain ~f:(fun vs _ -> failwith "todo")


    let to_codepoint t =
      List.fold2_exn t dimension ~init:0 ~f:(fun cp v n -> v + n * cp)

    let of_codepoint cp =
      List.fold_right dimension ~init:(cp,[]) ~f:(fun n (cp, hp) ->
        let (cp, v) = Int.(cp /% n, cp % n) in
        (cp, v::hp))
      |> snd

    let to_pk t =
      List.fold2_exn t ejection ~init:Field.Map.empty ~f:(fun pk v (f, vej) ->
        Field.Map.add pk f (vej v))

    let of_pk pk =
      List.map injection ~f:(fun (f, vinj) -> vinj (Field.Map.find_exn pk f))
  end

  module Codepoint = struct
    type t = domain_witness codepoint
    let to_hyperpoint = Hyperpoint.of_codepoint
    let of_hyperpoint = Hyperpoint.to_codepoint
    let to_pk = Fn.compose Hyperpoint.to_pk to_hyperpoint
    let of_pk = Fn.compose of_hyperpoint Hyperpoint.of_pk
  end

end
