(** nominal packet encoding  *)
open Core.Std
open ProbNetKAT

type t = value Field.Map.t
type pk = t

type 'domain_witness hyperpoint = int list
type 'domain_witness codepoint = int

module type S = sig
  type domain_witness

  (** Encoding of packet in n dimensional space.
      More precisely, a packet is encoded as a point in a hypercube, with the
      coordinates being of type int.
      If [dimension] = {k1, ..., kn}, then the hypercube is given by
        {0, ..., k1} x ... x {0, ..., kn}.
      The points within this cube are represented as lists, rather than tuples,
      because n is not known at compile time.
  *)
  module rec Hyperpoint : sig
    type t = domain_witness hyperpoint
    val dimension : int list
    val to_codepoint : t -> Codepoint.t
    val of_codepoint : Codepoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
  end

  (** Encoding of packets as integers, i.e. points in single dimensional space. *)
  and Codepoint : sig
    type t = domain_witness codepoint
    val max : t
    val to_hyperpoint : t -> Hyperpoint.t
    val of_hyperpoint : Hyperpoint.t -> t
    val to_pk : t -> pk
    val of_pk : pk -> t
    val to_idx : t -> int (** non-negative matrix index *)
  end
end


module Make(D : ProbNetKAT.Domain) : S = struct

  let domain : (Field.t * Value.t list) list =
    Map.to_alist (Map.map D.domain ~f:Value.Set.to_list)

  type domain_witness

  module Hyperpoint = struct
    type t = domain_witness hyperpoint

    let dimension =
      List.map domain ~f:(fun (_,vs) -> List.length vs + 1)

    let injection : (Field.t * (Value.t option -> int)) list =
      List.Assoc.map domain ~f:(fun vs ->
        List.mapi vs ~f:(fun i v -> (v, i+1))
        |> Value.Map.of_alist_exn
        |> (fun m -> function
            | None -> 0
            | Some v -> Option.value (Map.find m v) ~default:0))

    let ejection : (Field.t * (int -> Value.t option)) list =
      List.Assoc.map domain ~f:List.to_array
      |> List.Assoc.map ~f:(fun inj v -> if v = 0 then None else Some inj.(v-1))


    let to_codepoint t =
      List.fold2_exn t dimension ~init:0 ~f:(fun cp v n -> v + n * cp)

    let of_codepoint cp =
      List.fold_right dimension ~init:(cp,[]) ~f:(fun n (cp, hp) ->
        let (cp, v) = Int.(cp /% n, cp % n) in
        (cp, v::hp))
      |> snd

    let to_pk t =
      List.fold2_exn t ejection ~init:Field.Map.empty ~f:(fun pk v (f, vej) ->
        Option.value_map (vej v)
          ~f:(fun data -> Field.Map.add pk ~key:f ~data)
          ~default:pk)

    let of_pk pk =
      List.map injection ~f:(fun (f, vinj) -> vinj (Field.Map.find pk f))
  end

  module Codepoint = struct
    type t = domain_witness codepoint
    let to_hyperpoint = Hyperpoint.of_codepoint
    let of_hyperpoint = Hyperpoint.to_codepoint
    let to_pk = Fn.compose Hyperpoint.to_pk to_hyperpoint
    let of_pk = Fn.compose of_hyperpoint Hyperpoint.of_pk
    let max = (List.fold ~init:1 ~f:( * ) Hyperpoint.dimension) - 1
    let to_idx cp = cp + 1
  end

end
