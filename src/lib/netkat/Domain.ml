(** The domain of an FDD/Automaton *)

open Core

module FDD = Fdd.FDD

module Field = struct
  module T = struct
    include Fdd.Field
    let compare (x : t) (y : t) = Int.compare (Obj.magic x) (Obj.magic y)
  end
  include T
  include Comparator.Make(T)
end

type t = Set.M(Int64).t Map.M(Field).t

let merge : t -> t -> t =
  Map.merge_skewed ~combine:(fun ~key -> Set.union)

let of_fdd (fdd : FDD.t) : t =
  let rec for_fdd dom fdd =
    match FDD.unget fdd with
    | Leaf r ->
      for_leaf dom r
    | Branch {test=(field,_)} ->
      let vs, residuals =
        for_field field fdd (Set.empty (module Int64)) (Set.empty (module FDD))
      in
      let dom = Map.update dom field ~f:(function
        | None -> vs
        | Some vs' -> Set.union vs vs')
      in
      Set.fold residuals ~init:dom ~f:for_fdd

  (** returns list of values appearing in tests with field [f] in [fdd], and
      residual trees below f-tests. *)
  and for_field f fdd vs residuals =
    match FDD.unget fdd with
    | Branch {test=(f',v); tru; fls} when f' = f ->
      let vs = match v with Const v -> Set.add vs v | _ -> vs in
      for_field f fls vs (Set.add residuals tru)
    | Branch _ | Leaf _ ->
      (vs, Set.add residuals fdd)

  and for_leaf dom act =
    Set.fold act ~init:dom ~f:for_seq

  and for_seq dom seq =
    Map.to_alist seq
    |> List.fold ~init:dom ~f:(fun dom -> function
      | (F f, Const v) ->
        Map.update dom f ~f:(function
          | None -> Set.singleton (module Int64) v
          | Some vs -> Set.add vs v
        )
      | _ ->
        dom
    )


  in
  for_fdd (Map.empty (module Field)) fdd



(*
let pre_variants (dom : t) : PrePacket.t list =
  Map.fold dom ~init:[PrePacket.empty] ~f:(fun ~key:f ~data:vs pks ->
    Valset.to_list vs
    |> List.concat_map ~f:(fun v -> List.map pks ~f:(fun pk ->
      PrePacket.modify pk f v))
  )

let variants (dom : t) : Packet.t list =
  pre_variants dom
  |> List.map ~f:(fun pk -> Packet.Pk pk)
  |> List.cons (Packet.Emptyset)
 *)
