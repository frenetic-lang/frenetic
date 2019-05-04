open Core

module FDD = Fdd.FDD

module Field = Packet.Field

type t = Set.M(Int64).t Map.M(Field).t

let empty : t = Map.empty (module Field) 

let merge : t -> t -> t =
  Map.merge_skewed ~combine:(fun ~key -> Set.union)

let rec of_fdd (fdd : FDD.t) : t =
  for_fdd empty fdd

and for_fdd dom fdd =
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


module Auto = Global_compiler.Automaton

let of_automaton (auto : Auto.t) : t =
  Auto.fold_reachable auto ~init:empty ~f:(fun dom _ (e,d) ->
    for_fdd (for_fdd dom e) d
  )


let representative_pks (t : t) : Packet.t list =
  Map.fold t ~init:[Packet.empty] ~f:(fun ~key:field ~data:vs pks ->
    Set.to_list vs
    (* add fresh value, representing the case that all tests fails *)
    |> List.cons Int64.(Set.min_elt_exn vs - 1L)
    |> List.concat_map ~f:(fun v ->
      List.map pks ~f:(Map.add_exn ~key:field ~data:v)
    )
  )
