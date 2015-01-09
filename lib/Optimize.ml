open Core.Std
open NetKAT_Types

let mk_and pr1 pr2 =
  match pr1, pr2 with
    | NetKAT_Types.True, _ ->
      pr2
    | _, NetKAT_Types.True ->
      pr1
    | NetKAT_Types.False, _ ->
      NetKAT_Types.False
    | _, NetKAT_Types.False ->
      NetKAT_Types.False
    | _ ->
      NetKAT_Types.And(pr1, pr2)

let mk_or pr1 pr2 =
  match pr1, pr2 with
    | NetKAT_Types.True, _ ->
      NetKAT_Types.True
    | _, NetKAT_Types.True ->
      NetKAT_Types.True
    | NetKAT_Types.False, _ ->
      pr2
    | _, NetKAT_Types.False ->
      pr1
    | _ ->
      NetKAT_Types.Or(pr1, pr2)

let mk_not pat =
  match pat with
    | NetKAT_Types.False -> NetKAT_Types.True
    | NetKAT_Types.True -> NetKAT_Types.False
    | _ -> NetKAT_Types.Neg(pat)

let mk_filter pr =
  NetKAT_Types.Filter (pr)

let mk_union pol1 pol2 =
  match pol1, pol2 with
    | NetKAT_Types.Filter NetKAT_Types.False, _ ->
      pol2
    | _, NetKAT_Types.Filter NetKAT_Types.False ->
      pol1
    | _ ->
      NetKAT_Types.Union(pol1,pol2)

let mk_seq pol1 pol2 =
  match pol1, pol2 with
    | NetKAT_Types.Filter NetKAT_Types.True, _ ->
      pol2
    | _, NetKAT_Types.Filter NetKAT_Types.True ->
      pol1
    | NetKAT_Types.Filter NetKAT_Types.False, _ ->
      pol1
    | _, NetKAT_Types.Filter NetKAT_Types.False ->
      pol2
    | _ ->
      NetKAT_Types.Seq(pol1,pol2)

let mk_star pol =
  match pol with
    | NetKAT_Types.Filter NetKAT_Types.True ->
      pol
    | NetKAT_Types.Filter NetKAT_Types.False ->
      NetKAT_Types.Filter NetKAT_Types.True
    | NetKAT_Types.Star(pol1) -> pol
    | _ -> NetKAT_Types.Star(pol)

let specialize_pred sw pr =
  let rec loop pr k =
    match pr with
      | NetKAT_Types.True ->
        k pr
      | NetKAT_Types.False ->
        k pr
      | NetKAT_Types.Neg pr1 ->
        loop pr1 (fun pr -> k (mk_not pr))
      | NetKAT_Types.Test (NetKAT_Types.Switch v) ->
        if v = sw then
          k NetKAT_Types.True
        else
          k NetKAT_Types.False
      | NetKAT_Types.Test _ ->
        k pr
      | NetKAT_Types.And (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_and p1 p2)))
      | NetKAT_Types.Or (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_or p1 p2))) in
  loop pr (fun x -> x)

let specialize_policy sw pol =
  let rec loop pol k =
    match pol with
      | NetKAT_Types.Filter pr ->
        k (NetKAT_Types.Filter (specialize_pred sw pr))
      | NetKAT_Types.Mod hv ->
        k pol
      | NetKAT_Types.Union (pol1, pol2) ->
        loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_union p1 p2)))
      | NetKAT_Types.Seq (pol1, pol2) ->
        loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_seq p1 p2)))
      | NetKAT_Types.Star pol ->
        loop pol (fun p -> k (mk_star p))
      | NetKAT_Types.Link(sw,pt,sw',pt') ->
	failwith "Not a local policy" in
  loop pol (fun x -> x)

let mk_big_and = List.fold_left ~f:mk_and ~init:NetKAT_Types.True

let mk_big_or = List.fold_left ~f:mk_or ~init:NetKAT_Types.False

let mk_big_union = List.fold_left ~f:mk_union ~init:NetKAT_Types.drop

let mk_big_seq = List.fold_left ~f:mk_seq ~init:NetKAT_Types.id

(* list_of_and flattens a predicate into a list of predicates, each of which
   is not an And. E.g., list_of_and (And (p, And (q, r))) = [p; q; r], if
   p, q, and r are not Ands. The other list_of_* functions are similar. *)
let rec list_of_and (pred : pred) : pred list = match pred with
  | And (a, b) -> list_of_and a @ list_of_and b
  | _ -> [pred]

let rec list_of_or (pred : pred) : pred list = match pred with
  | Or (a, b) -> list_of_or a @ list_of_or b
  | _ -> [pred]

let rec list_of_seq (pol : policy) : policy list = match pol with
  | Seq (p, q) -> list_of_seq p @ list_of_seq q
  | _ -> [pol]

let rec list_of_union (pol : policy) : policy list = match pol with
  | Union (p, q) -> list_of_union p @ list_of_union q
  | _ -> [pol]

(* Normalizes predicate so that all nested Ands and Ors are nested on the
   right-hand side. norm_policy is similar. *)
let rec norm_pred (pred : pred) : pred = match pred with
  | True | False | Test _ -> pred
  | Neg a -> Neg (norm_pred a)
  | And (a, b) ->
    let pred' = And (norm_pred a, norm_pred b) in
    mk_big_and (list_of_and pred')
  | Or (a, b) ->
    let pred' = Or (norm_pred a, norm_pred b) in
    mk_big_or (list_of_or pred')

let rec norm_policy (pol : policy) : policy = match pol with
  | Mod _ | Link _ -> pol
  | Filter a -> Filter (norm_pred a)
  | Star p -> Star (norm_policy p)
  | Union (p, q) ->
    let pol' = Union (norm_policy p, norm_policy q) in
    mk_big_union (list_of_union pol')
  | Seq (p, q) ->
    let pol' = Seq (norm_policy p, norm_policy q) in
    mk_big_seq (list_of_seq pol')
