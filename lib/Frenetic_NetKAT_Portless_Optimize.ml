open Core.Std
open Frenetic_NetKAT_Portless

let mk_and pr1 pr2 =
  match pr1, pr2 with
    | True, _ ->
      pr2
    | _, True ->
      pr1
    | False, _ ->
      False
    | _, False ->
      False
    | _ ->
      And(pr1, pr2)

let mk_or pr1 pr2 =
  match pr1, pr2 with
    | True, _ ->
      True
    | _, True ->
      True
    | False, _ ->
      pr2
    | _, False ->
      pr1
    | _ ->
      Or(pr1, pr2)

let mk_not pat =
  match pat with
    | False -> True
    | True -> False
    | _ -> Neg(pat)

let mk_filter pr =
  Filter (pr)

let mk_union pol1 pol2 =
  match pol1, pol2 with
    | Filter False, _ ->
      pol2
    | _, Filter False ->
      pol1
    | _ ->
      Union(pol1,pol2)

let mk_seq pol1 pol2 =
  match pol1, pol2 with
    | Filter True, _ ->
      pol2
    | _, Filter True ->
      pol1
    | Filter False, _ ->
      pol1
    | _, Filter False ->
      pol2
    | _ ->
      Seq(pol1,pol2)

let mk_star pol =
  match pol with
    | Filter True ->
      pol
    | Filter False ->
      Filter True
    | Star(pol1) -> pol
    | _ -> Star(pol)

let mk_big_and = List.fold_left ~f:mk_and ~init:True

let mk_big_or = List.fold_left ~f:mk_or ~init:False

let mk_big_union = List.fold_left ~f:mk_union ~init:drop

let mk_big_seq = List.fold_left ~f:mk_seq ~init:id

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
  | Mod _ -> pol
  | Filter a -> Filter (norm_pred a)
  | Star p -> Star (norm_policy p)
  | Union (p, q) ->
    let pol' = Union (norm_policy p, norm_policy q) in
    mk_big_union (list_of_union pol')
  | Seq (p, q) ->
    let pol' = Seq (norm_policy p, norm_policy q) in
    mk_big_seq (list_of_seq pol')

(*
   TODO(car): flatten_union in Optimize outputs the unioned policies in reverse order.  While this is semantically equivalent,
   it's probably not right.
 *)
let rec flatten_union_k (pol : policy)
  (acc : policy list)
  (k : policy list -> 'a) : 'a = match pol with
  | Union (p, q) -> flatten_union_k p acc (fun acc ->
    flatten_union_k q acc k)
  | _ -> k (pol :: acc)

let flatten_union (pol : policy) : policy list = flatten_union_k pol [] ident
