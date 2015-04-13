open Core.Std
open NetKAT_Types

let pred_true = True

let pred_false = False

let queries_of_policy (pol : policy) : string list =
  let rec loop (pol : policy) (acc : string list) : string list = match pol with
    | Mod (Location (Query str)) ->
      if List.mem acc str then acc else str :: acc
    | Filter _ | Mod _ | Link _ -> acc
    | Union (p, q) | Seq (p, q) -> loop q (loop p acc)
    | Star p -> loop p acc in
  loop pol []

let switches_of_policy (p:policy) =
  let rec collect' a =
    match a with
    | Test (Switch sw) ->
       [sw]
    | True | False | Test _ ->
       []
    | And (b, c) | Or (b, c) ->
       collect' b @ collect' c
    | Neg b -> collect' b in
  let rec collect p =
    match p with
    | Filter a ->
       collect' a
    | Mod _ ->
       []
    | Union(q,r) | Seq (q,r) ->
       collect q @ collect r
    | Star q ->
       collect q
    | Link(sw1,_,sw2,_) ->
       [sw1;sw2] in
  List.to_list (List.dedup (collect p))
