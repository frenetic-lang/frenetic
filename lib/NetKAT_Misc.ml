open NetKAT_Types

let pred_true = True

let pred_false = False

let switches_of_policy (p:policy) =
  let open Core.Core_list in
  let rec collect p =
    match p with
    | Filter _ | Mod _ ->
       []
    | Union(q,r) | Seq (q,r) ->
       collect q @ collect r
    | Star q ->
       collect q
    | Link(sw1,_,sw2,_) ->
       [sw1;sw2] in
  to_list (dedup (collect p))
