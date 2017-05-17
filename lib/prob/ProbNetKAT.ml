open Core.Std

type field = int [@@deriving sexp, show, compare, eq]
module Field = Int

type value = int [@@deriving sexp, show, compare, eq]
module Value = Int

type policy =
  | Skip
  | Drop
  | TestEq of field * value
  | TestNeq of field * value
  | Modify of field * value
  | Seq of policy * policy
  | Ite of policy * policy * policy
  | While of policy * policy * policy
  [@@deriving sexp, show, compare, eq]


type domain = Value.Set.t Field.Map.t

(* compute domain of each field *)
let domain pol : domain =
  let rec domain pol d =
    match pol with
    | Skip 
    | Drop ->
      d
    | TestEq (f,n)
    | TestNeq (f,n)
    | Modify (f,n) ->
      Field.Map.update d f ~f:(function 
        | None -> Value.Set.singleton n 
        | Some ns -> Value.Set.add ns n)
    | Seq (p,q) ->
      d |> domain p |> domain q
    | Ite (p,q,r) | While (p,q,r) ->
      d |> domain p |> domain q |> domain r
  in
  domain pol Field.Map.empty

