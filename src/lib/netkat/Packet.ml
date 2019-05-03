open Core

module Field = struct
  module T = struct
    include Fdd.Field
    let compare (x : t) (y : t) = Int.compare (Obj.magic x) (Obj.magic y)
  end
  include T
  include Comparator.Make(T)
end

module T0 = struct
  type t = int64 Base.Map.M(Field).t
    [@@deriving compare, sexp, hash]
end

module T = struct
  include T0
  include Comparator.Make(T0)
end

include T

let apply_action_seq (pk : t) (seq : Fdd.Value.t Fdd.Action.Seq.t) : t =
  Map.to_alist seq
  |> List.fold ~init:pk ~f:(fun pk -> function 
    | F f, Const v -> Map.set pk ~key:f ~data:v
    | F _, _ -> failwith "unexpected action"
    | K, _ -> pk
  )

let apply_action (pk : t) (act : Fdd.Action.t) : Set.M(T).t =
  Set.map (module T) act ~f:(apply_action_seq pk) 

let rec apply_fdd (pk : t) (fdd : Fdd.FDD.t) : Fdd.Action.t =
  let open Fdd in
  match FDD.unget fdd with
  | Leaf act -> act
  | Branch { test=(f, Const v); tru; fls; _ } ->
    begin match Map.find pk f with
    | None ->
      Field.to_string f
      |> Format.sprintf "packet underspecified: missing value for field %s"
      |> failwith
    | Some v' ->
      if Int64.equal v v' then apply_fdd pk tru else apply_fdd pk fls
    end
  | Branch _ ->
    failwith "only constant tests supported"

