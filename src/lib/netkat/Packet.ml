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

let empty = Map.empty (module Field)

let apply_action_seq (seq : Fdd.Value.t Fdd.Action.Seq.t) (pk : t) : t =
  Map.to_alist seq
  |> List.fold ~init:pk ~f:(fun pk -> function 
    | F f, Const v -> Map.set pk ~key:f ~data:v
    | F _, _ -> failwith "unexpected action"
    | K, _ -> pk
  )

let apply_action (act : Fdd.Action.t) (pk : t) : Set.M(T).t =
  Set.map (module T) act ~f:(fun seq -> apply_action_seq seq pk) 

let rec apply_fdd (fdd : Fdd.FDD.t) (pk : t) : Fdd.Action.t =
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
      if Int64.equal v v' then apply_fdd tru pk else apply_fdd fls pk
    end
  | Branch _ ->
    failwith "only constant tests supported"

let eval_e_fdd (e : Fdd.FDD.t) (pk : t) : Set.M(T).t =
  let act = apply_fdd e pk in
  apply_action act pk

let eval_d_fdd (d : Fdd.FDD.t) (pk : t) : int64 Map.M(T).t =
  apply_fdd d pk
  |> Set.to_list
  |> List.map ~f:(fun seq ->
    match Map.find seq Fdd.Action.K with
    | Some (Const v) -> (apply_action_seq seq pk, v)
    | _ -> failwith "malformed D-FDD; continuation missing or invalid"
  )
  |> Map.of_alist_exn (module T)

