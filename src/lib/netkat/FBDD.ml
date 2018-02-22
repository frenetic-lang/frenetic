open Core
open Fdd
open Syntax
open Frenetic_kernel

module Field = Fdd.Field
module Action = Fdd.Action
module Value = Fdd.Value
module Par = Action.Par
module Seq = Action.Seq
module FDD = Local_compiler.FDD

module Bool = struct
  include Bool
  let zero = false
  let one = true
  let prod = (&&)
  let sum = (||)
end

module BDD = struct
  include Vlr.Make(Field)(Value)(Bool)
  let negate t =
    map_r t ~f:(function true -> false | false -> true)
  let one = id
  let zero = drop
  let is_zero = equal zero
  let is_one = equal one
end

module Modification = struct
  module T = Seq
  type t = Value.t T.t [@@deriving sexp]
  let compare = Seq.compare_direct Value.compare
  let equal = Seq.equal Value.equal
  let one = Seq.empty
  let is_one = Seq.is_empty
  let prod = 
    Seq.merge ~f:(fun ~key m ->
      match m with | `Both(_, v) | `Left v | `Right v -> Some(v)
    )
  let sum m1 m2 : t option =
    if equal m1 m2 then Some m1 else None

  let to_hvs = Seq.to_hvs
end

type modification = Modification.t [@@deriving sexp, compare]


module Factor = struct
  type t = (BDD.t * modification) [@@deriving sexp, compare]

  let one = (BDD.id, Modification.one)

  let prod (b1,m1) (b2,m2) : t =
    (BDD.prod b1 b2, Modification.prod m1 m2)

  let sum (b1,m1) (b2,m2) : t option = Option.(
      Modification.sum m1 m2 >>= fun m ->
      return (BDD.sum b1 b2, m)
    )

  let of_test env hv : t =
    (BDD.atom (Pattern.of_hv ~env hv) true false, Modification.one)

  let of_mod env hv : t =
    let k, v = Pattern.of_hv ~env hv in
    (* ensure the field is mutable *)
    begin match hv with
      | Meta (id,_) ->
        let _,(_,mut) = Field.Env.lookup env id in
        if not mut then failwith "cannot modify immutable field"
      | _ -> ()
    end;
    (BDD.id, (Seq.singleton (F k) v))

  let rec of_pred env p : t option=
    match p with
    | True ->
      Some one
    | False ->
      None
    | Test(hv) ->
      Some (of_test env hv)
    | And(p, q) -> Option.(
        of_pred env p >>= fun p ->
        of_pred env q >>= fun q ->
        return (prod p q)
      )
    | Or (p, q) ->
      let sum' = sum in
      Option.(
        of_pred env p >>= fun p ->
        of_pred env q >>= fun q ->
        sum' p q
      )
    | Neg(q) ->
      begin match of_pred env q with
      | None -> Some one
      | Some (b,m) ->
        if BDD.(equal b id) then None else
        Some (BDD.negate b, m)
      end

  let to_fdd (bdd, m) : FDD.t =
    let action = Par.singleton m in
    BDD.fold bdd
      ~f:(function true -> FDD.const action | false -> FDD.drop)
      ~g:FDD.unchecked_cond

end
  
module T = Map.Make(Modification) 
let one = T.singleton Modification.one BDD.one
let zero = T.empty
let drop = zero
let id = one
let is_zero = T.is_empty
let equal = Map.equal BDD.equal

let is_singleton (t : 'v T.t) : (Modification.t * 'v) option =
  if T.length t <> 1 then None else
  match T.to_alist t with
  | [kv] -> Some kv
  | _ -> assert false

let is_one t =
  match is_singleton t with
  | None -> false
  | Some (m,b) -> BDD.is_one b && Modification.is_one m

type t = BDD.t T.t [@@deriving sexp, compare]

let restrict hvs (t : t) : t =
  T.filter_map t ~f:(fun bdd -> 
    let bdd = BDD.restrict hvs bdd in
    if BDD.is_zero bdd then None else Some bdd
  )

let to_par_exn t : Par.t =
  assert (T.for_all t BDD.is_one);
  Par.of_list (T.keys t)

let clear_cache ~(preserve:Int.Set.t) : unit =
  BDD.clear_cache ~preserve

let refs t =
  T.data t
  |> List.map ~f:BDD.refs
  |> Int.Set.union_list

let of_pred env pred =
  match Factor.of_pred env pred with
  | None -> zero
  | Some (b,m) -> T.singleton m b

let of_mod env hv =
  let (b,m) = Factor.of_mod env hv in
  T.singleton m b

let compose t u =
  T.fold u ~init:T.empty ~f:(fun ~key:m2 ~data:b2 t ->
    T.fold t ~init:t ~f:(fun ~key:m1 ~data:b1 t ->
      let b = BDD.(prod b1 @@ restrict Modification.(to_hvs m1) b2) in
      if BDD.is_zero b then t else
      let m = Modification.prod m1 m2 in
      T.update t m ~f:(function
        | None -> b
        | Some b' -> BDD.sum b b'
      )
    )
  )

let seq = compose

(* FIXME: use more efficient `merge_skewed` once available *)
let union t u = 
  T.merge t u ~f:(fun ~key:_ -> function
    | `Left b | `Right b -> Some b
    | `Both (b1, b2) -> Some (BDD.sum b1 b2)
  )

let big_union fdds = List.fold ~init:drop ~f:union fdds

let star' lhs t =
  let rec loop acc power =
    let power' = seq power t in
    let acc' = union acc power' in
    if equal acc acc'
      then acc
      else loop acc' power'
  in
  loop id lhs

let star t = star' id t

(** Erases (all matches on) meta field. No need to erase modifications. *)
let erase env t meta_field init =
  match init with
  | Const v ->
    let constr = Pattern.of_hv ~env (Meta (meta_field, v)) in
    restrict [constr] t
  | Alias hv ->
    let alias = Field.of_hv ~env hv in
    let meta,_ = Field.Env.lookup env meta_field in
    T.map t ~f:BDD.(fold ~f:const ~g:(fun (field,v) tru fls ->
      if field = meta then
        cond (alias, v) tru fls
      else
        cond (field,v) tru fls
    ))

let rec of_local_pol_k env p k =
  let open Syntax in
  match p with
  | Filter   p  -> k (of_pred env p)
  | Mod      m  -> k (of_mod  env m)
  | Union (p, q) -> of_local_pol_k env p (fun p' ->
                      of_local_pol_k env q (fun q' ->
                        k (union p' q')))
  | Seq (p, q) -> of_local_pol_k env p (fun p ->
                    if is_zero p then
                      k p
                    else
                      of_local_pol_k env q (fun q ->
                        k (seq p q)))
  | Star p -> of_local_pol_k env p (fun p' -> k (star p'))
  | Let { id=field; init; mut; body=p } ->
    let env = Field.Env.add env field init mut in
    of_local_pol_k env p (fun p' -> k (erase env p' field init))
  | Link _ | VLink _ | Dup -> raise Non_local

let rec of_local_pol ?(env=Field.Env.empty) p = of_local_pol_k env p ident

let to_local_pol t =
  let open Optimize in
  T.to_alist t
  |> List.map ~f:(fun (m,b) ->
    let b = BDD.fold b
      ~f:(function 
        | true -> Syntax.True 
        | false -> Syntax.False
      )
      ~g:(fun v t f ->
        let p = Pattern.to_pred v in
        mk_or (mk_and p t) (mk_and (mk_not p) f)
      )
    in
    mk_seq (mk_filter b) (Seq.to_policy m)
  )
  |> mk_big_union

let to_fdd (t : t) : FDD.t =
  T.to_alist t
  |> List.map ~f:(fun (m,b) -> Factor.to_fdd (b,m))
  |> FDD.big_union


