open Core

module Field = struct

  type t
    =
    | F0
    | F1
    | F2
    | F3
    | F4
    | Meta0
    | Meta1
    | Meta2
    | Meta3
    | Meta4
    [@@deriving sexp, enumerate, enum, eq, hash]
  type field = t

  let num_fields = max + 1

  let hash = Hashtbl.hash

  let of_string s =
    t_of_sexp (Sexp.of_string s)

  let to_string t =
    Sexp.to_string (sexp_of_t t)

  let is_valid_order (lst : t list) : bool =
    Set.Poly.(equal (of_list lst) (of_list all))

  let order = Array.init num_fields ~f:ident

  let set_order (lst : t list) : unit =
    assert (is_valid_order lst);
    List.iteri lst ~f:(fun i fld -> order.(to_enum fld) <- i)

  (* Not a clean way to invert a permutation, but fast *)
  let invert arr =
    let inverted = Array.init num_fields ~f:ident in
    Array.iteri arr ~f:(fun i elt -> inverted.(elt) <- i );
    inverted

  let get_order () =
    Array.to_list (invert order)
    |> List.filter_map ~f:of_enum

  (* compare depends on current order! *)
  let compare (x : t) (y : t) : int =
    (* using Obj.magic instead of to_enum for bettter performance *)
    Int.compare order.(Obj.magic x) order.(Obj.magic y)

  module type ENV = sig
    type t
    val empty : t
    exception Full
    val add : t -> string -> field Probnetkat.meta_init -> bool -> t (* may raise Full *)
    val lookup : t -> string -> field * (field Probnetkat.meta_init * bool) (* may raise Not_found *)
  end

  module Env : ENV = struct

    type t = {
      alist : (string * (field * (field Probnetkat.meta_init * bool))) list;
      depth : int
    }

    let empty = { alist = []; depth = 0 }

    exception Full

    let add env name init mut =
      let field =
        match env.depth with
        | 0 -> Meta0
        | 1 -> Meta1
        | 2 -> Meta2
        | 3 -> Meta3
        | 4 -> Meta4
        | _ -> raise Full
      in
      { alist = List.Assoc.add ~equal:(=) env.alist name (field, (init, mut));
        depth = env.depth + 1}

    let lookup env name =
      List.Assoc.find_exn ~equal:(=) env.alist name
  end
(*
  (* Heuristic to pick a variable order that operates by scoring the fields
     in a policy. A field receives a high score if, when a test field=X
     is false, the policy can be shrunk substantially.

     NOTE(arjun): This could be done better, but it seems to work quite well
     on FatTrees and the SDX benchmarks. Some ideas for improvement:

     - Easy: also account for setting tests field=X suceeded
     - Harder, but possibly much better: properly calculate the size of the
       pol for different field assignments. Don't traverse the policy
       repeatedly. Instead, write a size function that returns map from
       field assignments to sizes. *)
  let auto_order (pol : Probnetkat.policy) : unit =
    let open Probnetkat in
    (* Construct array of scores, where score starts at 0 for every field *)
    let count_arr = Array.init num_fields ~f:(fun _ -> 0) in
    let rec f_pred size (env, pred) = match pred with
      | True -> ()
      | False -> ()
      | Test (Probnetkat.Meta (id,_)) ->
        begin match Env.lookup env id with
        | (f, (Alias hv, false)) ->
          let f = to_enum f in
          let f' = to_enum (of_hv hv) in
          count_arr.(f) <- count_arr.(f) + size;
          count_arr.(f') <- count_arr.(f') + size
        | (f,_) ->
          let f = to_enum f in
          count_arr.(f) <- count_arr.(f) + size
        end
      | Test hv ->
        let f = to_enum (of_hv hv) in
        count_arr.(f) <- count_arr.(f) + size
      | Or (a, b) -> f_pred size (env, a); f_pred size (env, b)
      | And (a, b) -> f_pred size (env, a); f_pred size (env, b)
      | Neg a -> f_pred size (env, a) in
    let rec f_seq' pol lst env k = match pol with
      | Mod _ -> k (1, lst)
      | Filter a -> k (1, (env, a) :: lst)
      | Seq (p, q) ->
        f_seq' p lst env (fun (m, lst) ->
          f_seq' q lst env (fun (n, lst) ->
            k (m * n, lst)))
      | Union _ -> k (f_union pol env, lst)
      | Let { id; init; mut; body=p } ->
        let env = Env.add env id init mut in
        f_seq' p lst env k
      | Star p -> k (f_union p env, lst)
      | Link (sw,pt,_,_) -> k (1, (env, Test (Switch sw)) :: (env, Test (Location (Physical pt))) :: lst)
      | VLink (sw,pt,_,_) -> k (1, (env, Test (VSwitch sw)) :: (env, Test (VPort pt)) :: lst)
      | Dup -> k (1, lst)
    and f_seq pol env : int =
      let (size, preds) = f_seq' pol [] env (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    and f_union' pol lst env k = match pol with
      | Mod _ -> (1, lst)
      | Filter a -> (1, (env, a) :: lst)
      | Union (p, q) ->
        f_union' p lst env (fun (m, lst) ->
          f_union' q lst env (fun (n, lst) ->
            k (m + n, lst)))
      | Seq _ -> k (f_seq pol env, lst)
      | Let { id; init; mut; body=p } ->
        let env = Env.add env id init mut in
        k (f_seq p env, lst)
      | Star p -> f_union' p lst env k
      | Link (sw,pt,_,_) -> k (1, (env, Test (Switch sw)) :: (env, Test (Location (Physical pt))) :: lst)
      | VLink (sw,pt,_,_) -> k (1, (env, Test (VSwitch sw)) :: (env, Test (VPort pt)) :: lst)
      | Dup -> k (1, lst)
    and f_union pol env : int =
      let (size, preds) = f_union' pol [] env (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    in
    let _ = f_seq pol Env.empty in
    Array.foldi count_arr ~init:[] ~f:(fun i acc n -> ((Obj.magic i, n) :: acc))
    |> List.stable_sort ~cmp:(fun (_, x) (_, y) -> Int.compare x y)
    |> List.rev (* SJS: do NOT remove & reverse order! Want stable sort *)
    |> List.map ~f:fst
    |> set_order *)

end

module Value = struct
  include Int
  let subset_eq = equal
end

module Action = struct
  include Map.Make(Field)

  let compare = compare_direct Value.compare
  let one = empty
  let hash_fold_t = Map.hash_fold_direct Field.hash_fold_t

  let prod x y =
    (* Favor modifications to the right *)
    merge x y ~f:(fun ~key m ->
      match m with | `Both(_, v) | `Left v | `Right v -> Some(v))

  let sum x y = failwith "multicast not implemented!"

  let to_hvs = to_alist ~key_order:`Increasing

  let to_string (t : Value.t t) : string =
    let s = to_alist t
      |> List.map ~f:(fun (f,v) ->
          sprintf "%s := %s" (Field.to_string f) (Value.to_string v))
      |> String.concat ~sep:", "
    in "[" ^ s ^ "]"
end


module ActionDist = struct

  module T = Dist.Make(struct
    type t = Value.t Action.t [@@deriving sexp, eq, hash]
    let to_string = Action.to_string
    let compare = Action.compare
  end)

  include T

  let zero = T.empty
  let is_zero = T.is_empty

  let one = T.dirac Action.one
  let is_one d = match Map.find d Action.one with
    | None -> false
    | Some p when not Prob.(equal p one) -> false
    | Some _ -> Map.length d = 1

  let sum = T.sum

  let prod = T.prod_with ~f:Action.prod

  let negate t : t =
    (* This implements negation for the [zero] and [one] actions. Any
       non-[zero] action will be mapped to [zero] by this function. *)
    if is_zero t then one else zero
end




module FDD = struct

  include Vlr.Make
          (Field)
          (Value)
          (ActionDist)
  open Probnetkat

  let allocate_fields (pol : string policy)
    : Field.t policy * Field.t String.Map.t =
    let tbl : (string, Field.t) Hashtbl.t = String.Table.create () in
    let next = ref 0 in
    let do_field env (f : string) : Field.t =
      match Field.Env.lookup env f with
      | (field, _) -> field
      | exception Not_found -> String.Table.find_or_add tbl f ~default:(fun () ->
        let open Field in
        let field = match !next with
          | 0 -> F0
          | 1 -> F1
          | 2 -> F2
          | 3 -> F3
          | 4 -> F4
          | _ -> failwith "too many fields! (only up to 5 supported)"
        in incr next; field)
    in
    let rec do_pol env (p : string policy) : Field.t policy =
      match p with
      | Filter pred ->
        Filter (do_pred env pred)
      | Modify (f,v) ->
        Modify (do_field env f, v)
      | Seq (p, q) ->
        Seq (do_pol env p, do_pol env q)
      | Ite (a, p, q) ->
        Ite (do_pred env a, do_pol env p, do_pol env q)
      | While (a, p) ->
        While (do_pred env a, do_pol env p)
      | Choice dist ->
        Choice (Util.map_fst dist ~f:(do_pol env))
      | Let { id; init; mut; body; } ->
        let init = match init with
          | Alias f -> Alias (do_field env f)
          | Const v -> Const v
        in
        let env = Field.Env.add env id init mut in
        let (id,_) = Field.Env.lookup env id in
        let body = do_pol env body in
        Let { id; init; mut; body; }
    and do_pred env (p : string pred) : Field.t pred =
      match p with
      | True -> True
      | False -> False
      | Test (f, v) -> Test (do_field env f, v)
      | And (p, q) -> And (do_pred env p, do_pred env q)
      | Or (p, q) -> Or (do_pred env p, do_pred env q)
      | Neg p -> Neg (do_pred env p)
    in
    let pol = do_pol Field.Env.empty pol in
    let field_map = String.(Map.of_alist_exn (Table.to_alist tbl)) in
    (pol, field_map)

  let of_test hv =
    atom hv ActionDist.one ActionDist.zero

  let of_mod (f,v) =
    const (ActionDist.dirac (Action.singleton f v))

  let negate fdd =
    map_r fdd ~f:ActionDist.negate

  let rec of_pred p =
    match p with
    | True      -> id
    | False     -> drop
    | Test(hv)  -> of_test hv
    | And(p, q) -> prod (of_pred p) (of_pred q)
    | Or (p, q) -> sum (of_pred p) (of_pred q)
    | Neg(q)    -> negate (of_pred q)

  let seq_tbl = BinTbl.create ~size:1000 ()

  let clear_cache ~preserve = begin
    BinTbl.clear seq_tbl;
    clear_cache preserve;
  end

  let seq t u =
    match unget u with
    | Leaf _ -> prod t u (* This is an optimization. If [u] is an
                            [Action.Par.t], then it will compose with [t]
                            regardless of however [t] modifies packets. None
                            of the decision variables in [u] need to be
                            removed because there are none. *)
    | Branch _ ->
      dp_map t
        ~f:(fun dist ->
          List.map (ActionDist.to_alist dist) ~f:(fun (action, prob) ->
            restrict_map (Action.to_hvs action) u ~f:(fun leaf ->
              ActionDist.scale leaf ~scalar:prob))
          |> List.fold ~init:drop ~f:sum
        )
        ~g:(fun v t f -> cond v t f)
        ~find_or_add:(fun t -> BinTbl.find_or_add seq_tbl (t,u))

  let union t u = sum t u

  let big_union fdds = List.fold ~init:drop ~f:union fdds

  (** Erases (all matches on) meta field. No need to erase modifications. *)
  let erase t meta_field init =
    match init with
    | Const v ->
      restrict [(meta_field,v)] t
    | Alias alias ->
      fold t ~f:const ~g:(fun (field,v) tru fls ->
        if field = meta_field then
          cond (alias, v) tru fls
        else
          cond (field,v) tru fls)

  let rec of_local_pol_k p k =
    let open Probnetkat in
    match p with
    | Filter p ->
      k (of_pred p)
    | Modify m ->
      k (of_mod  m)
    | Seq (p, q) ->
      of_local_pol_k p (fun p' ->
        if equal p' drop then
          k drop
        else
          of_local_pol_k q (fun q' -> k (seq p' q')))
    | Ite (a, p, q) ->
      let a = of_pred a in
      if equal a id then
        of_local_pol_k p k
      else if equal a drop then
        of_local_pol_k q k
      else
        of_local_pol_k p (fun p ->
          of_local_pol_k q (fun q ->
            k @@ union (prod a p) (prod (negate a) q)
          )
        )
    | While (a, p) -> failwith "todo"
    | Choice dist ->
      List.map dist ~f:(fun (p, prob) ->
        of_local_pol_k p (map_r ~f:(ActionDist.scale ~scalar:prob))
      )
      |> big_union
      |> k
    | Let { id=field; init; mut; body=p } ->
      of_local_pol_k p (fun p' -> k (erase p' field init))

  and of_local_pol p = of_local_pol_k p ident

end
