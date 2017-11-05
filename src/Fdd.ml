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
    val add : t -> string -> Probnetkat.meta_init -> bool -> t (* may raise Full *)
    val lookup : t -> string -> field * (Probnetkat.meta_init * bool) (* may raise Not_found *)
  end

  module Env : ENV = struct

    type t = {
      alist : (string * (field * (Probnetkat.meta_init * bool))) list;
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

module Pattern = struct
  type t = Field.t * Value.t
  [@@deriving compare, eq, hash]

  let to_string (f, v) =
    Printf.sprintf "%s = %s" (Field.to_string f) (Value.to_string v)

  let of_hv ?(env=Field.Env.empty) hv =
    failwith "not implemented"
(*     let open Probnetkat in
    match hv with
    | Switch sw_id -> (Field.Switch, Value.(Const sw_id))
    | Location(Physical p) -> (Field.Location, Value.of_int32 p)
    | From loc -> (Field.From, Value.AbstractLocation loc)
    | AbstractLoc loc -> (Field.AbstractLoc, Value.AbstractLocation loc)
    (* TODO(grouptable): value hack *)
    | Location(FastFail p_lst) -> (Field.Location, Value.(FastFail p_lst))
    | Location(Pipe p)  -> (Field.Location, Value.(Pipe p))
    | Location(Query p) -> (Field.Location, Value.(Query p))
    | EthSrc(dlAddr) -> (Field.EthSrc, Value.(Const dlAddr))
    | EthDst(dlAddr) -> (Field.EthDst, Value.(Const dlAddr))
    | Vlan(vlan) -> (Field.Vlan, Value.of_int vlan)
    | VlanPcp(vlanPcp) -> (Field.VlanPcp, Value.of_int vlanPcp)
    | VSwitch(vsw_id) -> (Field.VSwitch, Value.(Const vsw_id))
    | VPort(vpt) ->  (Field.VPort, Value.(Const vpt))
    | EthType(dlTyp) -> (Field.EthType, Value.of_int dlTyp)
    | IPProto(nwProto) -> (Field.IPProto, Value.of_int nwProto)
    | IP4Src(nwAddr, mask) ->
      (Field.IP4Src, Value.(Mask(Int64.of_int32 nwAddr, 32 + (Int32.to_int_exn mask))))
    | IP4Dst(nwAddr, mask) ->
      (Field.IP4Dst, Value.(Mask(Int64.of_int32 nwAddr, 32 + (Int32.to_int_exn mask))))
    | TCPSrcPort(tpPort) -> (Field.TCPSrcPort, Value.of_int tpPort)
    | TCPDstPort(tpPort) -> (Field.TCPDstPort, Value.of_int tpPort)
    | VFabric(vfab) -> (Field.VFabric, Value.(Const vfab))
    | Meta(name,v) -> (fst (Field.Env.lookup env name), Value.(Const v)) *)

(*   let to_hv (f, v) =
    let open Field in
    let open Value in
    match f, v with
    | (Switch  , Const sw) -> Probnetkat.Switch sw
    | (Location, Const p) -> Probnetkat.(Location (Physical (to_int32 p)))
    | (Location, Pipe  p) -> Probnetkat.(Location (Pipe p))
    | (Location, Query q) -> Probnetkat.(Location (Query q))
    | (From, AbstractLocation l) -> Probnetkat.From l
    | (AbstractLoc, AbstractLocation l) -> Probnetkat.AbstractLoc l
    | (EthSrc  , Const dlAddr) -> Probnetkat.(EthSrc dlAddr)
    | (EthDst  , Const dlAddr) -> Probnetkat.(EthDst dlAddr)
    | (Vlan    , Const vlan) -> Probnetkat.(Vlan(to_int vlan))
    | (VlanPcp , Const vlanPcp) -> Probnetkat.(VlanPcp (to_int vlanPcp))
    | (VSwitch  , Const vsw) -> Probnetkat.VSwitch vsw
    | (VPort  , Const vpt) -> Probnetkat.VPort vpt
    | (EthType , Const dlTyp) -> Probnetkat.(EthType (to_int dlTyp))
    | (IPProto , Const nwProto) -> Probnetkat.(IPProto (to_int nwProto))
    | (IP4Src  , Mask(nwAddr, mask)) -> Probnetkat.(IP4Src(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Src  , Const nwAddr) -> Probnetkat.(IP4Src(to_int32 nwAddr, 32l))
    | (IP4Dst  , Mask(nwAddr, mask)) -> Probnetkat.(IP4Dst(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Dst  , Const nwAddr) -> Probnetkat.(IP4Dst(to_int32 nwAddr, 32l))
    | (TCPSrcPort, Const tpPort) -> Probnetkat.(TCPSrcPort(to_int tpPort))
    | (TCPDstPort, Const tpPort) -> Probnetkat.(TCPDstPort(to_int tpPort))
    | (VFabric, Const vfab) -> Probnetkat.VFabric vfab
    | _, _ -> raise (FieldValue_mismatch(f, v))

  let to_pred (f, v) =
    Probnetkat.Test (to_hv (f, v)) *)

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

  let allocate_fields (pol : string Probnetkat.policy) 
    : Field.t Probnetkat.policy * Field.t String.Map.t =
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
    let open Probnetkat in
    let rec do_pol env (p : string policy) : Field.t policy =
      match p with
      | Filter pred ->
        Filter (do_pred env pred)
      | Modify (f,v) ->
        Modify (do_field env f, v)
      | Ite (a, p, q) ->
        Ite (do_pred env a, do_pol env p, do_pol env q)
      | While (a, p) ->
        While (do_pred env a, do_pol env p)
      | Choice dist ->
        Choice (Util.map_fst dist ~f:(do_pol env))
      | Let { id : string; init : meta_init; mut : bool; body : 'pol } ->
        let env = Field.Env.add env id init mut in
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

  let of_test env hv =
    atom (Pattern.of_hv ~env hv) ActionDist.one ActionDist.zero

end
