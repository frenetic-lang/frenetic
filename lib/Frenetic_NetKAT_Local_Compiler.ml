open Core.Std
open Frenetic_Fdd
open Frenetic_NetKAT

module Field = Frenetic_Fdd.Field
exception Non_local = Frenetic_Fdd.Non_local

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]

module FDK = struct

  include FDK

  let of_test hv =
    atom (Pattern.of_hv hv) Action.one Action.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    const Action.(Par.singleton (Seq.singleton (F k) v))

  let rec of_pred p =
    match p with
    | True      -> mk_id ()
    | False     -> mk_drop ()
    | Test(hv)  -> of_test hv
    | And(p, q) -> prod (of_pred p) (of_pred q)
    | Or (p, q) -> sum (of_pred p) (of_pred q)
    | Neg(q)    -> map_r Action.negate (of_pred q)

  let cond v t f =
    if equal t f then
      t
    else
      (sum (prod (atom v Action.one Action.zero) t)
             (prod (atom v Action.zero Action.one) f))

  let dp_fold (g : Action.t -> 'a)
              (h : Field.t * Value.t -> 'a -> 'a -> 'a)
              (t : t) : 'a =
    let tbl = Hashtbl.Poly.create () in
    let rec f t =
      Hashtbl.Poly.find_or_add tbl t ~default:(fun () -> f' t)
    and f' t = match unget t with
      | Leaf r -> g r
      | Branch ((v, l), tru, fls) -> h (v,l) (f tru) (f fls) in
    f t

  let seq t u =
    match peek u with
    | Some _ -> prod t u (* This is an optimization. If [u] is an
                              [Action.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
    | None   ->
      dp_fold
        (fun par ->
          Action.Par.fold par ~init:(mk_drop ()) ~f:(fun acc seq ->
            let u' = restrict (Action.Seq.to_hvs seq) u in
            (sum (prod (const Action.Par.(singleton seq)) u') acc)))
        (fun v t f -> cond v t f)
      t

  let union t u =
    (* Compute the union of [t] and [u] by using the sum operation. This will
       appropriately combine actions for overlapping patterns. *)
    if equal t u then
      t
    else
      sum t u

  (* Do NOT eta-reduce to avoid caching problems with mk_drop *)
  let big_union fdds = List.fold ~init:(mk_drop ()) ~f:union fdds

  let star' lhs t = 
    let rec loop acc power =
      let power' = seq power t in
      let acc' = union acc power' in
      if equal acc acc'
        then acc
        else loop acc' power'
    in
    loop (mk_id ()) lhs

  let star t = star' (mk_id ()) t

  let rec of_local_pol_k p k =
    let open Frenetic_NetKAT in
    match p with
    | Filter   p  -> k (of_pred p)
    | Mod      m  -> k (of_mod  m)
    | Union (p, q) -> of_local_pol_k p (fun p' ->
                        of_local_pol_k q (fun q' ->
                          k (union p' q')))
    | Seq (p, q) -> of_local_pol_k p (fun p' ->
                      if FDK.equal p' (FDK.mk_drop ()) then
                        k (FDK.mk_drop ())
                      else
                        of_local_pol_k q (fun q' ->
                          k (seq p' q')))
    | Star p -> of_local_pol_k p (fun p' -> k (star p'))
    | Link (sw1, pt1, sw2, pt2) -> raise Non_local

  let rec of_local_pol p = of_local_pol_k p ident

  let to_local_pol =
    fold
      (fun r -> Action.to_policy r)
      (fun v t f ->
        let p = Pattern.to_pred v in
        match t, f with
        | Filter t, Filter f ->
          Frenetic_NetKAT_Optimize.(mk_filter (mk_or (mk_and p t)
                                                 (mk_and (mk_not p) f)))
        | _       , _        ->
          Frenetic_NetKAT_Optimize.(mk_union (mk_seq (mk_filter p) t)
                                      (mk_seq (mk_filter (mk_not p)) f)))

  let dedup fdd =
    let module FS = Set.Make(Field) in
    dp_fold
      (fun par ->
        let mods = Action.Par.to_hvs par in
        let fields = List.map mods ~f:fst |> FS.of_list in
        let harmful = Action.Par.fold par ~init:FS.empty ~f:(fun acc seq ->
          let seq_fields =
            Action.Seq.to_hvs seq |> List.map ~f:fst |> FS.of_list in
          FS.union acc (FS.diff fields seq_fields)) in
        let mods = List.filter mods ~f:(fun (f,_) -> FS.mem harmful f) in
        List.fold mods ~init:(mk_leaf par) ~f:(fun fdd test ->
          cond test (map_r (Action.demod test) fdd) fdd))
      cond
      fdd
end

(** An internal module that implements an interpreter for a [FDK.t]. This
    interpreter uses [FDK.t] operations to find the [Action.t] that should
    apply to the packet. Once that's found, it converts the [Action.t] into a
    NetKAT policy and falls back to the [NetKAT_Semantics] module to process the
    actions and produce the final [PacketSet.t] *)
module Interp = struct
  open Frenetic_NetKAT_Semantics

  let eval_to_action (packet:packet) (t:FDK.t) =
    let hvs = HeadersValues.to_hvs packet.headers in
    let sw  = (Field.Switch, Value.of_int64 packet.switch) in
    let vs  = List.map hvs ~f:Pattern.of_hv in
    let () = eprintf "In eval_to_action" in
    match FDK.(peek (restrict (sw :: vs) t)) with
    | None    -> assert false
    | Some(r) -> r

  let eval (p:packet) (t:FDK.t) =
    Frenetic_NetKAT_Semantics.eval p Action.(to_policy (eval_to_action p t))

  let eval_pipes (p:packet) (t:FDK.t) =
    let () = printf "In Interp.eval_pipes" in
    Frenetic_NetKAT_Semantics.eval_pipes p Action.(to_policy (eval_to_action p t))
end

include FDK

type cache
  = [ `Keep
    | `Empty
    | `Preserve of t ]

type compiler_options = {
    cache_prepare: cache;
    field_order: order;
    remove_tail_drops: bool;
    dedup_flows: bool;
    optimize: bool;
}

let clear_cache () = FDK.clear_cache Int.Set.empty

(* TODO: Change remove_tails_drops to default false and field order to Heuristic *)
let default_compiler_options = { 
  cache_prepare = `Empty; 
  field_order = `Static [
    Field.Switch; Field.Vlan; Field.EthSrc; Field.EthDst; Field.Location; 
    Field.VlanPcp; Field.EthType; Field.IPProto; Field.IP4Src; Field.IP4Dst;
    Field.TCPSrcPort; Field.TCPDstPort
  ]; 
  remove_tail_drops = true;
  dedup_flows = false;
  optimize = true;
}

let compile ?(options=default_compiler_options) pol =
  (match options.cache_prepare with
   | `Keep -> ()
   | `Empty -> FDK.clear_cache Int.Set.empty
   | `Preserve fdd -> FDK.clear_cache (FDK.refs fdd));
  (match options.field_order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all_fields
   | `Static flds -> Field.set_order flds);
  of_local_pol pol

let is_valid_pattern (pat : Frenetic_OpenFlow.Pattern.t) : bool =
  (Option.is_none pat.dlTyp ==>
     (Option.is_none pat.nwProto &&
      Option.is_none pat.nwSrc &&
      Option.is_none pat.nwDst)) &&
  (Option.is_none pat.nwProto ==>
     (Option.is_none pat.tpSrc &&
      Option.is_none pat.tpDst))

let mk_flow pattern action queries =
  if is_valid_pattern pattern then
    let open Frenetic_OpenFlow.Pattern in
    let open Frenetic_OpenFlow in
    Some ({ pattern
          ; action
          ; cookie = 0L
          ; idle_timeout = Permanent
          ; hard_timeout = Permanent
          }, queries)
  else
   None

let get_inport hvs =
  let get_inport' current hv =
  match hv with
    | (Field.Location, Value.Const p) -> Some p
    | _ -> current
  in
  List.fold_left hvs ~init:None ~f:get_inport'

let to_action in_port r tests =
  List.fold tests ~init:r ~f:(fun a t -> Action.demod t a)
  |> Action.to_sdn ?in_port

let to_pattern hvs =
  List.fold_right hvs ~f:Pattern.to_sdn  ~init:Frenetic_OpenFlow.Pattern.match_all

let mk_branch_or_leaf test t f =
  match t with
  | None -> Some f
  | Some t -> Some (FDK.mk_branch test t f)

let opt_to_table sw_id t =
  let t =
    FDK.(restrict [(Field.Switch, Value.Const sw_id)] t)
  in
  let rec next_table_row tests mk_rest t =
    match FDK.unget t with
    | Branch ((Location, Pipe _), _, f) ->
      next_table_row tests mk_rest f
    | Branch (test, t, f) ->
      next_table_row (test::tests) (fun t' -> mk_rest (mk_branch_or_leaf test t' f)) t
    | Leaf actions ->
      let openflow_instruction = [to_action (get_inport tests) actions tests] in
      let queries = Action.get_queries actions in
      let row = mk_flow (to_pattern tests) openflow_instruction queries in
      (row, mk_rest None)
  in
  let rec loop t acc =
    match next_table_row [] (fun x -> x) t with
    | (row, None) -> List.rev (row::acc)
    | (row, Some rest) -> loop rest (row::acc)
  in
  List.filter_opt (loop t [])

let rec naive_to_table sw_id (t : FDK.t) =
  let t = FDK.(restrict [(Field.Switch, Value.Const sw_id)] t) in
  let rec dfs tests t = match FDK.unget t with
  | Leaf actions ->
    let openflow_instruction = [to_action (get_inport tests) actions tests] in
    let queries = Action.get_queries actions in
    [mk_flow (to_pattern tests) openflow_instruction queries]
  | Branch ((Location, Pipe _), _, fls) -> dfs tests fls
  | Branch (test, tru, fls) ->
    dfs (test :: tests) tru @ dfs tests fls in
  List.filter_opt (dfs [] t)

let remove_tail_drops fl =
  let rec remove_tail_drop fl =
    match fl with 
    | [] -> fl
    | h :: t ->
      let actions = (fst h).Frenetic_OpenFlow.action in 
      match (List.concat (List.concat actions)) with 
      | [] -> remove_tail_drop t
      | _ -> h :: t in 
  List.rev (remove_tail_drop (List.rev fl))

let to_table' ?(options=default_compiler_options) swId t =
  let t = if options.dedup_flows then FDK.dedup t else t in
  let t = match options.optimize with
  | true -> opt_to_table swId t
  | false -> naive_to_table swId t in
  if options.remove_tail_drops then (remove_tail_drops t) else t

let to_table ?(options=default_compiler_options) swId t = 
  List.map ~f:fst (to_table' ~options swId t)

let pipes t =
  let ps = FDK.fold
    (fun r -> Action.pipes r)
    (fun _ t f -> Frenetic_Util.StringSet.union t f)
    t
  in
  Frenetic_Util.StringSet.to_list ps

let queries t =
  let module S = Set.Make(struct
    type t = string * Frenetic_NetKAT.pred sexp_opaque with sexp
    let compare = Pervasives.compare
  end) in
  let qs = FDK.fold
    (fun r ->
      let qs = Action.queries r in
      S.of_list (List.map qs ~f:(fun q -> (q, Frenetic_NetKAT.True))))
    (fun v t f ->
      let p = Pattern.to_pred v in
      let open Frenetic_NetKAT_Optimize in
      S.(union (map t ~f:(fun (q, p') -> (q, mk_and p p')))
               (map t ~f:(fun (q, p') -> (q, mk_and (mk_not p) p')))))
    t
  in
  S.to_list qs

let size =
  FDK.fold
    (fun r -> 1)
    (fun v t f -> 1 + t + f)

let compression_ratio t = (FDK.compressed_size t, FDK.uncompressed_size t)

let eval_to_action (packet:Frenetic_NetKAT_Semantics.packet) (t:FDK.t) =
  let open Frenetic_NetKAT_Semantics in 
  let hvs = HeadersValues.to_hvs packet.headers in
  let sw  = (Field.Switch, Value.of_int64 packet.switch) in
  let vs  = List.map hvs ~f:Pattern.of_hv in
  let () = eprintf "In eval_to_action" in
  match FDK.(peek (restrict (sw :: vs) t)) with
  | None    -> assert false
  | Some(r) -> r

let eval (p:Frenetic_NetKAT_Semantics.packet) (t:FDK.t) =
  Frenetic_NetKAT_Semantics.eval p Action.(to_policy (eval_to_action p t))

let eval_pipes (p:Frenetic_NetKAT_Semantics.packet) (t:FDK.t) =
  Frenetic_NetKAT_Semantics.eval_pipes p Action.(to_policy (eval_to_action p t))

let to_dotfile t filename =
  Out_channel.with_file filename ~f:(fun chan ->
    Out_channel.output_string chan (FDK.to_dot t))

let restrict hv t = FDK.restrict [Pattern.of_hv hv] t

let options_from_json_string s =
  let open Yojson.Basic.Util in 
  let json = Yojson.Basic.from_string s in
  let cache_prepare_string = json |> member "cache_prepare" |> to_string in
  let cache_prepare = 
    match cache_prepare_string with
    | "keep" -> `Keep
    | _ -> `Empty in
    (* TODO: Handle Preserve *)
  let field_order_string = json |> member "field_order" |> to_string in
  let field_order = 
    match field_order_string with
    | "default" -> `Default
    | "heuristic" -> `Heuristic
    | _ ->
     (* TODO: This is copypasta from Frenetic_Shell. *)
     let ls = String.split_on_chars ~on:['<'] field_order_string 
      |> List.map ~f:String.strip 
      |> List.map ~f:Field.of_string in   
     let compose f g x = f (g x) in
     let curr_order = Field.all_fields in
     let removed = List.filter curr_order (compose not (List.mem ls)) in
     (* Tags all specified Fields at the highest priority *)
     let new_order = List.append (List.rev ls) removed in
     (`Static new_order) in
  let remove_tail_drops = json |> member "remove_tail_drops" |> to_bool in
  let dedup_flows = json |> member "dedup_flows" |> to_bool in
  let optimize = json |> member "optimize" |> to_bool in
  {cache_prepare; field_order; remove_tail_drops; dedup_flows; optimize}

let field_order_to_string fo = 
 match fo with
  | `Heuristic -> "heuristic"
  | `Default -> "default"
  | `Static fields ->
     List.rev (List.map fields Field.to_string) |> String.concat ~sep:" < "

let options_to_json_string opt =
  let open Yojson.Basic in 
  `Assoc [
    ("cache_prepare", `String (if opt.cache_prepare = `Keep then "keep" else "empty"));
    ("field_order", `String (field_order_to_string opt.field_order));
    ("remove_tail_drops", `Bool opt.remove_tail_drops);
    ("dedup_flows", `Bool opt.dedup_flows);
    ("optimze", `Bool opt.optimize)
  ] |> pretty_to_string 

(* *** GLOBAL COMPILATION *)

(* internal policy representation that allows to inject fdks into policies *)
module Pol = struct

  type policy =
    | Filter of pred
    | Mod of header_val
    | Union of policy * policy
    | Seq of policy * policy
    | Star of policy
    | Dup (* we can handle all of NetKAT *)
    | FDK of FDK.t * FDK.t (* FDK injection. E and D matrix. *)

  let drop = Filter False
  let id = Filter True

  let mk_filter pred = Filter pred
  let mk_mod hv = Mod hv

  let mk_union pol1 pol2 =
    match pol1, pol2 with
    | Filter False, _ -> pol2
    | _, Filter False -> pol1
    | _ -> Union (pol1,pol2)

  let mk_seq pol1 pol2 =
    match pol1, pol2 with
    | Filter True, _ -> pol2
    | _, Filter True -> pol1
    | Filter False, _ | _, Filter False -> drop
    | _ -> Seq (pol1,pol2)

  let mk_star pol =
    match pol with
    | Filter True | Filter False -> id
    | Star _ -> pol
    | _ -> Star(pol)

  let mk_fdk e d =
    let drop' = FDK.mk_drop () in
    if FDK.equal e drop' && FDK.equal d drop' then drop
    else FDK (e, d)

  let mk_big_union = List.fold ~init:drop ~f:mk_union
  let mk_big_seq = List.fold ~init:id ~f:mk_seq

  let match_loc sw pt =
    let t1 = Test (Switch sw) in
    let t2 = Test (Location (Physical pt)) in
    Frenetic_NetKAT_Optimize.mk_and t1 t2

  let filter_loc sw pt = match_loc sw pt |> mk_filter

  let rec of_pol (ing : Frenetic_NetKAT.pred option) (pol : Frenetic_NetKAT.policy) : policy =
    match pol with
    | Filter a -> Filter a
    | Mod hv -> Mod hv
    | Union (p,q) -> Union (of_pol ing p, of_pol ing q)
    | Seq (p,q) -> Seq (of_pol ing p, of_pol ing q)
    | Star p -> Star (of_pol ing p)
    | Link (s1,p1,s2,p2) ->
      (* SJS: This is not the true sematnics of a link! This is a hack that works for now,
         but we will need to use the correct encoding once we start doing things like global
         optimization or deciding equivalence. *)
      let post_link = match ing with
        | None -> filter_loc s2 p2
        | Some ing -> Frenetic_NetKAT_Optimize.mk_and (Test (Switch s2)) (Frenetic_NetKAT_Optimize.mk_not ing) |> mk_filter in
      mk_big_seq [filter_loc s1 p1; Dup; post_link ]

end



(* Symbolic NetKAT Automata *)
module NetKAT_Automaton = struct

  (* table *)
  module T = Hashtbl.Make(struct
    type t = int with sexp
    let hash n = n
    let compare = Int.compare
  end)

  (* untable *)
  module U = Hashtbl.Make(struct
    type t = (int * int) with sexp
    let hash (t1, t2) = 617 * t1 +  619 * t2
    let compare = Pervasives.compare
  end)

  (* (hashable) set *)
  module S = struct
    module S = struct
      include Set.Make(Int)
      let hash = Hashtbl.hash
    end
    include Hashable.Make(S)
    include S
  end

  (* main data structure of symbolic NetKAT automaton *)
  type t =
    { states : (FDK.t * FDK.t) T.t;
      has_state : int U.t;
      mutable source : int;
      mutable nextState : int }

  (* lazy intermediate presentation to avoid compiling uncreachable automata states *)
  type t0 =
    { states : (FDK.t * FDK.t) Lazy.t T.t;
      source : int;
      mutable nextState : int }

  let create_t0 () : t0 =
    let states = T.create () ~size:10 in
    let source = 0 in
    { states; source; nextState = source+1 }

  let create_t () : t =
    let states = T.create () ~size:10 in
    let has_state = U.create () ~size:10 in
    let source = 0 in
    { states; has_state; source; nextState = source+1 }

  let mk_state_t0 (automaton : t0) =
    let id = automaton.nextState in
    automaton.nextState <- id + 1;
    id

  let mk_state_t (automaton : t) =
    let id = automaton.nextState in
    automaton.nextState <- id + 1;
    id

  let add_to_t (automaton : t) (fdks : (FDK.t * FDK.t)) =
    match U.find automaton.has_state fdks with
    | Some k -> k
    | None ->
      let k = mk_state_t automaton in
      T.add_exn automaton.states ~key:k ~data:fdks;
      U.add_exn automaton.has_state ~key:fdks ~data:k;
      k

  let map_reachable ?(order = `Pre) (automaton : t) ~(f: int -> (FDK.t * FDK.t) -> (FDK.t * FDK.t)) : unit =
    let rec loop seen (id : int) =
      if S.mem seen id then seen else
        let seen = S.add seen id in
        let fdks = T.find_exn automaton.states id in
        let this seen =
          let fdks = f id fdks in
          T.replace automaton.states ~key:id ~data:fdks; (seen, fdks) in
        let that (seen, (_,d)) = List.fold (FDK.conts d) ~init:seen ~f:loop in
        match order with
        | `Pre -> seen |> this |> that
        | `Post -> (seen, fdks) |> that |> this |> fst
    in
    loop S.empty automaton.source |> ignore

  let fold_reachable ?(order = `Pre) (automaton : t) ~(init : 'a) ~(f: 'a -> int -> (FDK.t * FDK.t) -> 'a) =
    let rec loop (acc, seen) (id : int) =
      if S.mem seen id then (acc, seen) else
        let seen = S.add seen id in
        let (_,d) as fdks = T.find_exn automaton.states id in
        let this (acc, seen) = (f acc id fdks, seen) in
        let that (acc, seen) = List.fold (FDK.conts d) ~init:(acc, seen) ~f:loop in
        match order with
        | `Pre -> (acc, seen) |> this |> that
        | `Post -> (acc, seen) |> that |> this
    in
    loop (init, S.empty) automaton.source |> fst

  let iter_reachable ?(order = `Pre) (automaton : t) ~(f: int -> (FDK.t * FDK.t) -> unit) : unit =
    fold_reachable automaton ~order ~init:() ~f:(fun _ -> f)

  let t_of_t0' (automaton : t0) =
    let t = create_t () in
    let rec add id =
      if not (T.mem t.states id) then
        let _ = t.nextState <- max t.nextState (id + 1) in
        let (_,d) as fdk = Lazy.force (T.find_exn automaton.states id) in
        T.add_exn t.states ~key:id ~data:fdk;
        List.iter (FDK.conts d) ~f:add
    in
    add automaton.source;
    t.source <- automaton.source;
    t

  let t_of_t0 ?(remove_duplicates=false) (automaton : t0) =
    if not remove_duplicates then t_of_t0' automaton else
    let t = create_t () in
    let ktbl = U.create () ~size:10 in
    let rec loop id =
      let fdk = T.find_exn automaton.states id in
      let seen = Lazy.is_val fdk in
      let (e,d) = Lazy.force fdk in
      if seen then match U.find ktbl (e,d) with
        | None -> failwith "cyclic FDKG"
        | Some k -> k
      else
        let d' = FDK.map_r (fun par -> Action.Par.map par ~f:(fun seq ->
          Action.(Seq.change seq K (function None -> None | Some id ->
            loop (Value.to_int_exn id) |> Value.of_int |> Option.some)))) d in
        let k = add_to_t t (e,d') in
        U.set ktbl ~key:(e,d) ~data:k;
        k
    in
    let source = loop automaton.source in
    t.source <- source;
    t


  let dedup_global (automaton : t) : unit =
    let tbl = S.Table.create () ~size:10 in
    let untbl = Int.Table.create () ~size:10 in
    let unmerge k = Int.Table.find untbl k |> Option.value ~default:[k] in
    let merge ks =
      let () = assert (List.length ks > 1) in
      let ks = List.concat_map ks ~f:unmerge in
      let ks_set = S.of_list ks in
      match S.Table.find tbl ks_set with
      | Some k -> k
      | None ->
        let (es, ds) =
          List.map ks ~f:(T.find_exn automaton.states)
          |> List.unzip in
        let fdk = (FDK.big_union es, FDK.big_union ds) in
        let k = add_to_t automaton fdk in
        S.Table.add_exn tbl ~key:ks_set ~data:k;
        Int.Table.add_exn untbl ~key:k ~data:ks;
        k
    in
    let dedup_action par =
      par
      |> Action.Par.to_list
      |> List.group ~break:(fun s1 s2 -> not (Action.Seq.equal_mod_k s1 s2))
      |> List.map ~f:(function
        | [seq] -> seq
        | group ->
          let ks = List.map group ~f:(fun s -> Action.Seq.find_exn s K |> Value.to_int_exn) in
          let k = merge ks in
          List.hd_exn group |> Action.Seq.add ~key:K ~data:(Value.of_int k))
      |> Action.Par.of_list
    in
    let dedup_fdk = FDK.map_r dedup_action in
    map_reachable automaton ~order:`Pre ~f:(fun _ (e,d) -> (e, dedup_fdk d))




  let rec split_pol (automaton : t0) (pol: Pol.policy) : FDK.t * FDK.t * ((int * Pol.policy) list) =
    match pol with
    | Filter pred -> (FDK.of_pred pred, FDK.mk_drop (), [])
    | Mod hv -> (FDK.of_mod hv, FDK.mk_drop (), [])
    | Union (p,q) ->
      let (e_p, d_p, k_p) = split_pol automaton p in
      let (e_q, d_q, k_q) = split_pol automaton q in
      let e = FDK.union e_p e_q in
      let d = FDK.union d_p d_q in
      let k = k_p @ k_q in
      (e, d, k)
    | Seq (p,q) ->
      (* TODO: short-circuit *)
      let (e_p, d_p, k_p) = split_pol automaton p in
      let (e_q, d_q, k_q) = split_pol automaton q in
      let e = FDK.seq e_p e_q in
      let d = FDK.union d_p (FDK.seq e_p d_q) in
      let q' = Pol.mk_fdk e_q d_q in
      let k = (List.map k_p ~f:(fun (id,p) -> (id, Pol.mk_seq p q'))) @ k_q in
      (e, d, k)
    | Star p ->
      let (e_p, d_p, k_p) = split_pol automaton p in
      let e = FDK.star e_p in
      let d = FDK.seq e d_p in
      let pol' = Pol.mk_fdk e d in
      let k = List.map k_p ~f:(fun (id,k) -> (id, Pol.mk_seq k pol')) in
      (e, d, k)
    | Dup ->
      let id = mk_state_t0 automaton in
      let e = FDK.mk_drop () in
      let d = FDK.mk_cont id in
      let k = [(id, Pol.id)] in
      (e, d, k)
    | FDK (e,d) -> (e,d,[])

  let rec add_policy (automaton : t0) (id, pol : int * Pol.policy) : unit =
    let f () =
      let (e,d,k) = split_pol automaton pol in
      List.iter k ~f:(add_policy automaton);
      (e, d)
    in
    T.add_exn automaton.states ~key:id ~data:(Lazy.from_fun f)

  let of_policy ?(dedup=true) ?ing ?(remove_duplicates=false) (pol : Frenetic_NetKAT.policy) : t =
    let automaton = create_t0 () in
    let pol = Pol.of_pol ing pol in
    let () = add_policy automaton (automaton.source, pol) in
    let automaton = t_of_t0 ~remove_duplicates automaton in
    let () = if dedup then dedup_global automaton in
    automaton


  let pc_unused pc fdd =
    FDK.fold
      (fun par -> Action.Par.for_all par ~f:(fun seq -> not (Action.(Seq.mem seq (F pc)))))
      (fun (f,_) l r -> l && r && f<>pc)
      fdd

  let to_local (pc : Field.t) (automaton : t) : FDK.t =
    fold_reachable automaton ~init:(FDK.mk_drop ()) ~f:(fun acc id (e,d) ->
      let _ = assert (pc_unused pc e && pc_unused pc d) in
      let guard =
        if id = automaton.source then FDK.mk_id ()
        else FDK.atom (pc, Value.of_int id) Action.one Action.zero in
      let fdk = FDK.seq guard (FDK.union e d) in
      FDK.union acc fdk)


  (* SJS: horrible hack *)
  let to_dot (automaton : t) =
    let states = T.map automaton.states ~f:(fun (e,d) -> FDK.union e d) in
    let open Format in
    let buf = Buffer.create 200 in
    let fmt = formatter_of_buffer buf in
    let seen = T.create () ~size:20 in
    pp_set_margin fmt (1 lsl 29);
    fprintf fmt "digraph fdk {@\n";
    let rec node_loop node =
      if not (T.mem seen node) then begin
        T.add_exn seen node ();
        match FDK.unget node with
        | Leaf par ->
          let seqId = ref 0 in
          let edges = ref [] in
          fprintf fmt "subgraph cluster_%d {@\n" node;
          fprintf fmt "\trank = sink;@\n" ;
          fprintf fmt "\tshape = box;@\n" ;
          fprintf fmt "\t%d [shape = point];@\n" node;
          Action.Par.iter par ~f:(fun seq ->
            let id = sprintf "\"%dS%d\"" node (!seqId) in
            let cont = Action.Seq.find seq K |> Option.map ~f:(fun v -> T.find_exn states (Value.to_int_exn v)) in
            let label = Action.to_string (Action.Par.singleton seq) in
            fprintf fmt "\t%s [shape=box, label=\"%s\"];@\n" id label;
            Option.iter cont ~f:(fun k ->
              edges := sprintf "%s -> %d [style=bold, color=blue];@\n" id k :: (!edges));
            incr seqId;
          );
          fprintf fmt "}@\n";
          List.iter (!edges) ~f:(fprintf fmt "%s")
        | Branch((f, v), a, b) ->
          fprintf fmt "%d [label=\"%s = %s\"];@\n"
            node (Field.to_string f) (Value.to_string v);
          fprintf fmt "%d -> %d;@\n" node a;
          fprintf fmt "%d -> %d [style=\"dashed\"];@\n" node b;
          node_loop a;
          node_loop b
      end
    in
    let fdks = ref [] in
    let rec fdk_loop fdkId =
      let fdk = T.find_exn states fdkId in
      let conts = FDK.conts fdk in
      fdks := fdk :: (!fdks);
      node_loop fdk;
      List.iter conts ~f:fdk_loop
    in
    fdk_loop automaton.source;
    fprintf fmt "%d [style=bold, color=red];@\n" (T.find_exn states automaton.source);
    fprintf fmt "{rank=source; ";
    List.iter (!fdks) ~f:(fun fdk -> fprintf fmt "%d " fdk);
    fprintf fmt ";}@\n";
    fprintf fmt "}@.";
    Buffer.contents buf

end

let compile_global (pol : Frenetic_NetKAT.policy) : FDK.t =
  NetKAT_Automaton.(to_local Field.Vlan (of_policy ~dedup:true pol))