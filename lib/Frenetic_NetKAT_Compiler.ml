open Core.Std
open Frenetic_Fdd
open Frenetic_NetKAT

module Field = Frenetic_Fdd.Field
exception Non_local = Frenetic_NetKAT.Non_local

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]

module Action = Frenetic_Fdd.Action
module Value = Frenetic_Fdd.Value
module Par = Action.Par
module Seq = Action.Seq


(*==========================================================================*)
(* LOCAL COMPILATION                                                        *)
(*==========================================================================*)

(* the shared intermediate representation of the local & global compiler *)
module FDK = struct

  include FDK

  let of_test hv =
    atom (Pattern.of_hv hv) Action.one Action.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    const Action.(Par.singleton (Seq.singleton (F k) v))

  let rec of_pred p =
    match p with
    | True      -> id
    | False     -> drop
    | Test(hv)  -> of_test hv
    | And(p, q) -> prod (of_pred p) (of_pred q)
    | Or (p, q) -> sum (of_pred p) (of_pred q)
    | Neg(q)    -> map_r Action.negate (of_pred q)

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
      dp_map
        (fun par ->
          Action.Par.fold par ~init:drop ~f:(fun acc seq ->
            let u' = restrict (Action.Seq.to_hvs seq) u in
            (sum (prod (const Action.Par.(singleton seq)) u') acc)))
        (fun v t f -> cond v t f)
        ~find_or_add:(fun t -> BinTbl.find_or_add seq_tbl (t,u))
        t

  let union t u = sum t u

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

  let rec of_local_pol_k p k =
    let open Frenetic_NetKAT in
    match p with
    | Filter   p  -> k (of_pred p)
    | Mod      m  -> k (of_mod  m)
    | Union (p, q) -> of_local_pol_k p (fun p' ->
                        of_local_pol_k q (fun q' ->
                          k (union p' q')))
    | Seq (p, q) -> of_local_pol_k p (fun p' ->
                      if FDK.equal p' FDK.drop then
                        k FDK.drop
                      else
                        of_local_pol_k q (fun q' ->
                          k (seq p' q')))
    | Star p -> of_local_pol_k p (fun p' -> k (star p'))
    | Link _ | VLink _ -> raise Non_local

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
    FDK.map
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
    match FDK.(unget (restrict (sw :: vs) t)) with
    | Leaf r -> r
    | Branch _ -> assert false

  let eval (p:packet) (t:FDK.t) =
    Frenetic_NetKAT_Semantics.eval p Action.(to_policy (eval_to_action p t))

  let eval_pipes (p:packet) (t:FDK.t) =
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

let default_compiler_options = {
  cache_prepare = `Empty;
  field_order = `Heuristic;
  remove_tail_drops = false;
  dedup_flows = true;
  optimize = true;
}

let prepare_compilation ~options pol = begin
  (match options.cache_prepare with
   | `Keep -> ()
   | `Empty -> FDK.clear_cache ~preserve:Int.Set.empty
   | `Preserve fdd -> FDK.clear_cache ~preserve:(FDK.refs fdd));
  (match options.field_order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all
   | `Static flds -> Field.set_order flds)
end

let compile_local ?(options=default_compiler_options) pol =
  prepare_compilation ~options pol; of_local_pol pol

let is_valid_pattern options (pat : Frenetic_OpenFlow.Pattern.t) : bool =
  (Option.is_none pat.dlTyp ==>
     (Option.is_none pat.nwProto &&
      Option.is_none pat.nwSrc &&
      Option.is_none pat.nwDst)) &&
  (Option.is_none pat.nwProto ==>
     (Option.is_none pat.tpSrc &&
      Option.is_none pat.tpDst))

let add_dependency_if_unseen all_tests pat dep =
  let dep_pat = Pattern.of_hv dep in
  match List.exists ~f:(Pattern.equal dep_pat) all_tests with
  | true -> None
  | false -> Some (Pattern.to_sdn dep_pat pat)

(* Note that although Vlan and VlanPcp technically have a dependency on the packet being an EthType 0x8100, you
   never have to include that Dependency in OpenFlow because the EthType match is always for the INNER packet. *)
let fill_in_dependencies all_tests (pat : Frenetic_OpenFlow.Pattern.t) =
  let open Frenetic_OpenFlow.Pattern in
  let all_dependencies =
    if (Option.is_some pat.nwSrc || Option.is_some pat.nwDst) && Option.is_none pat.dlTyp then
      [ EthType(0x800); EthType(0x806) ]
    else if Option.is_some pat.nwProto && Option.is_none pat.dlTyp then
      [ EthType(0x800) ]
    else if (Option.is_some pat.tpSrc || Option.is_some pat.tpDst) && Option.is_none pat.nwProto then
      [ IPProto(6); IPProto(17) ]
    else
      []
    in
  (* tpSrc/tpDst has two dependencies, so fold the dlTyp into the given pattern, if needed. *)
  let pat =
    if (Option.is_some pat.tpSrc || Option.is_some pat.tpDst) && Option.is_none pat.nwProto && Option.is_none pat.dlTyp then
      match add_dependency_if_unseen all_tests pat (EthType(0x800)) with
      | Some new_pat -> new_pat
      | None -> pat
    else
      pat
    in
  match all_dependencies with
  | [] -> [ pat ]
  | deps -> List.filter_opt (List.map deps ~f:(add_dependency_if_unseen all_tests pat))

let to_pattern hvs =
  List.fold_right hvs ~f:Pattern.to_sdn  ~init:Frenetic_OpenFlow.Pattern.match_all

let mk_flows options true_tests all_tests action queries =
  let open Frenetic_OpenFlow.Pattern in
  let open Frenetic_OpenFlow in
  let patterns = to_pattern true_tests |> fill_in_dependencies all_tests in
  List.map patterns ~f:(fun p ->
    ({ pattern=p; action; cookie=0L; idle_timeout=Permanent; hard_timeout=Permanent }, queries)
  )

let get_inport hvs =
  let get_inport' current hv =
  match hv with
    | (Field.Location, Value.Const p) -> Some p
    | _ -> current
  in
  List.fold_left hvs ~init:None ~f:get_inport'

let to_action ?group_tbl (in_port : Int64.t option) r tests =
  List.fold tests ~init:r ~f:(fun a t -> Action.demod t a)
  |> Action.to_sdn ?group_tbl in_port

let remove_local_fields = FDK.fold
  (fun r -> mk_leaf (Action.Par.map r ~f:(fun s -> Action.Seq.filteri s ~f:(fun ~key ~data ->
    match key with
    | Action.F VPort | Action.F VSwitch -> false
    | _ -> true))))
  (fun v t f ->
    match v with
    | Field.VSwitch, _ | Field.VPort, _ -> failwith "uninitialized local field"
    | _, _ -> mk_branch v t f)

let mk_branch_or_leaf test t f =
  match t with
  | None -> Some f
  | Some t -> Some (FDK.mk_branch test t f)

let opt_to_table ?group_tbl options sw_id t =
  let t =
    t
    |> restrict [(Field.Switch, Value.Const sw_id)
                ;(Field.VFabric, Value.Const (Int64.of_int 1))]
    |> remove_local_fields
  in
  let rec next_table_row true_tests all_tests mk_rest t =
    match FDK.unget t with
    | Branch ((Location, Pipe _), _, f) ->
      next_table_row true_tests all_tests mk_rest f
    | Branch (test, t, f) ->
      next_table_row (test::true_tests) (test::all_tests) (fun t' all_tests -> mk_rest (mk_branch_or_leaf test t' f) (test::all_tests)) t
    | Leaf actions ->
      let openflow_instruction = [to_action ?group_tbl (get_inport true_tests) actions true_tests] in
      let queries = Action.get_queries actions in
      let row = mk_flows options true_tests all_tests openflow_instruction queries in
      (row, mk_rest None all_tests)
  in
  let rec loop t all_tests acc =
    match next_table_row [] all_tests (fun x all_tests -> (x, all_tests) ) t with
    | (row, (None, _)) -> List.rev (row::acc)
    | (row, (Some rest, all_tests)) -> loop rest all_tests (row::acc)
  in
  (loop t [] []) |> List.concat

let rec naive_to_table ?group_tbl options sw_id (t : FDK.t) =
  let t = FDK.(restrict [(Field.Switch, Value.Const sw_id)] t) |> remove_local_fields in
  let rec dfs true_tests all_tests t = match FDK.unget t with
  | Leaf actions ->
    let openflow_instruction = [to_action ?group_tbl (get_inport true_tests) actions true_tests] in
    let queries = Action.get_queries actions in
    [ mk_flows options true_tests all_tests openflow_instruction queries ]
  | Branch ((Location, Pipe _), _, fls) -> dfs true_tests all_tests fls
  | Branch (test, tru, fls) ->
    dfs (test :: true_tests) (test :: all_tests) tru @ dfs true_tests (test :: all_tests) fls in
  (dfs [] [] t) |> List.concat

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

let to_table' ?(options=default_compiler_options) ?group_tbl swId t =
  let t = if options.dedup_flows then FDK.dedup t else t in
  let t = match options.optimize with
  | true -> opt_to_table ?group_tbl options swId t
  | false -> naive_to_table ?group_tbl options swId t in
  if options.remove_tail_drops then (remove_tail_drops t) else t

let to_table ?(options=default_compiler_options) ?group_tbl swId t =
  List.map ~f:fst (to_table' ~options ?group_tbl swId t)

let pipes t =
  let ps = FDK.fold
    (fun r -> Action.pipes r)
    (fun _ t f -> String.Set.union t f)
    t
  in
  String.Set.to_list ps

let queries t =
  let module S = Set.Poly in
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
  match FDK.(unget (restrict (sw :: vs) t)) with
  | Leaf r -> r
  | Branch _ -> assert false

let eval (p:Frenetic_NetKAT_Semantics.packet) (t:FDK.t) =
  Frenetic_NetKAT_Semantics.eval p Action.(to_policy (eval_to_action p t))

let eval_pipes (p:Frenetic_NetKAT_Semantics.packet) (t:FDK.t) =
  Frenetic_NetKAT_Semantics.eval_pipes p Action.(to_policy (eval_to_action p t))

let to_dotfile t filename =
  let t = remove_local_fields t in
  Out_channel.with_file filename ~f:(fun chan ->
    Out_channel.output_string chan (FDK.to_dot t))

let restrict hv t = FDK.restrict [Pattern.of_hv hv] t

let field_order_from_string = function
  | "default" -> `Default
  | "heuristic" -> `Heuristic
  | field_order_string ->
   let ls = String.split_on_chars ~on:['<'] field_order_string
    |> List.map ~f:String.strip
    |> List.map ~f:Field.of_string in
   let compose f g x = f (g x) in
   let curr_order = Field.all in
   let removed = List.filter curr_order (compose not (List.mem ls)) in
   (* Tags all specified Fields at the highest priority *)
   let new_order = List.append (List.rev ls) removed in
   (`Static new_order)

let options_from_json_string s =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string s in
  let cache_prepare_string = json |> member "cache_prepare" |> to_string in
  (*  Note: Preserve is not settable with JSON because it requires a complex data structure *)
  let cache_prepare =
    match cache_prepare_string with
    | "keep" -> `Keep
    | _ -> `Empty in
  let field_order = field_order_from_string (json |> member "field_order" |> to_string) in
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
    ("optimize", `Bool opt.optimize)
  ] |> pretty_to_string


(*==========================================================================*)
(* GLOBAL COMPILATION                                                       *)
(*==========================================================================*)


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
    if FDK.equal e FDK.drop && FDK.equal d FDK.drop then drop
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
        | Some ing ->
            Frenetic_NetKAT_Optimize.(mk_and (Test (Switch s2)) (mk_not ing))
            |> mk_filter
      in
      mk_big_seq [filter_loc s1 p1; Dup; post_link ]
    | VLink _ -> assert false (* SJS / JNF *)
end



(* Symbolic NetKAT Automata *)
module NetKAT_Automaton = struct

  (* table *)
  module Tbl = Int.Table

  (* untable (inverse table) *)
  module Untbl = FDK.BinTbl

  (* (hashable) int sets *)
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
    { states : (FDK.t * FDK.t) Tbl.t;
      has_state : int Untbl.t;
      mutable source : int;
      mutable nextState : int }

  (* lazy intermediate presentation to avoid compiling uncreachable automata states *)
  type t0 =
    { states : (FDK.t * FDK.t) Lazy.t Tbl.t;
      source : int;
      mutable nextState : int }

  let create_t0 () : t0 =
    let states = Tbl.create () ~size:100 in
    let source = 0 in
    { states; source; nextState = source+1 }

  let create_t () : t =
    let states = Tbl.create () ~size:100 in
    let has_state = Untbl.create () ~size:100 in
    let source = 0 in
    { states; has_state; source; nextState = source+1 }

  let mk_state_t0 (automaton : t0) : int =
    let id = automaton.nextState in
    automaton.nextState <- id + 1;
    id

  let mk_state_t (automaton : t) : int =
    let id = automaton.nextState in
    automaton.nextState <- id + 1;
    id

  let add_to_t (automaton : t) (state : (FDK.t * FDK.t)) : int =
    match Untbl.find automaton.has_state state with
    | Some k -> k
    | None ->
      let k = mk_state_t automaton in
      Tbl.add_exn automaton.states ~key:k ~data:state;
      Untbl.add_exn automaton.has_state ~key:state ~data:k;
      k

  let add_to_t_with_id (automaton : t) (state : (FDK.t * FDK.t)) (id : int) : unit = begin
      assert (not (Tbl.mem automaton.states id));
      Tbl.add_exn automaton.states ~key:id ~data:state;
      Untbl.set automaton.has_state ~key:state ~data:id;
    end

  let map_reachable ?(order = `Pre) (automaton : t) ~(f: int -> (FDK.t * FDK.t) -> (FDK.t * FDK.t)) : unit =
    let rec loop seen (id : int) =
      if S.mem seen id then seen else
        let seen = S.add seen id in
        let state = Tbl.find_exn automaton.states id in
        let this seen =
          let state = f id state in
          Tbl.set automaton.states ~key:id ~data:state; (seen, state) in
        let that (seen, (_,d)) = Set.fold (FDK.conts d) ~init:seen ~f:loop in
        match order with
        | `Pre -> seen |> this |> that
        | `Post -> (seen, state) |> that |> this |> fst
    in
    loop S.empty automaton.source |> ignore

  let fold_reachable ?(order = `Pre) (automaton : t) ~(init : 'a) ~(f: 'a -> int -> (FDK.t * FDK.t) -> 'a) =
    let rec loop (acc, seen) (id : int) =
      if S.mem seen id then (acc, seen) else
        let seen = S.add seen id in
        let (_,d) as state = Tbl.find_exn automaton.states id in
        let this (acc, seen) = (f acc id state, seen) in
        let that (acc, seen) = Set.fold (FDK.conts d) ~init:(acc, seen) ~f:loop in
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
      if not (Tbl.mem t.states id) then
        let _ = t.nextState <- max t.nextState (id + 1) in
        let (_,d) as state = Lazy.force (Tbl.find_exn automaton.states id) in
        Tbl.add_exn t.states ~key:id ~data:state;
        Set.iter (FDK.conts d) ~f:add
    in
    add automaton.source;
    t.source <- automaton.source;
    t

  let lex_sort (t0 : t0) =
    let rec loop acc stateId =
      if List.mem acc stateId then acc else
      let init = stateId :: acc in
      let (_,d) = Lazy.force (Tbl.find_exn t0.states stateId) in
      Set.fold (FDK.conts d) ~init ~f:loop
    in
    loop [] t0.source

  let t_of_t0 ?(cheap_minimize=true) (t0 : t0) =
    if not cheap_minimize then t_of_t0' t0 else
    let t = create_t () in
    (* table that maps old ids to new ids *)
    let newId = Int.Table.create () ~size:100 in
    lex_sort t0
    |> List.iter ~f:(fun id ->
        let (e,d) = Lazy.force (Tbl.find_exn t0.states id) in
        (* SJS: even though we are traversing the graph in reverse-lexiographic order,
           a node may be visited prior to one of its sucessors because there may be cylces *)
        let d = FDK.map_conts d ~f:(Tbl.find_or_add newId ~default:(fun () -> mk_state_t t)) in
        (* check if new id was already assigned *)
        match Tbl.find newId id with
        | None ->
          let new_id = add_to_t t (e,d) in
          Tbl.add_exn newId ~key:id ~data:new_id
        | Some new_id ->
          add_to_t_with_id t (e,d) new_id
      );
    t.source <- Tbl.find_exn newId t0.source;
    t

  (* classic powerset construction, performed on symbolic automaton *)
  let determinize (automaton : t) : unit =
    (* table of type : int set -> int *)
    let tbl : int S.Table.t = S.Table.create () ~size:10 in
    (* table of type : int -> int set *)
    let untbl : S.t Int.Table.t = Int.Table.create () ~size:10 in
    let unmerge k = Int.Table.find untbl k |> Option.value ~default:(S.singleton k) in
    let merge ks =
      let () = assert (S.length ks > 1) in
      let ks = S.fold ks ~init:S.empty ~f:(fun acc k -> S.union acc (unmerge k)) in
      match S.Table.find tbl ks with
      | Some k -> k
      | None ->
        let (es, ds) =
          S.to_list ks
          |> List.map ~f:(Tbl.find_exn automaton.states)
          |> List.unzip in
        let fdk = (FDK.big_union es, FDK.big_union ds) in
        let k = add_to_t automaton fdk in
        S.Table.add_exn tbl ~key:ks ~data:k;
        (* k may not be fresh, since there could have been an FDK equvialent to fdk
           present in the automaton already; therefore, simply ignore warning *)
        ignore (Int.Table.add untbl ~key:k ~data:ks);
        k
    in
    let determinize_action par =
      par
      |> Action.Par.to_list
      |> List.sort ~cmp:Action.Seq.compare_mod_k
      |> List.group ~break:(fun s1 s2 -> not (Action.Seq.equal_mod_k s1 s2))
      |> List.map ~f:(function
        | [seq] -> seq
        | group ->
          let ks = List.map group ~f:(fun s -> Action.Seq.find_exn s K |> Value.to_int_exn)
                   |> S.of_list in
          let k = merge ks in
          List.hd_exn group |> Action.Seq.add ~key:K ~data:(Value.of_int k))
      |> Action.Par.of_list
    in
    let dedup_fdk = FDK.map_r determinize_action in
    map_reachable automaton ~order:`Pre ~f:(fun _ (e,d) -> (e, dedup_fdk d))

  let rec split_pol (automaton : t0) (pol: Pol.policy) : FDK.t * FDK.t * ((int * Pol.policy) list) =
    match pol with
    | Filter pred -> (FDK.of_pred pred, FDK.drop, [])
    | Mod hv -> (FDK.of_mod hv, FDK.drop, [])
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
      let e = FDK.drop in
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
    Tbl.add_exn automaton.states ~key:id ~data:(Lazy.from_fun f)

  let of_policy ?(dedup=true) ?ing ?(cheap_minimize=true) (pol : Frenetic_NetKAT.policy) : t =
    let automaton = create_t0 () in
    let pol = Pol.of_pol ing pol in
    let () = add_policy automaton (automaton.source, pol) in
    let automaton = t_of_t0 ~cheap_minimize automaton in
    let () = if dedup then determinize automaton in
    automaton

  let pc_unused pc fdd =
    FDK.fold
      (fun par -> Action.Par.for_all par ~f:(fun seq -> not (Action.(Seq.mem seq (F pc)))))
      (fun (f,_) l r -> l && r && f<>pc)
      fdd

  let to_local ~(pc : Field.t) (automaton : t) : FDK.t =
    fold_reachable automaton ~init:FDK.drop ~f:(fun acc id (e,d) ->
      let _ = assert (pc_unused pc e && pc_unused pc d) in
      let d =
        let open Action in
        FDK.map_r
          (Par.map ~f:(fun seq -> match Seq.find seq K with
            | None -> failwith "transition function must specify next state!"
            | Some data -> Seq.remove seq K |> Seq.add ~key:(F pc) ~data))
          d
      in
      let guard =
        if id = automaton.source then FDK.id
        else FDK.atom (pc, Value.of_int id) Action.one Action.zero in
      let fdk = FDK.seq guard (FDK.union e d) in
      FDK.union acc fdk)

  (* SJS: horrible hack *)
  let to_dot (automaton : t) =
    let states = Tbl.map automaton.states ~f:(fun (e,d) -> FDK.union e d) in
    let open Format in
    let buf = Buffer.create 200 in
    let fmt = formatter_of_buffer buf in
    let seen = FDK.Tbl.create () ~size:20 in
    pp_set_margin fmt (1 lsl 29);
    fprintf fmt "digraph fdk {@\n";
    let rec node_loop node =
      if not (FDK.Tbl.mem seen node) then begin
        FDK.Tbl.add_exn seen node ();
        match FDK.unget node with
        | Leaf par ->
          let node = (node : FDK.t :> int) in
          let seqId = ref 0 in
          let edges = ref [] in
          fprintf fmt "subgraph cluster_%d {@\n" node;
          fprintf fmt "\trank = sink;@\n" ;
          fprintf fmt "\tshape = box;@\n" ;
          fprintf fmt "\t%d [shape = point];@\n" node;
          Action.Par.iter par ~f:(fun seq ->
            let id : string = sprintf "\"%dS%d\"" node (!seqId) in
            let cont = Action.Seq.find seq K
              |> Option.map ~f:(fun v -> Tbl.find_exn states (Value.to_int_exn v)) in
            let label = Action.to_string (Action.Par.singleton seq) in
            fprintf fmt "\t%s [shape=box, label=\"%s\"];@\n" id label;
            Option.iter cont ~f:(fun k ->
              edges := sprintf "%s -> %d [style=bold, color=blue];@\n"
                id (k : FDK.t :> int) :: (!edges));
            incr seqId;
          );
          fprintf fmt "}@\n";
          List.iter (!edges) ~f:(fprintf fmt "%s")
        | Branch((f, v), a, b) ->
          let node = (node : FDK.t :> int) in
          fprintf fmt "%d [label=\"%s = %s\"];@\n" node (Field.to_string f) (Value.to_string v);
          fprintf fmt "%d -> %d;@\n" node (a : FDK.t :> int);
          fprintf fmt "%d -> %d [style=\"dashed\"];@\n" node (b : FDK.t :> int);
          node_loop a;
          node_loop b
      end
    in
    let fdks = ref [] in
    let rec fdk_loop fdkId =
      let fdk = Tbl.find_exn states fdkId in
      let conts = FDK.conts fdk in
      fdks := fdk :: (!fdks);
      node_loop fdk;
      Set.iter conts ~f:fdk_loop
    in
    fdk_loop automaton.source;
    fprintf fmt "%d [style=bold, color=red];@\n"
      (Tbl.find_exn states automaton.source : FDK.t :> int);
    fprintf fmt "{rank=source; ";
    List.iter (!fdks) ~f:(fun fdk -> fprintf fmt "%d " (fdk : FDK.t :> int));
    fprintf fmt ";}@\n";
    fprintf fmt "}@.";
    Buffer.contents buf
end

let compile_global ?(options=default_compiler_options) (pol : Frenetic_NetKAT.policy) : FDK.t =
  prepare_compilation ~options pol;
  NetKAT_Automaton.of_policy pol
  |> NetKAT_Automaton.to_local ~pc:Field.Vlan








(*==========================================================================*)
(* MULTITABLE                                                               *)
(*==========================================================================*)

(* Each list of fields represents the fields one flow table can match on *)
type flow_layout = Field.t list list [@@deriving sexp]

let layout_to_string (layout : flow_layout) : string =
  List.map layout ~f:(fun table ->
    let str_lst = List.map table ~f:Field.to_string in
    "[" ^ (String.concat str_lst ~sep:" ") ^ "]")
  |> String.concat

(* Each flow table row has a table location, and a meta value on that table *)
type tableId = int [@@deriving sexp]
type metaId = int [@@deriving sexp]
type flowId = tableId * metaId [@@deriving sexp]

(* Match subtrees of t with the table location they will be placed *)
type flow_subtrees = (t, flowId) Map.Poly.t

(* OpenFlow 1.3+ instruction types *)
type instruction =
  [ `Action of Frenetic_OpenFlow.group
  | `GotoTable of flowId ]
  [@@deriving sexp]

(* A flow table row, with multitable support. If goto has a Some value
 * then the 0x04 row instruction is GotoTable. *)
type multitable_flow = {
  pattern      : Frenetic_OpenFlow.Pattern.t;
  cookie       : int64;
  idle_timeout : Frenetic_OpenFlow.timeout;
  hard_timeout : Frenetic_OpenFlow.timeout;
  instruction  : instruction;
  flowId       : flowId;
} [@@deriving sexp]

(* C style x++ for mutable ints *)
let post (x : int ref) : int =
  x := !x + 1;
  (!x - 1)

(* Make Map of subtrees of t and their corresponding flow table locations *)
let flow_table_subtrees (layout : flow_layout) (t : t) : flow_subtrees =
  let rec subtrees_for_table (table : Field.t list) (t : t)
    (subtrees : t list) : t list =
    match FDK.unget t with
    | Leaf _ -> subtrees
    | Branch ((field, _), tru, fls) ->
      if (List.mem table field) then
        t :: subtrees
      else
        subtrees_for_table table tru subtrees
        |> subtrees_for_table table fls
  in
  let meta_id = ref 0 in
  List.map layout ~f:(fun table -> subtrees_for_table table t [])
  |> List.filter ~f:(fun subtrees -> subtrees <> [])
  |> List.foldi ~init:Map.Poly.empty ~f:(fun tbl_id accum subtrees ->
      List.fold_right subtrees ~init:accum ~f:(fun t accum ->
        Map.add accum ~key:t ~data:(tbl_id,(post meta_id))))

(* make a flow struct that includes the table and meta id of the flow *)
let mk_multitable_flow options (pattern : Frenetic_OpenFlow.Pattern.t)
  (instruction : instruction) (flowId : flowId) : multitable_flow option =
  (* TODO: Fill in dependencies, similar to mk_flows above *)
  if is_valid_pattern options pattern then
    Some { cookie = 0L;
           idle_timeout = Permanent;
           hard_timeout = Permanent;
           pattern; instruction; flowId }
  else
    None

(* Create flow table rows for one subtree *)
let subtree_to_table options (subtrees : flow_subtrees) (subtree : (t * flowId))
  (group_tbl : Frenetic_GroupTable0x04.t) : multitable_flow list =
  let rec dfs (tests : (Field.t * Value.t) list) (subtrees : flow_subtrees)
  (t : t) (flowId : flowId) : multitable_flow option list =
    match FDK.unget t with
    | Leaf actions ->
      let insts = [to_action (get_inport tests) actions tests ~group_tbl] in
      [mk_multitable_flow options (to_pattern tests) (`Action insts) flowId]
    | Branch ((Location, Pipe _), _, fls) ->
      failwith "1.3 compiler does not support pipes"
    | Branch (test, tru, fls) ->
     (match Map.find subtrees t with
      | Some goto_id ->
        [mk_multitable_flow options (to_pattern tests) (`GotoTable goto_id) flowId]
      | None -> List.append (dfs (test :: tests) subtrees tru flowId)
                            (dfs tests subtrees fls flowId))
  in
  let (t, flowId) = subtree in
  match FDK.unget t with
  | Branch (test, tru, fls) ->
    List.filter_opt (List.append (dfs [test] subtrees tru flowId)
                                 (dfs [] subtrees fls flowId))
  | Leaf _  -> assert false (* each entry in the subtree map is a branch *)

(* Collect the flow table rows for each subtree in one list. *)
let subtrees_to_multitable options (subtrees : flow_subtrees) : (multitable_flow list * Frenetic_GroupTable0x04.t) =
  let group_table = (Frenetic_GroupTable0x04.create ()) in
  Map.to_alist subtrees
  |> List.rev
  |> List.map ~f:(fun subtree -> subtree_to_table options subtrees subtree group_table)
  |> List.concat
  |> fun ls -> (ls, group_table)

(* Produce a list of flow table entries for a multitable setup *)
let to_multitable ?(options=default_compiler_options) (sw_id : switchId) (layout : flow_layout) t
  : (multitable_flow list * Frenetic_GroupTable0x04.t) =
  (* restrict to only instructions for this switch, get subtrees,
   * turn subtrees into list of multitable flow rows *)
  FDK.restrict [(Field.Switch, Value.Const sw_id)] t
  |> flow_table_subtrees layout
  |> subtrees_to_multitable options
