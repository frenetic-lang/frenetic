open Core
open Fdd
open Syntax

module Field = Fdd.Field
exception Non_local = Syntax.Non_local

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]

module Action = Fdd.Action
module Value = Fdd.Value
module Par = Action.Par
module Seq = Action.Seq

(*==========================================================================*)
(* LOCAL COMPILATION                                                        *)
(*==========================================================================*)

(* the shared intermediate representation of the local & global compiler *)
module FDD = struct

  include FDD

  let of_test env hv =
    atom (Pattern.of_hv ~env hv) Action.one Action.zero

  let of_mod env hv =
    let k, v = Pattern.of_hv ~env hv in
    (* ensure the field is mutable *)
    begin match hv with
      | Meta (id,_) ->
        let _,(_,mut) = Field.Env.lookup env id in
        if not mut then failwith "cannot modify immutable field"
      | _ -> ()
    end;
    const Action.(Par.singleton (Seq.singleton (F k) v))

  let rec of_pred env p =
    match p with
    | True      -> id
    | False     -> drop
    | Test(hv)  -> of_test env hv
    | And(p, q) -> prod (of_pred env p) (of_pred env q)
    | Or (p, q) -> sum (of_pred env p) (of_pred env q)
    | Neg(q)    -> map_r Action.negate (of_pred env q)

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
        ~f:(fun par ->
          Action.Par.fold par ~init:drop ~f:(fun acc seq ->
            let u' = restrict (Action.Seq.to_hvs seq) u in
            (sum (prod (const Action.Par.(singleton seq)) u') acc)))
        ~g:(fun v t f -> cond v t f)
        ~find_or_add:(fun t -> BinTbl.find_or_add seq_tbl (t,u))

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

  (** Erases (all matches on) meta field. No need to erase modifications. *)
  let erase env t meta_field init =
    match init with
    | Const v ->
      let constr = Pattern.of_hv ~env (Meta (meta_field, v)) in
      restrict [constr] t
    | Alias hv ->
      let alias = Field.of_hv ~env hv in
      let meta,_ = Field.Env.lookup env meta_field in
      fold t ~f:const ~g:(fun (field,v) tru fls ->
        if field = meta then
          cond (alias, v) tru fls
        else
          cond (field,v) tru fls)

  let rec of_local_pol_k env p k =
    let open Syntax in
    match p with
    | Filter   p  -> k (of_pred env p)
    | Mod      m  -> k (of_mod  env m)
    | Union (p, q) -> of_local_pol_k env p (fun p' ->
                        of_local_pol_k env q (fun q' ->
                          k (union p' q')))
    | Seq (p, q) -> of_local_pol_k env p (fun p' ->
                      if FDD.equal p' FDD.drop then
                        k FDD.drop
                      else
                        of_local_pol_k env q (fun q' ->
                          k (seq p' q')))
    | Star p -> of_local_pol_k env p (fun p' -> k (star p'))
    | Let { id=field; init; mut; body=p } ->
      let env = Field.Env.add env field init mut in
      of_local_pol_k env p (fun p' -> k (erase env p' field init))
    | Link _ | VLink _ | Dup -> raise Non_local

  let rec of_local_pol ?(env=Field.Env.empty) p = of_local_pol_k env p ident

  let to_local_pol t =
    fold t ~f:Action.to_policy ~g:(fun v t f ->
      let p = Pattern.to_pred v in
      match t, f with
      | Filter t, Filter f ->
        Optimize.(mk_filter (mk_or (mk_and p t)
                                               (mk_and (mk_not p) f)))
      | _       , _        ->
        Optimize.(mk_union (mk_seq (mk_filter p) t)
                                    (mk_seq (mk_filter (mk_not p)) f)))

  (**
    Prevent NetKAT from creating spurious duplicate packets.

    Problem:
    ========

    Consider the program p := (f=n); (id + f:=n). It is equivalent to the
    program p' := (f=n). So clearly p should never produce more than one output
    packet per input packet.

    Unfortunately, naive translation of p will yield the FDD

                                f=n
                              /     \
                        {f:=n, 1}    {}

    which translates to the open flow table

                      match | action
                     =======+==========
                      f=n   | f:=n + id
                      *     | drop

    which produces TWO output packets for packets satisfying f=n.

    The issue is that the two actions "f:=n" and "id" are equivalent for packets
    satisfying "f=n".

    A more subtle version of the same problem occurs for the program
      q := (g:=m) + id
    It is equivalent to the program
      q' := (g=m; 1) + (g!=m; (g:=m + id))
    Thus q should produce only a single output packet if the input packet
    satisfies "g=m". Unfortunately, naive compilation of q yields the table

                        match | action
                       =======+==========
                        *     | g:=m + id

    which always generates TWO output packets.

    Solution:
    =========

    For action sequents a, b we can define the weakest precondition for equality
    of a and b, denoted wpe(a,b), as follows:
    wpe(a,b) is the (unique) NetKAT predicate satisfying
      π ⊢ wpe(a,b) <=> [[a]](π) = [[b]](π)
    i.e. a packet π satisfies wpe(a,b) iff the actions a and b produce the same
    output on input π.
    Semantically, we can define wpe(a,b) as the set of packets on which a and b
    agree, i.e. { π | [[a]](π) = [[b]](π) }.

    Then to avoid spurious duplicate packets, we would replace
      (a + b)
    with
      if wpe(a,b) then a else (a + b).

    The idea generalized to actions (i.e., sets of action sequents) by applying
    the above construction repeatedly; note that this can lead to a tree of
    conditionals whose size is exponential in the number of sequents in the
    worst case. Luckily, packet duplication is uncommon in practise.

    Implementation:
    ===============

    The [dedup fdd] function replaces all actions occuring in [fdd] with their
    respective trees of conditionals, as described above. The details of the
    construction differ slightly as the algorithms works syntactically rather
    than semantically, and uses an overapproximation of wpe for simplicity.

  *)
  let dedup fdd =
    let module FS = Set.Make(Field) in
    FDD.map
      (fun par ->
        let mods = Action.Par.to_hvs par in
        let fields = List.map mods ~f:fst |> FS.of_list in
        (* only fields that DO NOT occur in all sequents can contribute to the
           wpe (weakeast precondition for equality) *)
        let harmful = Action.Par.fold par ~init:FS.empty ~f:(fun acc seq ->
          let seq_fields =
            Action.Seq.to_hvs seq |> List.map ~f:fst |> FS.of_list in
          FS.union acc (FS.diff fields seq_fields)) in
        let mods = List.filter mods ~f:(fun (f,_) -> FS.mem harmful f) in
        List.fold mods ~init:(const par) ~f:(fun fdd test ->
          cond test (map_r (Action.demod test) fdd) fdd))
      cond
      fdd
end



(** An internal module that implements an interpreter for a [FDD.t]. This
    interpreter uses [FDD.t] operations to find the [Action.t] that should
    apply to the packet. Once that's found, it converts the [Action.t] into a
    NetKAT policy and falls back to the [Semantics] module to process the
    actions and produce the final [PacketSet.t] *)
module Interp = struct
  open Semantics

  let eval_to_action (packet:packet) (t:FDD.t) =
    let hvs = HeadersValues.to_hvs packet.headers in
    let sw  = (Field.Switch, Value.of_int64 packet.switch) in
    let vs  = List.map hvs ~f:Pattern.of_hv in
    match FDD.(unget (restrict (sw :: vs) t)) with
    | Leaf r -> r
    | Branch _ -> assert false

  let eval (p:packet) (t:FDD.t) =
    Semantics.eval p Action.(to_policy (eval_to_action p t))

  let eval_pipes (p:packet) (t:FDD.t) =
    Semantics.eval_pipes p Action.(to_policy (eval_to_action p t))
end

include FDD

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
   | `Empty -> FDD.clear_cache ~preserve:Int.Set.empty
   | `Preserve fdd -> FDD.clear_cache ~preserve:(FDD.refs fdd));
  (match options.field_order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all
   | `Static flds -> Field.set_order flds)
end

let compile_local ?(options=default_compiler_options) pol =
  prepare_compilation ~options pol; of_local_pol pol

let is_valid_pattern options (pat : Frenetic.OpenFlow.Pattern.t) : bool =
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
let fill_in_dependencies all_tests (pat : Frenetic.OpenFlow.Pattern.t) =
  let open Frenetic.OpenFlow.Pattern in
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
  List.fold_right hvs ~f:Pattern.to_sdn  ~init:Frenetic.OpenFlow.Pattern.match_all

let mk_flows options true_tests all_tests action queries =
  let open Frenetic.OpenFlow.Pattern in
  let open Frenetic.OpenFlow in
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

let erase_meta_fields fdd =
  let erase_meta_mods = Action.(Par.map ~f:(Seq.filteri ~f:(fun ~key ~data ->
    match key with
    | Action.F VPort | Action.F VSwitch
    | Action.F Meta0 | Action.F Meta1 | Action.F Meta2
    | Action.F Meta3 | Action.F Meta4 -> false
    | _ -> true)))
  in
  FDD.fold fdd
    ~f:(fun act -> FDD.const (erase_meta_mods act))
    ~g:(fun v t f ->
      match v with
      | VSwitch, _ | VPort, _
      | Meta0, _ | Meta1, _ | Meta2, _ | Meta3, _ | Meta4, _  ->
        failwith "uninitialized meta field"
      | _, _ -> unchecked_cond v t f)

let mk_branch_or_leaf test t f =
  match t with
  | None -> Some f
  | Some t -> Some (FDD.unchecked_cond test t f)

let opt_to_table ?group_tbl options sw_id t =
  let t =
    t
    |> restrict [(Field.Switch, Value.Const sw_id)
                ;(Field.VFabric, Value.Const (Int64.of_int 1))]
    |> erase_meta_fields
  in
  let rec next_table_row true_tests all_tests mk_rest t =
    match FDD.unget t with
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

let rec naive_to_table ?group_tbl options sw_id (t : FDD.t) =
  let t = FDD.(restrict [(Field.Switch, Value.Const sw_id)] t)
          |> erase_meta_fields in
  let rec dfs true_tests all_tests t = match FDD.unget t with
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
      let actions = (fst h).Frenetic.OpenFlow.action in
      match (List.concat (List.concat actions)) with
      | [] -> remove_tail_drop t
      | _ -> h :: t in
  List.rev (remove_tail_drop (List.rev fl))

let to_table' ?(options=default_compiler_options) ?group_tbl swId t =
  let t = if options.dedup_flows then FDD.dedup t else t in
  let t = match options.optimize with
  | true -> opt_to_table ?group_tbl options swId t
  | false -> naive_to_table ?group_tbl options swId t in
  if options.remove_tail_drops then (remove_tail_drops t) else t

let to_table ?(options=default_compiler_options) ?group_tbl swId t =
  List.map ~f:fst (to_table' ~options ?group_tbl swId t)

let pipes t =
  FDD.fold t ~f:Action.pipes ~g:(fun _ -> Set.union)
  |> Set.to_list

let queries t =
  FDD.fold t
  ~f:(fun r ->
    Action.queries r
    |> List.map ~f:(fun q -> (q, Syntax.True))
    |> Set.Poly.of_list)
  ~g:(fun v t f ->
      let p = Pattern.to_pred v in
      let open Optimize in
      Set.Poly.(union
        (map t ~f:(fun (q, p') -> (q, mk_and p p')))
        (map f ~f:(fun (q, p') -> (q, mk_and (mk_not p) p')))))
  |> Set.to_list

let size =
  FDD.fold
    ~f:(fun r -> 1)
    ~g:(fun v t f -> 1 + t + f)

let compression_ratio t = (FDD.compressed_size t, FDD.uncompressed_size t)

let eval_to_action (packet:Semantics.packet) (t:FDD.t) =
  let open Semantics in
  let hvs = HeadersValues.to_hvs packet.headers in
  let sw  = (Field.Switch, Value.of_int64 packet.switch) in
  let vs  = List.map hvs ~f:Pattern.of_hv in
  let () = eprintf "In eval_to_action" in
  match FDD.(unget (restrict (sw :: vs) t)) with
  | Leaf r -> r
  | Branch _ -> assert false

let eval (p:Semantics.packet) (t:FDD.t) =
  Semantics.eval p Action.(to_policy (eval_to_action p t))

let eval_pipes (p:Semantics.packet) (t:FDD.t) =
  Semantics.eval_pipes p Action.(to_policy (eval_to_action p t))

let to_dotfile t filename =
  let t = erase_meta_fields t in
  Out_channel.with_file filename ~f:(fun chan ->
    Out_channel.output_string chan (FDD.to_dot t))

let restrict hv t = FDD.restrict [Pattern.of_hv hv] t

let field_order_from_string = function
  | "default" -> `Default
  | "heuristic" -> `Heuristic
  | field_order_string ->
   let ls = String.split_on_chars ~on:['<'] field_order_string
    |> List.map ~f:String.strip
    |> List.map ~f:Field.of_string in
   let compose f g x = f (g x) in
   let curr_order = Field.all in
   let removed = List.filter curr_order ~f:(compose not (List.mem ~equal:(=) ls)) in
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


(* internal policy representation that allows to inject fdds into policies *)
module Pol = struct

  type t =
    | Filter of pred
    | Mod of header_val
    | Union of t * t
    | Seq of t * t
    | Star of t
    | Dup (* we can handle all of NetKAT *)
    | FDD of FDD.t * FDD.t (* FDD injection. E and D matrix. *)

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

  let mk_fdd e d =
    if FDD.equal e FDD.drop && FDD.equal d FDD.drop then drop
    else FDD (e, d)

  let mk_big_union = List.fold ~init:drop ~f:mk_union
  let mk_big_seq = List.fold ~init:id ~f:mk_seq

  let match_loc sw pt =
    let t1 = Test (Switch sw) in
    let t2 = Test (Location (Physical pt)) in
    Optimize.mk_and t1 t2
  let match_vloc sw pt =
    let t1 = Test (VSwitch sw) in
    let t2 = Test (VPort pt) in
    Optimize.mk_and t1 t2

  let filter_loc sw pt = match_loc sw pt |> mk_filter
  let filter_vloc sw pt = match_vloc sw pt |> mk_filter

  let rec of_pol (ing : Syntax.pred option) (pol : Syntax.policy) : t =
    match pol with
    | Filter a -> Filter a
    | Mod hv -> Mod hv
    | Union (p,q) -> Union (of_pol ing p, of_pol ing q)
    | Seq (p,q) -> Seq (of_pol ing p, of_pol ing q)
    | Star p -> Star (of_pol ing p)
    | Link (s1,p1,s2,p2) ->
      let link = mk_seq (mk_mod (Switch s2)) (mk_mod (Location (Physical p2))) in
      let post_link = match ing with
        | None -> filter_loc s2 p2
        | Some ing ->
          Optimize.(mk_and (Test (Switch s2)) (mk_not ing))
          |> mk_filter in
      mk_big_seq [filter_loc s1 p1; Dup; link; Dup; post_link]
    | VLink (s1,p1,s2,p2) ->
      let link = mk_seq (mk_mod (VSwitch s2)) (mk_mod (VPort p2)) in
      let post_link = match ing with
        | None -> filter_vloc s2 p2
        | Some ing ->
          Optimize.(mk_and (Test (VSwitch s2)) (mk_not ing))
          |> mk_filter in
      mk_big_seq [filter_vloc s1 p1; Dup; link; Dup; post_link]
    | Let _ -> failwith "meta fields not supported by global compiler yet"
    | Dup -> Dup
end



(* Symbolic NetKAT Automata (intermediate representation of global compiler) *)
module Automaton = struct

  (* table *)
  module Tbl = Int.Table

  (* untable (inverse table) *)
  module Untbl = FDD.BinTbl

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
    { states : (FDD.t * FDD.t) Tbl.t;
      has_state : int Untbl.t;
      mutable source : int;
      mutable nextState : int }

  (* lazy intermediate presentation to avoid compiling uncreachable automata states *)
  type t0 =
    { states : (FDD.t * FDD.t) Lazy.t Tbl.t;
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

  let add_to_t (automaton : t) (state : (FDD.t * FDD.t)) : int =
    match Untbl.find automaton.has_state state with
    | Some k -> k
    | None ->
      let k = mk_state_t automaton in
      Tbl.add_exn automaton.states ~key:k ~data:state;
      Untbl.add_exn automaton.has_state ~key:state ~data:k;
      k

  let add_to_t_with_id (automaton : t) (state : (FDD.t * FDD.t)) (id : int) : unit = begin
      assert (not (Tbl.mem automaton.states id));
      Tbl.add_exn automaton.states ~key:id ~data:state;
      Untbl.set automaton.has_state ~key:state ~data:id;
    end

  let map_reachable ?(order = `Pre) (automaton : t) ~(f: int -> (FDD.t * FDD.t) -> (FDD.t * FDD.t)) : unit =
    let rec loop seen (id : int) =
      if S.mem seen id then seen else
        let seen = S.add seen id in
        let state = Tbl.find_exn automaton.states id in
        let this seen =
          let state = f id state in
          Tbl.set automaton.states ~key:id ~data:state; (seen, state) in
        let that (seen, (_,d)) = Set.fold (FDD.conts d) ~init:seen ~f:loop in
        match order with
        | `Pre -> seen |> this |> that
        | `Post -> (seen, state) |> that |> this |> fst
    in
    loop S.empty automaton.source |> ignore

  let fold_reachable ?(order = `Pre) (automaton : t) ~(init : 'a) ~(f: 'a -> int -> (FDD.t * FDD.t) -> 'a) =
    let rec loop (acc, seen) (id : int) =
      if S.mem seen id then (acc, seen) else
        let seen = S.add seen id in
        let (_,d) as state = Tbl.find_exn automaton.states id in
        let this (acc, seen) = (f acc id state, seen) in
        let that (acc, seen) = Set.fold (FDD.conts d) ~init:(acc, seen) ~f:loop in
        match order with
        | `Pre -> (acc, seen) |> this |> that
        | `Post -> (acc, seen) |> that |> this
    in
    loop (init, S.empty) automaton.source |> fst

  let iter_reachable ?(order = `Pre) (automaton : t) ~(f: int -> (FDD.t * FDD.t) -> unit) : unit =
    fold_reachable automaton ~order ~init:() ~f:(fun _ -> f)

  let t_of_t0' (automaton : t0) =
    let t = create_t () in
    let rec add id =
      if not (Tbl.mem t.states id) then
        let _ = t.nextState <- max t.nextState (id + 1) in
        let (_,d) as state = Lazy.force (Tbl.find_exn automaton.states id) in
        Tbl.add_exn t.states ~key:id ~data:state;
        Set.iter (FDD.conts d) ~f:add
    in
    add automaton.source;
    t.source <- automaton.source;
    t

  let lex_sort (t0 : t0) =
    let rec loop acc stateId =
      if List.mem ~equal:(=) acc stateId then acc else
      let init = stateId :: acc in
      let (_,d) = Lazy.force (Tbl.find_exn t0.states stateId) in
      Set.fold (FDD.conts d) ~init ~f:loop
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
        let d = FDD.map_conts d ~f:(Tbl.find_or_add newId ~default:(fun () -> mk_state_t t)) in
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
        let fdd = (FDD.big_union es, FDD.big_union ds) in
        let k = add_to_t automaton fdd in
        S.Table.add_exn tbl ~key:ks ~data:k;
        (* k may not be fresh, since there could have been an FDD equvialent to fdd
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
    let dedup_fdd = FDD.map_r ~f:determinize_action in
    map_reachable automaton ~order:`Pre ~f:(fun _ (e,d) -> (e, dedup_fdd d))

  (** symbolic antimirov derivatives *)
  let rec split_pol (automaton : t0) (pol: Pol.t) : FDD.t * FDD.t * ((int * Pol.t) list) =
    (* SJS: temporary hack *)
    let env = Field.Env.empty in
    match pol with
    | Filter pred -> (FDD.of_pred env pred, FDD.drop, [])
    | Mod hv -> (FDD.of_mod env hv, FDD.drop, [])
    | Union (p,q) ->
      let (e_p, d_p, k_p) = split_pol automaton p in
      let (e_q, d_q, k_q) = split_pol automaton q in
      let e = FDD.union e_p e_q in
      let d = FDD.union d_p d_q in
      let k = k_p @ k_q in
      (e, d, k)
    | Seq (p,q) ->
      (* TODO: short-circuit *)
      let (e_p, d_p, k_p) = split_pol automaton p in
      let (e_q, d_q, k_q) = split_pol automaton q in
      let e = FDD.seq e_p e_q in
      let d = FDD.union d_p (FDD.seq e_p d_q) in
      let q' = Pol.mk_fdd e_q d_q in
      let k = (List.map k_p ~f:(fun (id,p) -> (id, Pol.mk_seq p q'))) @ k_q in
      (e, d, k)
    | Star p ->
      let (e_p, d_p, k_p) = split_pol automaton p in
      let e = FDD.star e_p in
      let d = FDD.seq e d_p in
      let pol' = Pol.mk_fdd e d in
      let k = List.map k_p ~f:(fun (id,k) -> (id, Pol.mk_seq k pol')) in
      (e, d, k)
    | Dup ->
      let id = mk_state_t0 automaton in
      let e = FDD.drop in
      let d = FDD.mk_cont id in
      let k = [(id, Pol.id)] in
      (e, d, k)
    | FDD (e,d) -> (e,d,[])

  let rec add_policy (automaton : t0) (id, pol : int * Pol.t) : unit =
    let f () =
      let (e,d,k) = split_pol automaton pol in
      List.iter k ~f:(add_policy automaton);
      (e, d)
    in
    Tbl.add_exn automaton.states ~key:id ~data:(Lazy.from_fun f)

  let of_policy ?(dedup=true) ?ing ?(cheap_minimize=true) (pol : Syntax.policy) : t =
    let automaton = create_t0 () in
    let pol = Pol.of_pol ing pol in
    let () = add_policy automaton (automaton.source, pol) in
    let automaton = t_of_t0 ~cheap_minimize automaton in
    let () = if dedup then determinize automaton in
    automaton

  let pc_unused pc fdd =
    FDD.fold fdd
      ~f:Action.(Par.for_all ~f:(fun seq -> not (Seq.mem seq (F pc))))
      ~g:(fun (f,_) l r -> l && r && f<>pc)

  (** Assumes [automaton] is a bipartite automaton in which switch states and
      topology states alternate, with the start state being a switch state.
      Modifies automaton by skipping topology states and transitioning straight
      to the (unique) next switch states. *)
  let skip_topo_states (automaton : t) : unit =
    map_reachable automaton ~order:`Pre ~f:(fun _ (e,d) ->
      let d = FDD.map_conts d ~f:(fun k ->
        Tbl.find_exn automaton.states k
        |> snd
        |> FDD.conts
        |> Set.to_list
        |> (function
           | [k'] -> k'
           | _ -> failwith "topology state expected to have unique successor"))
      in (e,d))

  let to_local ~(pc : Field.t) (automaton : t) : FDD.t =
    skip_topo_states automaton;
    fold_reachable automaton ~init:FDD.drop ~f:(fun acc id (e,d) ->
      let _ = assert (pc_unused pc e && pc_unused pc d) in
      let d =
        FDD.map_r
          Action.(Par.map ~f:(fun seq -> match Seq.find seq K with
               | None -> failwith "transition function must specify next state!"
               | Some data -> Seq.remove seq K |> Seq.add ~key:(F pc) ~data))
          d
      in
      let guard =
        if id = automaton.source then FDD.id
        else FDD.atom (pc, Value.of_int id) Action.one Action.zero in
      let fdd = FDD.seq guard (FDD.union e d) in
      FDD.union acc fdd)

  let to_dot (automaton : t) =
    let open Format in
    let buf = Buffer.create 200 in
    let fmt = formatter_of_buffer buf in
    let states = Hashtbl.map automaton.states ~f:(fun (e,d) -> FDD.union e d) in
    let state_lbl fmt = fprintf fmt "state_%d" in
    let fdd_lbl fmt fdd = fprintf fmt "fdd_%d" (fdd : FDD.t :> int) in
    let fdd_leaf_lbl fmt (i,fdd) = fprintf fmt "seq_%d_%d" (fdd : FDD.t :> int) i in

    (* auxillary functions *)
    let rec do_states () =
      fprintf fmt "# put state nodes on top\n";
      fprintf fmt "{rank=source;";
      List.iter (Hashtbl.keys states) ~f:(fprintf fmt " %a" state_lbl);
      fprintf fmt ";}@\n";
      (* -- *)
      fprintf fmt "\n# mark start state\n";
      fprintf fmt "%a [style=bold, color=red, shape=octagon];@\n" state_lbl automaton.source;
      (* -- *)
      fprintf fmt "\n# connect state nodes to FDDs\n";
      Hashtbl.iteri states ~f:(fun ~key:id ~data:fdd ->
        fprintf fmt "%a -> %a;@\n" state_lbl id fdd_lbl fdd);
      (* -- *)
      fprintf fmt "\n# define FDDs\n";
      do_fdds Int.Set.empty (Hashtbl.data states)

    and do_fdds seen worklist =
      match worklist with
      | [] -> ()
      | fdd::worklist ->
        let uid = (fdd : FDD.t :> int) in
        if Set.mem seen uid then
          do_fdds seen worklist
        else
          do_node (Set.add seen uid) worklist fdd

    and do_node seen worklist fdd =
      match FDD.unget fdd with
      | Branch((f, v), a, b) ->
        fprintf fmt "%a [label=\"%s = %s\"];\n" fdd_lbl fdd (Field.to_string f) (Value.to_string v);
        fprintf fmt "%a -> %a;\n" fdd_lbl fdd fdd_lbl a;
        fprintf fmt "%a -> %a [style=\"dashed\"];\n" fdd_lbl fdd fdd_lbl b;
        do_fdds seen (a::b::worklist)
      | Leaf par ->
        fprintf fmt "subgraph cluster_%a {@\n" fdd_lbl fdd;
        fprintf fmt "\trank=sink;@\n";
        fprintf fmt "\t%a [shape = point];@\n" fdd_lbl fdd;
        let transitions = List.mapi (Action.Par.to_list par) ~f:(do_seq fdd) in
        fprintf fmt "}@\n";
        List.iter transitions ~f:(function
          | None -> ()
          | Some (s,t) ->
            fprintf fmt "%a -> %a [style=bold, color=blue];@\n" fdd_leaf_lbl s state_lbl t);
        do_fdds seen worklist

    and do_seq fdd i seq =
      let label = Action.Seq.to_string seq in
      fprintf fmt "\t%a [shape=box, label=\"%s\"];@\n" fdd_leaf_lbl (i,fdd) label;
      Option.map (Action.Seq.find seq K) ~f:(fun v ->
        ((i, fdd), Value.to_int_exn v))

    in begin
      fprintf fmt "digraph automaton {@\n";
      do_states ();
      fprintf fmt "}@.";
      Buffer.contents buf
    end

end
(* END: module Automaton *)


let compile_global ?(options=default_compiler_options) ?(pc=Field.Vlan) pol : FDD.t =
  prepare_compilation ~options pol;
  Automaton.of_policy pol
  |> Automaton.to_local ~pc




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
  [ `Action of Frenetic.OpenFlow.group
  | `GotoTable of flowId ]
  [@@deriving sexp]

(* A flow table row, with multitable support. If goto has a Some value
 * then the 0x04 row instruction is GotoTable. *)
type multitable_flow = {
  pattern      : Frenetic.OpenFlow.Pattern.t;
  cookie       : int64;
  idle_timeout : Frenetic.OpenFlow.timeout;
  hard_timeout : Frenetic.OpenFlow.timeout;
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
    match FDD.unget t with
    | Leaf _ ->
       t :: subtrees
    | Branch ((field, _), tru, fls) ->
      if (List.mem ~equal:(=) table field) then
        t :: subtrees
      else
        subtrees_for_table table tru subtrees
        |> subtrees_for_table table fls in
  let meta_id = ref 0 in
    List.map layout ~f:(fun table -> subtrees_for_table table t [])
    |> List.filter ~f:(fun subtrees -> subtrees <> [])
    |> List.foldi ~init:Map.Poly.empty ~f:(fun tbl_id accum subtrees ->
      List.fold_right subtrees ~init:accum ~f:(fun t accum ->
        Map.add accum ~key:t ~data:(tbl_id,(post meta_id))))

(* make a flow struct that includes the table and meta id of the flow *)
let mk_multitable_flow options (pattern : Frenetic.OpenFlow.Pattern.t)
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
  (group_tbl : Frenetic.GroupTable0x04.t) : multitable_flow list =
  let rec dfs (tests : (Field.t * Value.t) list) (subtrees : flow_subtrees)
  (t : t) (flowId : flowId) : multitable_flow option list =
    match FDD.unget t with
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
  match FDD.unget t with
  | Branch (test, tru, fls) ->
    List.filter_opt (List.append (dfs [test] subtrees tru flowId)
                                 (dfs [] subtrees fls flowId))
  | Leaf _  ->
     List.filter_opt (dfs [] subtrees t flowId)

(* Collect the flow table rows for each subtree in one list. *)
let subtrees_to_multitable options (subtrees : flow_subtrees) : (multitable_flow list * Frenetic.GroupTable0x04.t) =
  let group_table = (Frenetic.GroupTable0x04.create ()) in
  Map.to_alist subtrees
  |> List.rev
  |> List.map ~f:(fun subtree -> subtree_to_table options subtrees subtree group_table)
  |> List.concat
  |> fun ls -> (ls, group_table)

(* Produce a list of flow table entries for a multitable setup *)
let to_multitable ?(options=default_compiler_options) (sw_id : switchId) (layout : flow_layout) t
  : (multitable_flow list * Frenetic.GroupTable0x04.t) =
  (* restrict to only instructions for this switch, get subtrees,
   * turn subtrees into list of multitable flow rows *)
  FDD.restrict [(Field.Switch, Value.Const sw_id)] t
  |> flow_table_subtrees layout
  |> subtrees_to_multitable options
