open Core.Std
open NetKAT_FDD

module Field = NetKAT_FDD.Field
exception Non_local = NetKAT_FDD.Non_local

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]

module Repr = struct

  type t = T.t

  let of_test hv =
    T.atom (Pattern.of_hv hv) Action.one Action.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    T.const Action.(Par.singleton (Seq.singleton k v))

  let restrict hv t =
    T.restrict [Pattern.of_hv hv] t

  let cond v t f =
    if T.equal t f then
      t
    else
      T.(sum (prod (atom v Action.one Action.zero) t)
             (prod (atom v Action.zero Action.one) f))

  let dp_fold (g : Action.t -> T.t)
              (h : Field.t * Value.t -> T.t -> T.t -> T.t)
              (t : T.t) : T.t =
     let tbl = Hashtbl.Poly.create () in
     let rec f t =
       Hashtbl.Poly.find_or_add tbl t ~default:(fun () -> f' t)
     and f' t = match T.unget t with
      | T.Leaf r -> g r
      | T.Branch ((v, l), tru, fls) -> h (v,l) (f tru) (f fls) in
      f t

  let seq t u =
    (* Compute the sequential composition of [t] and [u] as a fold over [t]. In
       the case of a leaf node, each sequence [seq] of modifications is used to
       [restrict] the diagram for [u] and produce a new diagram [u'] that
       assumes (but does not explicitly represent) the state of the packet after
       passing through [t]'s modifications. [seq] and [u'] are then mulitplied as
       decision diagrams, which will distribute [seq] to all the leaf nodes of
       [u'] to produce the result. All such [seq]s in the [par] are then summed
       together.

       In the case of a branch node, the true and false branches are combined so
       that packets satisfying [v] are handled by the true branch, and packets
       not satisfying [v] are handled by the false branch. *)
    match T.peek u with
    | Some _ -> T.prod t u (* This is an optimization. If [u] is an
                              [Action.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
    | None   ->
      dp_fold
        (fun par ->
          Action.Par.fold par ~init:(T.mk_drop ()) ~f:(fun acc seq ->
            let u' = T.restrict Action.Seq.(to_alist seq) u in
            T.(sum (prod (const Action.Par.(singleton seq)) u') acc)))
        (fun v t f -> cond v t f)
      t

  let union t u =
    (* Compute the union of [t] and [u] by using the sum operation. This will
       appropriately combine actions for overlapping patterns. *)
    if T.equal t u then
      t
    else
      T.sum t u

  let star' lhs t =
    (* Compute [star t] by iterating to a fixed point.

       NOTE that the equality check is not semantic equivalence, so this may not
       terminate when expected. In practice though, it should. *)
    let rec loop acc power =
      let power' = seq power t in
      let acc' = union acc power' in
      if T.equal acc acc'
        then acc
        else loop acc' power'
    in
    loop (T.mk_id ()) lhs

  let star = star' (T.mk_id ())

  let rec of_pred p =
    let open NetKAT_Types in
    match p with
    | True      -> T.mk_id ()
    | False     -> T.mk_drop ()
    | Test(hv)  -> of_test hv
    | And(p, q) -> T.prod (of_pred p) (of_pred q)
    | Or (p, q) -> T.sum (of_pred p) (of_pred q)
    | Neg(q)    -> T.map_r Action.negate (of_pred q)

  let rec of_policy_k p k =
    let open NetKAT_Types in
    match p with
    | Filter   p  -> k (of_pred p)
    | Mod      m  -> k (of_mod  m)
    | Union (p, q) -> of_policy_k p (fun p' ->
                        of_policy_k q (fun q' ->
                          k (union p' q')))
    | Seq (p, q) -> of_policy_k p (fun p' ->
                      if T.equal p' (T.mk_drop ()) then
                        k (T.mk_drop ())
                      else
                        of_policy_k q (fun q' ->
                          k (seq p' q')))
    | Star p -> of_policy_k p (fun p' -> k (star p'))
    | Link (sw1, pt1, sw2, pt2) -> raise Non_local

  let rec of_policy p = of_policy_k p ident

  let to_policy =
    T.fold
      (fun r -> Action.to_policy r)
      (fun v t f ->
        let p = Pattern.to_pred v in
        let open NetKAT_Types in
        match t, f with
        | Filter t, Filter f ->
          Optimize.(mk_filter (mk_or (mk_and p t)
                                     (mk_and (mk_not p) f)))
        | _       , _        ->
          Optimize.(mk_union (mk_seq (mk_filter p) t)
                             (mk_seq (mk_filter (mk_not p)) f)))

  let equal =
    T.equal

  let to_string =
    T.to_string
end

(** An internal module that implements an interpreter for a [Repr.t]. This
    interpreter uses [Repr.t] operations to find the [Action.t] that should
    apply to the packet. Once that's found, it converts the [Action.t] into a
    NetKAT policy and falls back to the [NetKAT_Semantics] module to process the
    actions and produce the final [PacketSet.t] *)
module Interp = struct
  open NetKAT_Semantics

  let eval_to_action (packet:packet) (t:Repr.t) =
    let hvs = HeadersValues.to_hvs packet.headers in
    let sw  = (Field.Switch, Value.of_int64 packet.switch) in
    let vs  = List.map hvs ~f:Pattern.of_hv in
    match T.(peek (restrict (sw :: vs) t)) with
    | None    -> assert false
    | Some(r) -> r

  let eval (p:packet) (t:Repr.t) =
    NetKAT_Semantics.eval p Action.(to_policy (eval_to_action p t))

  let eval_pipes (p:packet) (t:Repr.t) =
    NetKAT_Semantics.eval_pipes p Action.(to_policy (eval_to_action p t))
end

include Repr

type cache
  = [ `Keep
    | `Empty
    | `Preserve of t ]

let clear_cache () = T.clear_cache Int.Set.empty

let compile ?(order=`Heuristic) ?(cache=`Empty) pol =
  (match cache with
   | `Keep -> ()
   | `Empty -> T.clear_cache Int.Set.empty
   | `Preserve fdd -> T.clear_cache (T.refs fdd));
  (match order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all_fields
   | `Static flds -> Field.set_order flds);
  of_policy pol

let check_vlan_pcp pattern =
  let open SDN.Pattern in
  if (pattern.dlVlanPcp <> None) && (pattern.dlTyp = None)
  then { pattern with dlTyp = Some 0x8100 }
  else pattern

let check_nwProto pattern =
  let open SDN.Pattern in
  if (pattern.nwProto <> None) && (pattern.dlTyp = None)
  then { pattern with dlTyp = Some 0x0800 }
  else pattern

let check_tcp pattern =
  let open SDN.Pattern in
  if pattern.tpSrc <> None && pattern.tpDst <> None && pattern.nwProto = None
  (* This is okay for TCP. Do we need to worry about UDP? *)
  then { pattern with nwProto = Some 0x6 }
  else pattern

let nw_src_dst_implies (pat : SDN.Pattern.t) =
  if pat.nwSrc = None && pat.nwDst = None then
    pat
  else
    { pat with dlTyp = Some 0x0800 }

let mk_flow pattern action queries =
  let open SDN.Pattern in
  let pattern = nw_src_dst_implies pattern in
  let pattern' = check_nwProto pattern in
  let pattern'' = check_tcp pattern' in
  let pattern''' = check_nwProto pattern'' in
  (* Not entirely sure how to detect the following from the pattern:
      - Left out optional ARP packet where dlTyp should be set to 0x0806
      - Left out UDP where nwProto should be set to 7
      - Left out ICMP where nwProto should be set to 1
   *)
  let pattern = pattern''' in
  let open SDN in
  ({ pattern
    ; action
    ; cookie = 0L
    ; idle_timeout = Permanent
    ; hard_timeout = Permanent
    }, queries)

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
  List.fold_right hvs ~f:Pattern.to_sdn  ~init:SDN.Pattern.match_all

let mk_branch_or_leaf test t f =
  match t with
  | None -> Some f
  | Some t -> Some (T.mk_branch test t f)

let opt_to_table sw_id t =
  let t =
    T.(restrict [(Field.Switch, Value.Const sw_id)] t)
  in
  let rec next_table_row tests mk_rest t =
    match T.unget t with
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
  loop t []

let rec naive_to_table sw_id (t : T.t) =
  let t = T.(restrict [(Field.Switch, Value.Const sw_id)] t) in
  let rec dfs tests t = match T.unget t with
  | Leaf actions ->
    let openflow_instruction = [to_action (get_inport tests) actions tests] in
    let queries = Action.get_queries actions in
    [mk_flow (to_pattern tests) openflow_instruction queries]
  | Branch ((Location, Pipe _), _, fls) -> dfs tests fls
  | Branch (test, tru, fls) ->
    dfs (test :: tests) tru @ dfs tests fls in
  dfs [] t

let to_table' ?(opt = true) = match opt with
 | true -> opt_to_table
 | false -> naive_to_table

let to_table ?(opt = true) swId t = List.map ~f:fst (to_table' ~opt swId t)

let pipes t =
  let module S = Set.Make(String) in
  let ps = T.fold
    (fun r -> Action.pipes r)
    (fun _ t f -> S.union t f)
    t
  in
  S.to_list ps

let queries t =
  let module S = Set.Make(struct
    type t = string * NetKAT_Types.pred sexp_opaque with sexp
    let compare = Pervasives.compare
  end) in
  let qs = T.fold
    (fun r ->
      let qs = Action.queries r in
      S.of_list (List.map qs ~f:(fun q -> (q, NetKAT_Types.True))))
    (fun v t f ->
      let p = Pattern.to_pred v in
      let open Optimize in
      S.(union (map t ~f:(fun (q, p') -> (q, mk_and p p')))
               (map t ~f:(fun (q, p') -> (q, mk_and (mk_not p) p')))))
    t
  in
  S.to_list qs

let size =
  T.fold
    (fun r -> 1)
    (fun v t f -> 1 + t + f)

let compression_ratio t = (T.compressed_size t, T.uncompressed_size t)

let eval =
  Interp.eval

let eval_pipes =
  Interp.eval_pipes

let to_dotfile t filename =
  Out_channel.with_file filename ~f:(fun chan ->
    Out_channel.output_string chan (T.to_dot t))
