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
(*     let g' = Memo.general g in
    let h' = Memo.general (fun (x,y,z) -> h x y z) in
 *)
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
          Action.Par.fold par ~init:(T.const Action.zero) ~f:(fun acc seq ->
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
    loop (T.const Action.one) lhs

  let star = star' (T.const Action.one)

  let rec of_pred p =
    let open NetKAT_Types in
    match p with
    | True      -> T.const Action.one
    | False     -> T.const Action.zero
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

let compile ?(order=`Heuristic) pol =
  (match order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all_fields
   | `Static flds -> Field.set_order flds);
  of_policy pol

let clear_cache () = T.clear_cache ()

let to_table sw_id t =
  (* Convert a [t] to a flowtable for switch [sw_id]. This is implemented as a
     fold over the [t]. Leaf nodes emit a single rule flowtable that mach all
     packets and perform the action represented by the [Action.t] at the leaf.

     Branch nodes convert the variable [v] to a function [guard] that modifies a
     pattern to check for the condition represented by that [v]. The [guard]
     function is then mapped across the flowtable generated from the true branch
     of the node. That result, together with the flowtable for the false branch
     are appended to produce the flowtable at that node.

     No additional guarding is necessary to ensure that no unintended packets
     hit the flowtable for the false branch. All flowtables generated by this
     algorithm will forward or drop all packets; they will not allow any packets
     to fall through. This is true in the base case of a leaf node. Inductively,
     this is also holds for the true and false tables of a branch node. Guarding
     the true table with the pattern represented by [v] means that it will match
     all packets that satisfy [v]. The false branch will therefore only apply to
     packets that don't satisfy [v]. Appending the guarded true table and the
     unguarded false tables will produce a table that will match all packets. *)
  let mk_flow pattern action =
    let open SDN in
    { pattern
    ; action
    ; cookie = 0L
    ; idle_timeout = Permanent
    ; hard_timeout = Permanent
    }
  in
  let t = T.(restrict [(Field.Switch, Value.Const sw_id)] t) in
  let tbl = T.to_table t in
  let to_pattern hvs = List.fold_right hvs ~f:Pattern.to_sdn  ~init:SDN.Pattern.match_all in
  let get_inport' current hv =
  match hv with
    | (Field.Location, Value.Const p) -> Some p
    | _ -> current
  in
  let get_inport hvs = List.fold_left hvs ~init:None ~f:get_inport' in
  let to_action in_port r = Action.to_sdn ?in_port r in
  List.map tbl ~f:(fun (hvs, r) -> mk_flow (to_pattern hvs) [to_action (get_inport hvs) r])

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
