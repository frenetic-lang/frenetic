open Core.Std
open NetKAT_FDD
open NetKAT_Types

module Field = NetKAT_FDD.Field
exception Non_local = NetKAT_FDD.Non_local

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]


module Pol = struct
  
  type policy =
    | Filter of pred
    | Mod of header_val
    | Union of policy * policy
    | Seq of policy * policy
    | Star of policy
    | Link of switchId * portId * switchId * portId
    | FDK of FDK.t * FDK.t

  let drop = Filter False
  let id = Filter True

  let rec of_pol (pol : NetKAT_Types.policy) : policy =
    match pol with
    | NetKAT_Types.Filter a -> Filter a
    | NetKAT_Types.Mod hv -> Mod hv
    | NetKAT_Types.Union (p,q) -> Union (of_pol p, of_pol q)
    | NetKAT_Types.Seq (p,q) -> Seq (of_pol p, of_pol q)
    | NetKAT_Types.Star p -> Star (of_pol p)
    | NetKAT_Types.Link (s1,p1,s2,p2) -> Link (s1,p1,s2,p2)

  let match_location sw pt =
    let t1 = Test (Switch sw) in
    let t2 = Test (Location (Physical pt)) in
    Optimize.mk_and t1 t2

  let mk_fdk e d =
    let drop' = FDK.mk_drop () in
    if FDK.equal e drop' && FDK.equal d drop' then drop
    else FDK (e, d)

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

end


module Repr = struct

  module T = Hashtbl.Make(struct
    type t = int with sexp
    let hash n = n
    let compare = Pervasives.compare
  end)

  type t = 
    { trees : (FDK.t * FDK.t) Lazy.t T.t; 
      mutable next_id : int }

  let create () =
    let trees = T.create () ~size:10 in
    let root = 0 in
    ({ trees; next_id = root}, root)

  let mk_id forest =
    let id = forest.next_id in
    begin
      forest.next_id <- id + 1;
      id
    end

  let of_test hv =
    FDK.atom (Pattern.of_hv hv) ActionK.one ActionK.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    FDK.atom (k, v) ActionK.(one) ActionK.(Par.singleton (Seq.singleton (F k) v))

  let rec of_pred p =
    match p with
    | True      -> FDK.mk_id ()
    | False     -> FDK.mk_drop ()
    | Test(hv)  -> of_test hv
    | And(p, q) -> FDK.prod (of_pred p) (of_pred q)
    | Or (p, q) -> FDK.sum (of_pred p) (of_pred q)
    | Neg(q)    -> FDK.map_r ActionK.negate (of_pred q)

  let restrict hv t =
    FDK.restrict [Pattern.of_hv hv] t

  let cond v t f =
    if FDK.equal t f then
      t
    else
      FDK.(sum (prod (atom v ActionK.one ActionK.zero) t)
             (prod (atom v ActionK.zero ActionK.one) f))

  let dp_fold (g : ActionK.t -> FDK.t)
              (h : Field.t * Value.t -> FDK.t -> FDK.t -> FDK.t)
              (t : FDK.t) : FDK.t =
     let tbl = Hashtbl.Poly.create () in
     let rec f t =
       Hashtbl.Poly.find_or_add tbl t ~default:(fun () -> f' t)
     and f' t = match FDK.unget t with
      | FDK.Leaf r -> g r
      | FDK.Branch ((v, l), tru, fls) -> h (v,l) (f tru) (f fls) in
      f t

  let seq t u =
    match FDK.peek u with
    | Some _ -> FDK.prod t u (* This is an optimization. If [u] is an
                              [ActionK.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
    | None   ->
      dp_fold
        (fun par ->
          ActionK.Par.fold par ~init:(FDK.mk_drop ()) ~f:(fun acc seq ->
            let mods = ActionK.Seq.(to_alist seq) |> List.filter_map ~f:(fun (f,n) ->
              match f with 
              | ActionK.F f -> Some (f,n) 
              | ActionK.K -> None)
            in
            let u' = FDK.restrict mods u in
            FDK.(sum (prod (const ActionK.Par.(singleton seq)) u') acc)))
        (fun v t f -> cond v t f)
      t

  let union t u =
    (* Compute the union of [t] and [u] by using the sum operation. This will
       appropriately combine actions for overlapping patterns. *)
    if FDK.equal t u then
      t
    else
      FDK.sum t u

  let star' lhs t =
    let rec loop acc power =
      let power' = seq power t in
      let acc' = union acc power' in
      if FDK.equal acc acc'
        then acc
        else loop acc' power'
    in
    loop (FDK.mk_id ()) lhs

  let star = star' (FDK.mk_id ())

  let rec split_pol (forest : t) (pol: Pol.policy) : FDK.t * FDK.t * ((int * Pol.policy) list) =
    match pol with
    | Filter pred -> (of_pred pred, FDK.mk_drop (), [])
    | Mod hv -> (of_mod hv, FDK.mk_drop (), [])
    | Union (p,q) ->
      let (e_p, d_p, k_p) = split_pol forest p in
      let (e_q, d_q, k_q) = split_pol forest q in
      let e = union e_p e_q in
      let d = union d_p d_q in
      let k = k_p @ k_q in
      (e, d, k)
    | Seq (p,q) ->
      let (e_p, d_p, k_p) = split_pol forest p in
      let (e_q, d_q, k_q) = split_pol forest q in
      let e = seq e_p e_q in
      let d = union d_p (seq e_p d_q) in
      let q' = Pol.mk_fdk e_q d_q in
      let k = (List.map k_p ~f:(fun (id,p) -> (id, Pol.mk_seq p q'))) @ k_q in
      (e, d, k)
    | Star p ->
      let (e_p, d_p, k_p) = split_pol forest p in
      let e = star e_p in
      let d = seq e d_p in
      let pol' = Pol.mk_fdk e d in
      let k = List.map k_p ~f:(fun (id,k) -> (id, Pol.mk_seq k pol')) in
      (e, d, k)
    | Link (sw1,pt1,sw2,pt2) ->
      let id = mk_id forest in
      let e = FDK.mk_drop () in
      let d = seq (of_pred (Pol.match_location sw1 pt1)) (FDK.cont id) in
      let k = [(id, Pol.Filter (Pol.match_location sw2 pt2))] in
      (e, d, k)
    | FDK (e,d) -> (e,d,[])
  
  let rec add_policy (forest : t) (id, pol : int * Pol.policy) : unit =
    let f () =
      let (e,d,k) = split_pol forest pol in
      List.iter k ~f:(add_policy forest);
      (e, d)
    in
    match T.add forest.trees ~key:id ~data:(Lazy.from_fun f) with
    | `Duplicate -> assert false
    | `Ok -> ()

  let of_policy (pol : NetKAT_Types.policy) : (t * int) =
    let forest, root = create () in
    let pol = Pol.of_pol pol in
    add_policy forest (root, pol);
    (forest, root)

(*   let of_policy (tdks : t) (pol : policy) =
    ma
  and split (tdks : t) (pol : policy) *)

(*   let split (pol : policy) : FDK.t * FDK.t * rich_policy =
    match pol with
    match pol with
  | Filter _ -> (of_test, drop, drop)
  | Mod _ -> (pol, drop, drop)
  | Union (p,q) ->
    let (e_p, d_p, k_p) = split_pol p in
    let (e_q, d_q, k_q) = split_pol q in
    (mk_union e_p e_q, mk_union d_p d_q, mk_union k_p k_q)
  | Seq (p,q) ->
    let (e_p, d_p, k_p) = split_pol p in
    let (e_q, d_q, k_q) = split_pol q in
    let e = mk_seq e_p e_q in
    (* SJS: loss of precision!!! Sound but not optimal *)
    let e_p_ind = if e_p = drop then drop else id in
    let d = mk_union d_p (mk_seq e_p d_q) in
    let k = mk_union (mk_seq e_p_ind k_q) (mk_seq k_p q) in
    (* inline fdds into policies to avoid duplication *)
    (e, d, k)
  | Star p ->
    let (e_p, d_p, k_p) = split_pol p in
    let e = mk_star e_p in
    let d = mk_seq e d_p in
    let k = mk_seq k_p (mk_union e d) in
    (e, d, k)
  | Link (sw1,pt1,sw2,pt2) -> (drop, match_location sw1 pt1, match_location sw2 pt2)

  let of_policy p =
    match p with *)

(*   let seq t u =
    match FDK.peek u with
    | Some _ -> FDK.prod t u (* This is an optimization. If [u] is an
                              [ActionK.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
    | None   ->
      dp_fold
        (fun par ->
          ActionK.Par.fold par ~init:(FDK.mk_drop ()) ~f:(fun acc seq ->
            let mods = ActionK.Seq.(to_alist seq) |> filter_map ~f:(fun (f,n) ->
              match f with 
              | F f -> Some (f,n) 
              | K -> None)
            let u' = FDK.restrict mods u in
            FDK.(sum (prod (const ActionK.Par.(singleton seq)) u') acc)))
        (fun v t f -> cond v t f)
      t

  let union t u =
    (* Compute the union of [t] and [u] by using the sum operation. This will
       appropriately combine actions for overlapping patterns. *)
    if FDK.equal t u then
      t
    else
      FDK.sum t u

  let star' lhs t =
    Compute [star t] by iterating to a fixed point.

       NOTE that the equality check is not semantic equivalence, so this may not
       terminate when expected. In practice though, it should.
    let rec loop acc power =
      let power' = seq power t in
      let acc' = union acc power' in
      if FDK.equal acc acc'
        then acc
        else loop acc' power'
    in
    loop (FDK.mk_id ()) lhs

  let star = star' (FDK.mk_id ())



  let rec of_pred p =
    let open NetKAT_Types in
    match p with
    | True      -> FDK.mk_id ()
    | False     -> FDK.mk_drop ()
    | Test(hv)  -> of_test hv
    | And(p, q) -> FDK.prod (of_pred p) (of_pred q)
    | Or (p, q) -> FDK.sum (of_pred p) (of_pred q)
    | Neg(q)    -> FDK.map_r ActionK.negate (of_pred q)

  let rec of_policy_k p k =
    let open NetKAT_Types in
    match p with
    | Filter   p  -> k (of_pred p)
    | Mod      m  -> k (of_mod  m)
    | Union (p, q) -> of_policy_k p (fun p' ->
                        of_policy_k q (fun q' ->
                          k (union p' q')))
    | Seq (p, q) -> of_policy_k p (fun p' ->
                      if FDK.equal p' (FDK.mk_drop ()) then
                        k (FDK.mk_drop ())
                      else
                        of_policy_k q (fun q' ->
                          k (seq p' q')))
    | Star p -> of_policy_k p (fun p' -> k (star p'))
    | Link (sw1, pt1, sw2, pt2) -> raise Non_local

  let rec of_policy p = of_policy_k p ident *)

(*   let to_policy =
    FDK.fold
      (fun r -> ActionK.to_policy r)
      (fun v t f ->
        let p = Pattern.to_pred v in
        let open NetKAT_Types in
        match t, f with
        | Filter t, Filter f ->
          Optimize.(mk_filter (mk_or (mk_and p t)
                                     (mk_and (mk_not p) f)))
        | _       , _        ->
          Optimize.(mk_union (mk_seq (mk_filter p) t)
                             (mk_seq (mk_filter (mk_not p)) f))) *)

(*   let equal =
    FDK.equal

  let to_string =
    FDK.to_string *)
end

(* 
type cache
  = [ `Keep
    | `Empty
    | `Preserve of Repr.t ]

let clear_cache () = FDK.clear_cache Int.Set.empty

let compile ?(order=`Heuristic) ?(cache=`Empty) pol =
  (match cache with
   | `Keep -> ()
   | `Empty -> FDK.clear_cache Int.Set.empty
   | `Preserve fdd -> FDK.clear_cache (FDK.refs fdd));
  (match order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all_fields
   | `Static flds -> Field.set_order flds);
  Repr.of_policy pol *)


