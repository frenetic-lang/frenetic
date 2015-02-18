open Core.Std
open NetKAT_FDD
open NetKAT_Types


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
      rootId : int;
      mutable nextId : int }

  let create () =
    let trees = T.create () ~size:10 in
    let rootId = 0 in
    { trees; rootId; nextId = rootId+1 }

  let mk_id forest =
    let id = forest.nextId in
    begin
      forest.nextId <- id + 1;
      id
    end

  let int_of_val v =
    match v with
    | Value.Const k -> Int64.to_int_exn k
    | _ -> assert false

  let of_test hv =
    FDK.atom (Pattern.of_hv hv) ActionK.one ActionK.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    FDK.const ActionK.(Par.singleton (Seq.singleton (F k) v))
    (* FDK.atom (k, v) ActionK.(one) ActionK.(Par.singleton (Seq.singleton (F k) v)) *)

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

  let dp_fold (g : ActionK.t -> 'a)
              (h : Field.t * Value.t -> 'a -> 'a -> 'a)
              (t : FDK.t) : 'a =
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

  let conts_of_fdk fdk =
    dp_fold
      (fun par ->
        ActionK.Par.fold par ~init:[] ~f:(fun acc seq ->
          ActionK.(Seq.find seq K) :: acc)
        |> List.filter_opt)
      (fun _ t f -> t @ f)
      fdk
    |> List.map ~f:int_of_val
    |> List.dedup

  let force (forest : t) =
    let rec loop worklist =
      match worklist with
      | [] -> ()
      | id :: worklist ->
        let fdk = T.find_exn forest.trees id in
        if Lazy.is_val fdk then loop worklist
        else
          let (_,d) = Lazy.force fdk in
          loop (conts_of_fdk d @ worklist)
    in
    loop [forest.rootId];
    forest

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
      (* TODO: short-circuit *)
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
    T.add_exn forest.trees ~key:id ~data:(Lazy.from_fun f)

  let of_policy (pol : NetKAT_Types.policy) : t =
    let forest = create () in
    let pol = Pol.of_pol pol in
    add_policy forest (forest.rootId, pol);
    force forest

  let pc_unused pc fdd =
    dp_fold
      (fun par -> ActionK.Par.for_all par ~f:(fun seq -> not (ActionK.(Seq.mem seq (F pc)))))
      (fun (f,_) l r -> l && r && f<>pc)
      fdd

  let to_local (pc : Field.t) (forest : t) : NetKAT_LocalCompiler.t =
    (* let next_pc = ref (-1) in
    let next_pc () = next_pc := !next_pc + 1; !next_pc in
    let pc_tbl : int T.t = T.create () ~size:10 in *)
    let get_pc' (id : int) =
      (* T.find_exn pc_tbl *) id |> Value.of_int
    in
    let mk_pc (v : Value.t) =
      (* let v = T.find_or_add pc_tbl (int_of_val v) ~default:next_pc |> Value.of_int in *)
      (pc, v)
    in
    let fdk_to_fdd id e d =
      let guard =
        if id = forest.rootId then FDK.mk_id () else
        FDK.atom (pc, get_pc' id) ActionK.one ActionK.zero
      in
      let fdk = seq guard (union e d) in
      dp_fold
        (fun par -> ActionK.to_action mk_pc par |> NetKAT_FDD.T.mk_leaf)
        (* SJS: need to ensure variable order of fdk and fdd agree!! *)
        (fun v t f -> NetKAT_FDD.T.mk_branch v t f)
        fdk
    in
    let union = NetKAT_LocalCompiler.union in
    let rec main ks fdd =
      match ks with
      | [] -> fdd
      | k::ks ->
        begin match T.find_and_remove forest.trees k |> Option.map ~f:Lazy.force with
        | None -> main ks fdd
        | Some (e, d) ->
          let _ = assert (pc_unused pc e && pc_unused pc d) in
          let ks = conts_of_fdk d @ ks in
          let kfdd = fdk_to_fdd k e d in
          let file = Printf.sprintf "fdd-%d.dot" k in
          let fdd = union fdd kfdd in
          Out_channel.write_all file ~data:(NetKAT_FDD.T.to_dot kfdd);
          main ks fdd
        end
    in
    main [forest.rootId] (NetKAT_FDD.T.mk_drop ())

end

include Repr

(*


type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]

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


