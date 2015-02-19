open Core.Std
open NetKAT_FDD
open NetKAT_Types


let int_of_val v =
  match v with
  | Value.Const k -> Int64.to_int_exn k
  | _ -> assert false



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
    let compare = Int.compare
  end)

  module U = Hashtbl.Make(struct
    type t = (int * int) with sexp
    let hash (t1, t2) = 617 * t1 +  619 * t2
    let compare = Pervasives.compare
  end)

  type t0 =
    { trees : (FDK.t * FDK.t) Lazy.t T.t;
      rootId : int;
      mutable nextId : int }

  type t =
    { trees : (FDK.t * FDK.t) T.t;
      ids : int U.t;
      mutable rootId : int;
      mutable nextId : int }

  let create_t0 () =
    let trees = T.create () ~size:10 in
    let rootId = 0 in
    { trees; rootId; nextId = rootId+1 }

  let create_t () =
    let trees = T.create () ~size:10 in
    let ids = U.create () ~size:10 in
    let rootId = 0 in
    { trees; ids; rootId; nextId = rootId+1 }

  let copy_t (forest : t) =
    { trees = T.copy forest.trees;
      ids = U.copy forest.ids;
      rootId = forest.rootId;
      nextId = forest.nextId }

  let mk_id (forest : t0) =
    let id = forest.nextId in
    begin
      forest.nextId <- id + 1;
      id
    end

  let add t fdks =
    match U.find t.ids fdks with
    | Some id -> id
    | None ->
      let id = t.nextId in
      t.nextId <- t.nextId + 1;
      T.add_exn t.trees ~key:id ~data:fdks;
      U.add_exn t.ids ~key:fdks ~data:id;
      id

  let get t fdks = U.find_exn t.ids fdks

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

  let t0_to_t (forest : t0) =
    let t = create_t () in
    let rec loop id =
      let fdk = T.find_exn forest.trees id in
      let seen = Lazy.is_val fdk in
      let (e,d) = Lazy.force fdk in
      if seen then get t (e,d)
      else
        let d = FDK.map_r (fun par -> ActionK.Par.map par ~f:(fun seq ->
          ActionK.(Seq.change seq K (function None -> None | Some id ->
            loop (int_of_val id) |> Value.of_int |> Option.some)))) d in
        add t (e,d)
    in
    let rootId = loop forest.rootId in
    t.rootId <- rootId;
    t

  let rec split_pol (forest : t0) (pol: Pol.policy) : FDK.t * FDK.t * ((int * Pol.policy) list) =
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

  let rec add_policy (forest : t0) (id, pol : int * Pol.policy) : unit =
    let f () =
      let (e,d,k) = split_pol forest pol in
      List.iter k ~f:(add_policy forest);
      (e, d)
    in
    T.add_exn forest.trees ~key:id ~data:(Lazy.from_fun f)

  let of_policy (pol : NetKAT_Types.policy) : t =
    let forest = create_t0 () in
    let pol = Pol.of_pol pol in
    add_policy forest (forest.rootId, pol);
    t0_to_t forest

  let pc_unused pc fdd =
    dp_fold
      (fun par -> ActionK.Par.for_all par ~f:(fun seq -> not (ActionK.(Seq.mem seq (F pc)))))
      (fun (f,_) l r -> l && r && f<>pc)
      fdd

  let to_local (pc : Field.t) (forest : t) : NetKAT_LocalCompiler.t =
    (* make copy as we will destroy the forest in the process *)
    let forest = copy_t forest in
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
        begin match T.find_and_remove forest.trees k with
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

  (* SJS: horrible hack *)
  let to_dot (forest : t) =
    let trees = T.map forest.trees ~f:(fun (e,d) -> union e d) in
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
          ActionK.Par.iter par ~f:(fun seq ->
            let id = sprintf "\"%dS%d\"" node (!seqId) in
            let cont = ActionK.Seq.find seq K |> Option.map ~f:(fun v -> T.find_exn trees (int_of_val v)) in
            let label = Action.to_string (ActionK.to_action_wout_conts (ActionK.Par.singleton seq)) in
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
      let fdk = T.find_exn trees fdkId in
      let conts = conts_of_fdk fdk in
      fdks := fdk :: (!fdks);
      node_loop fdk;
      List.iter conts ~f:fdk_loop
    in
    fdk_loop forest.rootId;
    fprintf fmt "%d [style=bold, color=red];@\n" (T.find_exn trees forest.rootId);
    fprintf fmt "{rank=source; ";
    List.iter (!fdks) ~f:(fun fdk -> fprintf fmt "%d " fdk);
    fprintf fmt ";}@\n";
    fprintf fmt "}@.";
    Buffer.contents buf

end

include Repr
