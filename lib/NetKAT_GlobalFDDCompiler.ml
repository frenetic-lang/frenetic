open Core.Std
open NetKAT_FDD
open NetKAT_Types

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

  let match_loc sw pt =
    let t1 = Test (Switch sw) in
    let t2 = Test (Location (Physical pt)) in
    Optimize.mk_and t1 t2

  let mk_filter pred = Filter pred
  let filter_loc sw pt = match_loc sw pt |> mk_filter
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

  let rec of_pol (pol : NetKAT_Types.policy) : policy =
    match pol with
    | NetKAT_Types.Filter a -> Filter a
    | NetKAT_Types.Mod hv -> Mod hv
    | NetKAT_Types.Union (p,q) -> Union (of_pol p, of_pol q)
    | NetKAT_Types.Seq (p,q) -> Seq (of_pol p, of_pol q)
    | NetKAT_Types.Star p -> Star (of_pol p)
    | NetKAT_Types.Link (s1,p1,s2,p2) ->
      (* SJS: This is not the true sematnics of a link! This is a hack that works for now,
         but we will need to use the correct encoding once we start doing things like global
         optimization or deciding equivalence. *)
      mk_big_seq [filter_loc s1 p1; Dup; filter_loc s2 p2]

end



(* FDKs are just FDDs with continuations.
   Here we extend the FDD module with functions relevant for compilation,
   i.e. we define the operations for policies on FDDs.
   This is almost an exact copy of the local compiler *)
module FDK = struct

  include FDK

  let of_test hv =
    atom (Pattern.of_hv hv) ActionK.one ActionK.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    const ActionK.(Par.singleton (Seq.singleton (F k) v))

  let rec of_pred p =
    match p with
    | True      -> mk_id ()
    | False     -> mk_drop ()
    | Test(hv)  -> of_test hv
    | And(p, q) -> prod (of_pred p) (of_pred q)
    | Or (p, q) -> sum (of_pred p) (of_pred q)
    | Neg(q)    -> map_r ActionK.negate (of_pred q)

  let cond v t f =
    if equal t f then
      t
    else
      (sum (prod (atom v ActionK.one ActionK.zero) t)
             (prod (atom v ActionK.zero ActionK.one) f))

  let dp_fold (g : ActionK.t -> 'a)
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
                              [ActionK.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
    | None   ->
      dp_fold
        (fun par ->
          ActionK.Par.fold par ~init:(mk_drop ()) ~f:(fun acc seq ->
            let mods = ActionK.Seq.(to_alist seq) |> List.filter_map ~f:(fun (f,n) ->
              match f with
              | ActionK.F f -> Some (f,n)
              | ActionK.K -> None)
            in
            let u' = restrict mods u in
            (sum (prod (const ActionK.Par.(singleton seq)) u') acc)))
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

  (* Do NOT eta-reduce to avoid caching problems with mk_id *)
  let star t = star' (mk_id ()) t

end


(* A FDKG is a graph of FDKs that may contain cycles. *)
module FDKG = struct

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

  (* main FDKG data structure *)
  type t =
    { trees : (FDK.t * FDK.t) T.t;
      mutable rootId : int;
      mutable nextId : int }

  (* lazy intermediate presentation to avoid compiling uncreachable FDKG nodes *)
  type t0 =
    { trees : (FDK.t * FDK.t) Lazy.t T.t;
      rootId : int;
      mutable nextId : int }

  let create_t0 () : t0 =
    let trees = T.create () ~size:10 in
    let rootId = 0 in
    { trees; rootId; nextId = rootId+1 }

  let create_t () : t =
    let trees = T.create () ~size:10 in
    let rootId = 0 in
    { trees; rootId; nextId = rootId+1 }

  let mk_id_t0 (forest : t0) =
    let id = forest.nextId in
    forest.nextId <- id + 1;
    id

  let mk_id_t (forest : t) =
    let id = forest.nextId in
    forest.nextId <- id + 1;
    id

  let map_reachable ?(order = `Pre) (forest : t) ~(f: int -> (FDK.t * FDK.t) -> (FDK.t * FDK.t)) : unit =
    let rec loop seen (id : int) =
      if not (S.mem seen id) then
        let seen = S.add seen id in
        let fdks = T.find_exn forest.trees id in
        let this () =
          let fdks = f id fdks in
          T.replace forest.trees ~key:id ~data:fdks; fdks in
        let that (_,d) = List.iter (FDK.conts d) ~f:(loop seen) in
        match order with
        | `Pre -> () |> this |> that |> ignore
        | `Post -> fdks |> that |> this |> ignore
    in
    loop S.empty forest.rootId

  let fold_reachable ?(order = `Pre) (forest : t) ~(init : 'a) ~(f: 'a -> int -> (FDK.t * FDK.t) -> 'a) =
    let rec loop (acc, seen) (id : int) =
      if S.mem seen id then (acc, seen) else
        let seen = S.add seen id in
        let (_,d) as fdks = T.find_exn forest.trees id in
        let this (acc, seen) = (f acc id fdks, seen) in
        let that (acc, seen) = List.fold (FDK.conts d) ~init:(acc, seen) ~f:loop in
        match order with
        | `Pre -> (acc, seen) |> this |> that
        | `Post -> (acc, seen) |> that |> this
    in
    loop (init, S.empty) forest.rootId |> fst

  let iter_reachable ?(order = `Pre) (forest : t) ~(f: int -> (FDK.t * FDK.t) -> unit) : unit =
    fold_reachable forest ~order ~init:() ~f:(fun _ -> f)

  let t_of_t0 (forest : t0) =
    let t = create_t () in
    let rec add id =
      if not (T.mem t.trees id) then
        let _ = t.nextId <- max t.nextId (id + 1) in
        let (_,d) as fdk = Lazy.force (T.find_exn forest.trees id) in
        T.add_exn t.trees ~key:id ~data:fdk;
        List.iter (FDK.conts d) ~f:add
    in
    add forest.rootId;
    t.rootId <- forest.rootId;
    t


  let dedup_global (forest : t) : unit =
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
        let k = mk_id_t forest in
        let (es, ds) =
          List.map ks ~f:(T.find_exn forest.trees)
          |> List.unzip in
        let fdk = (FDK.big_union es, FDK.big_union ds) in
        T.add_exn forest.trees ~key:k ~data:fdk;
        S.Table.add_exn tbl ~key:ks_set ~data:k;
        Int.Table.add_exn untbl ~key:k ~data:ks;
        k
    in
    let dedup_action par =
      par
      |> ActionK.Par.to_list
      |> List.group ~break:(fun s1 s2 -> not (ActionK.Seq.equal_mod_k s1 s2))
      |> List.map ~f:(function
        | [seq] -> seq
        | group ->
          let ks = List.map group ~f:(fun s -> ActionK.Seq.find_exn s K |> Value.to_int_exn) in
          let k = merge ks in
          List.hd_exn group |> ActionK.Seq.add ~key:K ~data:(Value.of_int k))
      |> ActionK.Par.of_list
    in
    let dedup_fdk = FDK.map_r dedup_action in
    map_reachable forest ~order:`Pre ~f:(fun _ (e,d) -> (e, dedup_fdk d))


  let rec split_pol (forest : t0) (pol: Pol.policy) : FDK.t * FDK.t * ((int * Pol.policy) list) =
    match pol with
    | Filter pred -> (FDK.of_pred pred, FDK.mk_drop (), [])
    | Mod hv -> (FDK.of_mod hv, FDK.mk_drop (), [])
    | Union (p,q) ->
      let (e_p, d_p, k_p) = split_pol forest p in
      let (e_q, d_q, k_q) = split_pol forest q in
      let e = FDK.union e_p e_q in
      let d = FDK.union d_p d_q in
      let k = k_p @ k_q in
      (e, d, k)
    | Seq (p,q) ->
      (* TODO: short-circuit *)
      let (e_p, d_p, k_p) = split_pol forest p in
      let (e_q, d_q, k_q) = split_pol forest q in
      let e = FDK.seq e_p e_q in
      let d = FDK.union d_p (FDK.seq e_p d_q) in
      let q' = Pol.mk_fdk e_q d_q in
      let k = (List.map k_p ~f:(fun (id,p) -> (id, Pol.mk_seq p q'))) @ k_q in
      (e, d, k)
    | Star p ->
      let (e_p, d_p, k_p) = split_pol forest p in
      let e = FDK.star e_p in
      let d = FDK.seq e d_p in
      let pol' = Pol.mk_fdk e d in
      let k = List.map k_p ~f:(fun (id,k) -> (id, Pol.mk_seq k pol')) in
      (e, d, k)
    | Dup ->
      let id = mk_id_t0 forest in
      let e = FDK.mk_drop () in
      let d = FDK.mk_cont id in
      let k = [(id, Pol.id)] in
      (e, d, k)
    | FDK (e,d) -> (e,d,[])

  let rec add_policy (forest : t0) (id, pol : int * Pol.policy) : unit =
    let f () =
      let (e,d,k) = split_pol forest pol in
      List.iter k ~f:(add_policy forest);
      (e, d)
    in
    T.add_exn forest.trees ~key:id ~data:(Lazy.from_fun f)

  let of_policy ?(dedup=true) (pol : NetKAT_Types.policy) : t =
    let forest = create_t0 () in
    let pol = Pol.of_pol pol in
    let () = add_policy forest (forest.rootId, pol) in
    let forest = t_of_t0 forest in
    let () = if dedup then dedup_global forest in
    forest


  let pc_unused pc fdd =
    FDK.fold
      (fun par -> ActionK.Par.for_all par ~f:(fun seq -> not (ActionK.(Seq.mem seq (F pc)))))
      (fun (f,_) l r -> l && r && f<>pc)
      fdd

  let to_local (pc : Field.t) (forest : t) : NetKAT_LocalCompiler.t =
    let fdk_to_fdd =
      FDK.fold
        (fun par -> ActionK.to_action (fun v -> (pc,v)) par |> NetKAT_FDD.T.mk_leaf)
        (* SJS: using mk_branch here is safe since variable order of fdk and fdd agree *)
        (fun v t f -> NetKAT_FDD.T.mk_branch v t f)
    in
    fold_reachable forest ~init:(NetKAT_FDD.T.mk_drop ()) ~f:(fun acc id (e,d) ->
      let _ = assert (pc_unused pc e && pc_unused pc d) in
      let guard =
        if id = forest.rootId then FDK.mk_id ()
        else FDK.atom (pc, Value.of_int id) ActionK.one ActionK.zero in
      let fdk = FDK.seq guard (FDK.union e d) in
      let fdd = fdk_to_fdd fdk in
      NetKAT_LocalCompiler.union acc fdd)


  (* SJS: horrible hack *)
  let to_dot (forest : t) =
    let trees = T.map forest.trees ~f:(fun (e,d) -> FDK.union e d) in
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
            let cont = ActionK.Seq.find seq K |> Option.map ~f:(fun v -> T.find_exn trees (Value.to_int_exn v)) in
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
      let conts = FDK.conts fdk in
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

include FDKG

let compile (pol : NetKAT_Types.policy) : NetKAT_LocalCompiler.t =
  to_local Field.Vlan (of_policy ~dedup:true pol)
