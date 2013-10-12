module NetworkCompiler = struct

  open NetKAT_Types

  type explicit_topo_policy =
    | Filter of pred
    | Mod of SDN_Types.field * header_val
    (* switch, port -> switch, port *)
    | Link of header_val*header_val*header_val*header_val
    | Par of explicit_topo_policy * explicit_topo_policy
    | Seq of explicit_topo_policy * explicit_topo_policy
    | Star of explicit_topo_policy

(* i;(p;t)^*;p;e 
   where 
   i = t = v | t <- v | i + i | i ; i
   p = t = v | h = v | t <- v | h <- v | p + p | p ; p | p*
   t = (sw,pt) -> (sw',pt') | t + t
   e = t = v | i + i | i ; i
*)

  type vheader = int * int

  type ingress_pol =
    | ITest of vheader * header_val
    | IMod of vheader * header_val
    | IPar of ingress_pol * ingress_pol
    | ISeq of ingress_pol * ingress_pol
    | IPass

  type sPred = 
    | STrue
    | SFalse
    | STest of header * header_val
    | STTest of vheader * header_val
    | SNeg of sPred
    | SAnd of sPred * sPred
    | SOr of sPred * sPred


  type switch_pol =
    | SFilter of sPred
    | SMod of SDN_Types.field * header_val
    | STMod of vheader * header_val
    | SPar of switch_pol * switch_pol
    | SSeq of switch_pol * switch_pol
    | SStar of switch_pol
    | SPass
    | SDrop

  type topo_pol = 
    (* (sw,pt) -> (sw',pt') *)
    | TLink of header_val * header_val * header_val * header_val
    | TPar of topo_pol * topo_pol
    | TDrop

  type restricted_pol = ingress_pol * switch_pol * topo_pol * ingress_pol

  let vheader_count = ref 0

  let gen_header size =
    incr vheader_count;
    (!vheader_count, size)

  let rec pred_to_spred pr = 
    match pr with
      | True -> STrue
      | False -> SFalse
      | Test (h, v) -> STest(h, v)
      | And(a,b) -> SAnd (pred_to_spred a, pred_to_spred b)
      | Or(a,b) -> SOr (pred_to_spred a, pred_to_spred b)
      | Neg a -> SNeg (pred_to_spred a)

  let rec ipol_to_spol p =
    match p with
    | ITest (h,v) -> SFilter(STTest(h,v))
    | IMod (h,v) -> STMod(h,v)
    | IPar (p1,p2) -> SPar(ipol_to_spol p1, ipol_to_spol p2)
    | ISeq (p1,p2) -> SSeq(ipol_to_spol p1, ipol_to_spol p2)
    | IPass -> SPass

(* Compilation story: we have an unlimited number of header fields we
   can allocate on demand. Each header field has a specific number of
   entries. At a later stage, we inject the multiple header fields into a
   single header that allows arbitrary bitmasking. There should be some
   analysis/optimizations we can do (ala the earlier slices compiler) to
   reduce the number of unique entries required.

   Alternatively, lacking a bitmaskable field, we could simply expand
   all possible combinations of values and match/set them
   appropriately. We'd need some pretty good analysis to keep this
   from exploding.
*)
  let rec seqList ls =
    match ls with
      | [] -> SPass
      | l :: ls -> SSeq(l, seqList ls)

  let rec parList ls =
    match ls with
      | [] -> SDrop
      | l :: ls -> SPar(l, parList ls)


  let rec dehopify (p : explicit_topo_policy) : restricted_pol =
    match p with
      | Filter pr -> 
        let h = gen_header 2 in
        let h0 = VInt.Int16 0 in
        (ignore h0);
        let h1 = VInt.Int16 1 in
        IMod(h,h0), 
        SSeq(SFilter(STTest(h,h0)), SSeq(SFilter (pred_to_spred pr), STMod(h,h1))),
        TDrop, 
        ITest(h,h1)
      | Mod (h, v) -> 
        let h = gen_header 2 in
        let h0 = VInt.Int16 0 in
        (ignore h0);
        let h1 = VInt.Int16 1 in
        IMod(h,h0), 
        SSeq(SFilter(STTest(h,h0)), SSeq(STMod(h,v), STMod(h,h1))),
        TDrop, 
        ITest(h,h1)
      | Link (sw1,p1,sw2,p2) -> 
        let h = gen_header 3 in
        let h0 = VInt.Int16 0 in
        (ignore h0);
        let h1 = VInt.Int16 1 in
        let h2 = VInt.Int16 2 in
        IMod(h,h0),
        SPar(SSeq(SFilter(STTest(h,h0)), SSeq(SSeq(SFilter(STest(Switch, sw1)),
                                                   SFilter(STest(Header SDN_Types.InPort, p1))),
                                              STMod(h,h1))),
             SSeq(SFilter(STTest(h,h1)), SSeq(SSeq(SFilter(STest(Switch, sw2)),
                                                   SFilter(STest(Header SDN_Types.InPort, p2))),
                                              STMod(h,h2)))),
        TLink(sw1,p1,sw2,p2),
        ITest(h,h1)
      | Par (p,q) -> let i_p,s_p,t_p,e_p = dehopify p in
                     let i_q,s_q,t_q,e_q = dehopify q in
                     let h = gen_header 2 in
                     let h0 = VInt.Int16 0 in
                     let h1 = VInt.Int16 1 in
                     IPar(ISeq(IMod(h,h0), i_p),
                          ISeq(IMod(h,h1), i_q)),
                     SPar(SSeq(SFilter(STTest(h,h0)),s_p),
                          SSeq(SFilter(STTest(h,h1)),s_q)),
                     TPar(t_p,t_q),
                     IPar(ISeq(ITest(h,h0), e_p),
                          ISeq(ITest(h,h1), e_q))
      | Seq (p,q) -> let i_p,s_p,t_p,e_p = dehopify p in
                     let i_q,s_q,t_q,e_q = dehopify q in
                     let h = gen_header 8 in
                     let h0 = VInt.Int16 0 in
                     let h1 = VInt.Int16 1 in
                     let h1' = VInt.Int16 2 in
                     let h2 = VInt.Int16 3 in
                     let h2' = VInt.Int16 4 in
                     let h3 = VInt.Int16 5 in
                     let h3' = VInt.Int16 6 in
                     let h3'' = VInt.Int16 7 in
                     let h4 = VInt.Int16 8 in
                     ISeq(IPar(IMod(h,h0),
                               IPar(IMod(h,h1),
                                    IPar(IMod(h,h2),
                                         IMod(h,h3)))),
                          i_p),
                     parList [seqList [SFilter(STTest(h,h0));
                                       s_p;
                                       ipol_to_spol(e_p);
                                       ipol_to_spol(i_q);
                                       s_q;
                                       STMod(h,h4)];
                              seqList [SFilter(STTest(h,h1));
                                       s_p;
                                       SPar(STMod(h,h1),
                                            STMod(h,h1'))];
                              seqList [SFilter(STTest(h,h1'));
                                       s_p;
                                       ipol_to_spol(e_p);
                                       ipol_to_spol(i_q);
                                       s_q;
                                       STMod(h,h4)];
                              seqList [SFilter(STTest(h,h2));
                                       s_p;
                                       ipol_to_spol(e_p);
                                       ipol_to_spol(i_q);
                                       s_q;
                                       STMod(h,h2')];
                              SSeq (SFilter(STTest(h,h2')), s_q);
                              seqList [SFilter(STTest(h,h3));
                                       s_p;
                                       STMod(h,h3')];
                              seqList [SFilter(STTest(h,h3'));
                                       s_p;
                                       SPar(SPass,
                                            seqList [ipol_to_spol(e_p);
                                                     ipol_to_spol(i_q);
                                                     s_q;
                                                     STMod(h, h3'')])];
                              SSeq(SFilter(STTest(h,h3'')), s_q)],
                     TPar(t_p,t_q),
                     e_q
      | Star p -> let i_p,s_p,t_p,e_p = dehopify p in
                  let h = gen_header 7 in
                  let h0 = VInt.Int16 0 in
                  let h1 = VInt.Int16 1 in
                  let h2 = VInt.Int16 2 in
                  let h2' = VInt.Int16 3 in
                  let h2'' = VInt.Int16 4 in
                  let h3 = VInt.Int16 5 in
                  let h4 = VInt.Int16 6 in
                  IPar(IPar(IMod(h,h0),
                            IMod(h,h1)),
                       IMod(h,h2)),
                  parList [ SSeq(SFilter(STTest(h,h0)),
                                 STMod(h,h3));
                            seqList [SFilter(STTest(h,h1));
                                     ipol_to_spol(i_p);
                                     s_p;
                                     SStar(seqList [ipol_to_spol(e_p);ipol_to_spol(i_p);s_p]);
                                     STMod(h,h4)];
                            seqList [SFilter(STTest(h,h2));
                                     ipol_to_spol(i_p);
                                     s_p;
                                     SStar(seqList [ipol_to_spol(e_p);ipol_to_spol(i_p);s_p]);
                                     SPar(STMod(h,h2'),
                                          STMod(h,h2''))];
                            seqList [SFilter(STTest(h,h2'));
                                     s_p;
                                     SPar(STMod(h,h2'),
                                          STMod(h,h2''))];
                            seqList [SFilter(STTest(h,h2''));
                                     s_p;
                                     SStar(seqList [ipol_to_spol(e_p);ipol_to_spol(i_p);s_p]);                                     
                                     SPar(STMod(h,h2'),
                                          STMod(h,h2''))]],
                  t_p,
                  IPar(ISeq(IPar(ITest(h,h2'),
                                 IPar(ITest(h,h2''),
                                      ITest(h,h4))),
                            e_p),
                       ITest(h,h3))
      | _ -> 
        failwith "Unimplemented" 
end

module SwitchCompiler = struct
    (* metavariable conventions
       - a, b, c, actions
       - s, t, u, action sets
       - x, y, z, patterns
       - xs, ys, zs, pattern sets
       - p, q, local
       - r, atoms
    *)
    
    (* utility functions *)
  let header_val_map_to_string eq sep m =
    NetKAT_Types.HeaderMap.fold
      (fun h v acc ->
        Printf.sprintf "%s%s%s%s"
	  (if acc = "" then "" else acc ^ sep)
	  (NetKAT_Types.string_of_header h)
	  eq
	  (NetKAT_Types.string_of_vint v))
      m ""

  module Action = struct
    type t = NetKAT_Types.header_val_map 

    module Set = Set.Make (struct
      type t = NetKAT_Types.header_val_map

      let compare = NetKAT_Types.HeaderMap.compare Pervasives.compare
    end)

    let to_string (a:t) : string =
      if NetKAT_Types.HeaderMap.is_empty a then "id"
      else Printf.sprintf "%s" (header_val_map_to_string ":=" ", " a)
        
    let set_to_string (s:Set.t) : string =
      Printf.sprintf "{%s}"
        (Set.fold
           (fun a acc -> to_string a ^ if acc = "" then "" else ", " ^ acc)
           s "")
        
    let id : Set.t = 
      Set.singleton (NetKAT_Types.HeaderMap.empty)

    let drop : Set.t = 
      Set.empty

    let seq_act (a:t) (b:t) : t =
      let f h vo1 vo2 = match vo1, vo2 with
        | (_, Some v2) ->
          Some v2
        | _ -> 
          vo1 in
      NetKAT_Types.HeaderMap.merge f a b

    let seq_acts (a:t) (s:Set.t) : Set.t = 
      Set.fold 
        (fun b acc -> Set.add (seq_act a b) acc) 
        s Set.empty


    let to_netkat (pol:t) : NetKAT_Types.policy =
      if NetKAT_Types.HeaderMap.is_empty pol then
        NetKAT_Types.Filter NetKAT_Types.True
      else
        let (h, v) = NetKAT_Types.HeaderMap.min_binding pol in
        let pol' = NetKAT_Types.HeaderMap.remove h pol in
        let f h v pol' = NetKAT_Types.Seq (pol', NetKAT_Types.Mod (h, v)) in
        NetKAT_Types.HeaderMap.fold f pol' (NetKAT_Types.Mod  (h, v))
          
    let set_to_netkat (pol:Set.t) : NetKAT_Types.policy =
      if Set.is_empty pol then
        NetKAT_Types.Filter NetKAT_Types.False
      else
        let f seq pol' = NetKAT_Types.Par (pol', to_netkat seq) in
        let seq = Set.min_elt pol in
        let pol' = Set.remove seq pol in
        Set.fold f pol' (to_netkat seq)

    let to_action (a:t) : SDN_Types.seq =
      if not (NetKAT_Types.HeaderMap.mem (NetKAT_Types.Header SDN_Types.InPort) a) then
        []
      else
        let port = NetKAT_Types.HeaderMap.find (NetKAT_Types.Header SDN_Types.InPort) a in  
        let mods = NetKAT_Types.HeaderMap.remove (NetKAT_Types.Header SDN_Types.InPort) a in
        let mk_mod h v act = 
          match h with
            | NetKAT_Types.Switch -> raise (Invalid_argument "Action.to_action got switch update")
            | NetKAT_Types.Header h' ->  (SDN_Types.SetField (h', v)) :: act in
        NetKAT_Types.HeaderMap.fold mk_mod mods [SDN_Types.OutputPort port]
          
    let set_to_action (s:Set.t) : SDN_Types.par =
      let f a par = (to_action a)::par in
      Set.fold f s []
  end

  module Pattern = struct
    exception Empty_pat

    type t = NetKAT_Types.header_val_map 

    module Set = Set.Make(struct
      type t = NetKAT_Types.header_val_map 
          
      let compare = NetKAT_Types.HeaderMap.compare Pervasives.compare
    end)

    let to_string (x:t) : string =
      if NetKAT_Types.HeaderMap.is_empty x then "true"
      else Printf.sprintf "%s" (header_val_map_to_string "=" ", " x)
        
    let set_to_string (xs:Set.t) : string =
      Printf.sprintf "{%s}"
        (Set.fold
           (fun x acc -> to_string x ^ if acc = "" then "" else ", " ^ acc)
           xs "")

    let tru : t = NetKAT_Types.HeaderMap.empty

    let matches (h:NetKAT_Types.header) (v:NetKAT_Types.header_val) (x:t) : bool =
      not (NetKAT_Types.HeaderMap.mem h x) 
      || NetKAT_Types.HeaderMap.find h x = v

    let apply_act (x:t) (a:t) : t = 
      let f h vo1 vo2 = match vo1,vo2 with 
        | _, Some v2 -> 
          Some v2
        | _ -> 
          vo1 in 
      NetKAT_Types.HeaderMap.merge f a x 
        
    let seq_pat (x : t) (y : t) : t option =
      let f h vo1 vo2 = match vo1, vo2 with
        | (Some v1, Some v2) ->
          if v1 <> v2 then raise Empty_pat else Some v1
        | (Some v1, None) ->
          Some v1
        | (None, Some v2) ->
          Some v2
        | (None, None) ->
          None in
      try
        Some (NetKAT_Types.HeaderMap.merge f x y)
      with Empty_pat -> 
        None

    let rec seq_act_pat (x:t) (a:Action.t) (y:t) : t option =
      let f h vo1 vo2 = match vo1, vo2 with
        | Some (vo11, Some v12), Some v2 ->
          if v12 <> v2 then raise Empty_pat else vo11
        | Some (vo11, Some v12), None -> 
          vo11 
        | Some (Some v11, None), Some v2 -> 
          if v11 <> v2 then raise Empty_pat else Some v11
        | Some (Some v11, None), None ->
          Some v11
        | _, Some v2 -> 
          Some v2
        | _, None ->
          None in 
      let g h vo1 vo2 = Some (vo1, vo2) in 
      try
        Some (NetKAT_Types.HeaderMap.merge f 
                (NetKAT_Types.HeaderMap.merge g x a) y)
      with Empty_pat -> 
        None

    let to_netkat (x:t) : NetKAT_Types.pred =  
      if NetKAT_Types.HeaderMap.is_empty x then
        NetKAT_Types.True
      else
        let (h, v) = NetKAT_Types.HeaderMap.min_binding x in
        let x' = NetKAT_Types.HeaderMap.remove h x in
        let f h v pol = NetKAT_Types.And (pol, NetKAT_Types.Test (h, v)) in
        (NetKAT_Types.HeaderMap.fold f x' (NetKAT_Types.Test (h, v)))

    let set_to_netkat (xs:Set.t) : NetKAT_Types.pred = 
      if Set.is_empty xs then 
        NetKAT_Types.False
      else
        let x = Set.choose xs in 
        let xs' = Set.remove x xs in 
        let f x pol = NetKAT_Types.Or(pol, to_netkat x) in 
        Set.fold f xs' (to_netkat x)

    let to_pattern (sw : SDN_Types.fieldVal) (x:t) : SDN_Types.pattern option =
      let f (h : NetKAT_Types.header) (v : NetKAT_Types.header_val) (pat : SDN_Types.pattern) =
        match h with
          | NetKAT_Types.Switch -> pat (* already tested for this *)
          | NetKAT_Types.Header h' -> SDN_Types.FieldMap.add h' v pat in
      if NetKAT_Types.HeaderMap.mem NetKAT_Types.Switch x &&
        NetKAT_Types.HeaderMap.find NetKAT_Types.Switch x <> sw then
        None
      else 
        Some (NetKAT_Types.HeaderMap.fold f x SDN_Types.FieldMap.empty)
  end

  module Atom = struct
    type t = Pattern.Set.t * Pattern.t

    module Map = Map.Make (struct
      type t = Pattern.Set.t * Pattern.t 

      let compare (xs1,x1) (xs2,x2) = 
        if Pattern.Set.mem x2 xs1 then 1
        else if Pattern.Set.mem x1 xs2 then -1
        else 
          let cmp1 = Pattern.Set.compare xs1 xs2 in 
          if cmp1 = 0 then 
            NetKAT_Types.HeaderMap.compare Pervasives.compare x1 x2 
          else
            cmp1
    end)
      
    let to_string ((xs,x):t) : string = 
      Printf.sprintf "%s,%s" 
        (Pattern.set_to_string xs) (Pattern.to_string x)

    let tru : t = 
      (Pattern.Set.empty, Pattern.tru)

    let fls : t = 
      (Pattern.Set.singleton Pattern.tru, Pattern.tru) 

    let check ((xs,x) as r:t) = 
      if Pattern.Set.mem x xs then 
        None
      else 
        Some r

    let apply_act ((xs,x):t) (a:Action.t) : t = 
      let x' = Pattern.apply_act x a in 
      (Pattern.Set.remove x' xs, x')
        
    let seq_atom ((xs1,x1):t) ((xs2,x2):t) : t option = 
      match Pattern.seq_pat x1 x2 with 
        | Some x12 -> 
          check (Pattern.Set.union xs1 xs2, x12)
        | None -> 
          None

    let seq_act_atom ((xs1,x1):t) (a:Action.t) ((xs2,x2):t) : t option = 
      match Pattern.seq_act_pat x1 a x2 with 
        | Some x12 -> 
          check (Pattern.Set.union xs1 xs2, x12)
        | None -> 
          None       
  end

  module Local = struct
    type t = Action.Set.t Atom.Map.t

    let to_string (p:t) : string =
      Atom.Map.fold
        (fun r s acc -> 
          Printf.sprintf "%s(%s) => %s\n"
            (if acc = "" then "" else "" ^ acc)
            (Atom.to_string r) (Action.set_to_string s))
        p ""

    let extend (r:Atom.t) (s:Action.Set.t) (p:t) : t = 
      let (xs,x) = r in 
      if Pattern.Set.mem Pattern.tru xs || Pattern.Set.mem x xs then 
        p
      else if Atom.Map.mem r p then 
        Atom.Map.add r (Action.Set.union s (Atom.Map.find r p)) p
      else
        Atom.Map.add r s p
          
    let rec par_local (p:t) (q:t) : t =
      let diff_atoms ((xs1,x1):Atom.t) ((xs2,x2):Atom.t) (s1:Action.Set.t) (p:t) : t = 
        Pattern.Set.fold 
          (fun x2i acc -> 
            match Pattern.seq_pat x1 x2i with 
              | None -> acc
              | Some x12i -> extend (xs1,x12i) s1 acc) 
          xs2 (extend (Pattern.Set.add x2 xs1, x1) s1 p) in 
      Atom.Map.fold (fun ((xs1,x1) as r1) s1 acc -> 
        Atom.Map.fold (fun ((xs2,x2) as r2) s2 acc -> 
          match Atom.seq_atom r1 r2 with 
            | None -> 
              extend r1 s1 (extend r2 s2 acc)
            | Some r12 -> 
              extend r12 (Action.Set.union s1 s2)
                (diff_atoms r1 r2 s1
                   (diff_atoms r2 r1 s2 acc)))
          p acc)
        q Atom.Map.empty

      (* TODO(jnf) this is a helper function; give it a different name? *)
    let seq_atom_act_local_acc (r1:Atom.t) (a:Action.t) (p2:t) (q:t) : t = 
      Atom.Map.fold
        (fun r2 s2 acc -> 
          match Atom.seq_act_atom r1 a r2 with
            | None -> 
              acc
            | Some r12 -> 
              extend r12 (Action.seq_acts a s2) acc)
        p2 q

    let seq_local (p:t) (q:t) : t = 
      Atom.Map.fold
        (fun r1 s1 acc -> 
          if Action.Set.is_empty s1 then 
            extend r1 s1 acc 
          else
            Action.Set.fold 
              (fun a acc -> seq_atom_act_local_acc r1 a q acc)
              s1 acc)
        p Atom.Map.empty
        
      (* pre: t is a predicate *)
    let negate (p:t) : t = 
      Atom.Map.fold
        (fun r s acc -> 
          if Action.Set.is_empty s then 
            Atom.Map.add r Action.id acc
          else
            Atom.Map.add r Action.drop acc)
        p Atom.Map.empty

    let rec of_pred (pr:NetKAT_Types.pred) : t = 
      match pr with
        | NetKAT_Types.True ->
          Atom.Map.singleton Atom.tru Action.id
        | NetKAT_Types.False ->
          Atom.Map.singleton Atom.tru Action.drop
        | NetKAT_Types.Neg p ->
          negate (of_pred p)
        | NetKAT_Types.Test (h, v) ->
          let p = NetKAT_Types.HeaderMap.singleton h v in 
          Atom.Map.add (Pattern.Set.empty, p) Action.id
            (Atom.Map.singleton (Pattern.Set.singleton p, Pattern.tru) Action.drop)
        | NetKAT_Types.And (pr1, pr2) ->
          seq_local (of_pred pr1) (of_pred pr2)
        | NetKAT_Types.Or (pr1, pr2) ->
          par_local (of_pred pr1) (of_pred pr2)

    let star_local (p:t) : t =
      let rec loop acc = 
        let seq' = seq_local acc p in
        let acc' = par_local acc seq' in
        if Atom.Map.compare Action.Set.compare acc acc' = 0 then
          acc
        else 
          loop acc' in
      loop (Atom.Map.singleton Atom.tru Action.id)

    let rec of_policy (pol:NetKAT_Types.policy) : t = 
      match pol with
        | NetKAT_Types.Filter pr ->
          of_pred pr
        | NetKAT_Types.Mod (NetKAT_Types.Switch, _) ->
          failwith "unexpected Switch in local_normalize"
        | NetKAT_Types.Mod (h, v) ->
          Atom.Map.singleton Atom.tru (Action.Set.singleton (NetKAT_Types.HeaderMap.singleton h v))
        | NetKAT_Types.Par (pol1, pol2) ->
          par_local (of_policy pol1) (of_policy pol2)
        | NetKAT_Types.Seq (pol1, pol2) ->
          seq_local (of_policy pol1) (of_policy pol2)
        | NetKAT_Types.Star pol -> 
          star_local (of_policy pol)

    let to_netkat (p:t) : NetKAT_Types.policy = 
        (* "smart" constructors *)
      let mk_par nc1 nc2 = 
        match nc1, nc2 with 
          | NetKAT_Types.Filter NetKAT_Types.False, _ -> nc2
          | _, NetKAT_Types.Filter NetKAT_Types.False -> nc1
          | _ -> NetKAT_Types.Par(nc1,nc2) in       
      let mk_seq nc1 nc2 = 
        match nc1, nc2 with
          | NetKAT_Types.Filter NetKAT_Types.True, _ -> nc2
          | _, NetKAT_Types.Filter NetKAT_Types.True -> nc1
          | NetKAT_Types.Filter NetKAT_Types.False, _ -> nc1
          | _, NetKAT_Types.Filter NetKAT_Types.False -> nc2 
          | _ -> NetKAT_Types.Seq(nc1,nc2) in 
      let mk_and pat1 pat2 = 
        match pat1,pat2 with 
          | NetKAT_Types.False,_ -> pat1
          | _,NetKAT_Types.False -> pat2
          | NetKAT_Types.True,_ -> pat2
          | _,NetKAT_Types.True -> pat1
          | _ -> NetKAT_Types.And(pat1,pat2) in 
      let mk_not pat = 
        match pat with 
          | NetKAT_Types.False -> NetKAT_Types.True
          | NetKAT_Types.True -> NetKAT_Types.False
          | _ -> NetKAT_Types.Neg(pat) in 
      let rec loop p = 
        if Atom.Map.is_empty p then 
          NetKAT_Types.Filter NetKAT_Types.False
        else
          let r,s = Atom.Map.min_binding p in
          let p' = Atom.Map.remove r p in 
          let (xs,x) = r in 
          let nc_pred = mk_and (mk_not (Pattern.set_to_netkat xs)) (Pattern.to_netkat x) in 
          let nc_pred_acts = mk_seq (NetKAT_Types.Filter nc_pred) (Action.set_to_netkat s) in 
          mk_par nc_pred_acts (loop p') in 
      loop p
  end

  module RunTime = struct

    type i = Local.t
        
    let compile (pol:NetKAT_Types.policy) : i = 
      Local.of_policy pol

    let decompile (p:i) : NetKAT_Types.policy = 
      Local.to_netkat p

    let simpl_flow (p : SDN_Types.pattern) (a : SDN_Types.group) : SDN_Types.flow = {
      SDN_Types.pattern = p;
      SDN_Types.action = a;
      SDN_Types.cookie = 0L;
      SDN_Types.idle_timeout = SDN_Types.Permanent;
      SDN_Types.hard_timeout = SDN_Types.Permanent
    }

      (* Prunes out rules that apply to other switches. *)
    let to_table (sw:SDN_Types.fieldVal) (p:i) : SDN_Types.flowTable =
      let add_flow x s l = 
        match Pattern.to_pattern sw x with
          | None -> l
          | Some pat -> simpl_flow pat [Action.set_to_action s] :: l in 
      let rec loop (p:i) acc cover = 
        if Atom.Map.is_empty p then 
          acc 
        else
          let r,s = Atom.Map.min_binding p in 
          let (xs,x) = r in 
          let p' = Atom.Map.remove r p in 
          let ys = Pattern.Set.diff xs cover in 
          let acc' = Pattern.Set.fold (fun x acc -> add_flow x Action.Set.empty acc) ys acc in 
          let acc'' = add_flow x s acc' in 
          let cover' = Pattern.Set.add x (Pattern.Set.union xs cover) in 
          loop p' acc'' cover' in 
      List.rev (loop p [] Pattern.Set.empty)
  end
end
