(* metavariable conventions
   - a, b, c, actions
   - s, t, u, action sets
   - x, y, z, patterns
   - xs, ys, zs, pattern sets
   - p, q, local
   - r, atoms
   - g, groups
*)

(* utility function *)
let header_val_map_to_string eq sep m =
  Types.HeaderMap.fold
    (fun h v acc ->
      Printf.sprintf "%s%s%s%s"
	(Pretty.header_to_string h)
	eq
	(Pretty.value_to_string v)
	(if acc = "" then "" else sep ^ acc))
    m ""

module Action = struct
  type t = Types.header_val_map

  type this_t = t

  module Set = Set.Make (struct
    type t = this_t

    let compare (a1:t) (a2:t) =
      Types.HeaderMap.compare Pervasives.compare a1 a2
  end)

  type group = Set.t list

  let to_string (a:t) : string =
    if Types.HeaderMap.is_empty a then "id"
    else Printf.sprintf "%s" (header_val_map_to_string ":=" ", " a)

  let set_to_string (s:Set.t) : string =
    Printf.sprintf "{%s}"
      (Set.fold
         (fun a acc -> (if acc = "" then "" else acc ^ ", ") ^ to_string a)
         s "")

  let group_to_string (g:group) : string =
    Printf.sprintf "[%s]"
      (List.fold_left
         (fun acc s -> 
           (if acc = "" then "" else acc ^ "; ") ^ set_to_string s)
         "" g)

  let mk_group (g:group) : group =
    let r = 
      List.rev
	(List.fold_left
	   (fun acc si ->
	     if List.exists (Set.equal si) acc then 
	       acc
	     else
	       si::acc)
	   [] g) in 
    (* Printf.printf "MK_GROUP\n%s\n%s\n\n" *)
    (*   (group_to_string g) *)
    (*   (group_to_string r); *)
    r

  (* TODO(jnf): surely this is a library function? *)
  let rec group_compare (g1:group) (g2:group) : int =
    match g1,g2 with
      | [],[] -> 0
      | [],_ -> -1
      | _,[] -> 1
      | s1::h1,s2::h2 ->
        let cmp = Set.compare s1 s2 in
        if cmp <> 0 then cmp
        else group_compare h1 h2

  let group_crossproduct (g1:group) (g2:group) : group =
    mk_group
      (List.rev
         (List.fold_left
            (fun acc s1i ->
	      List.fold_left
		(fun acc s2j ->
                  Set.union s1i s2j::acc)
		acc g2)
            [] g1))

  let group_union (g1:group) (g2:group) : group =
    let r = mk_group (g1 @ g2) in 
    (* Printf.printf "GROUP_UNION\n%s\n%s\n%s\n\n"  *)
    (*   (group_to_string g1) (group_to_string g2) (group_to_string r); *)
    r

  let id : Set.t =
    Set.singleton (Types.HeaderMap.empty)

  let drop : Set.t =
    Set.empty

  let is_id (s:Set.t) : bool =
    Set.cardinal s = 1 &&
    (Types.HeaderMap.is_empty (Set.choose s))

  let is_drop (s:Set.t) : bool =
    Set.is_empty s

  let seq_act (a1:t) (a2:t) : t =
    let f h vo1 vo2 = match vo1, vo2 with
      | (_, Some v2) ->
        Some v2
      | _ ->
        vo1 in
    Types.HeaderMap.merge f a1 a2

  let seq_acts (a:t) (s:Set.t) : Set.t =
    Set.fold
      (fun b acc -> Set.add (seq_act a b) acc)
      s Set.empty

  let seq_group (a:t) (g:group) : group =
    List.rev
      (List.fold_left
         (fun acc si -> seq_acts a si::acc)
         [] g)

  let to_netkat (a:t) : Types.policy =
    let h_port = Types.Header SDN_Types.InPort in 
    if Types.HeaderMap.is_empty a then 
      Types.Filter Types.True
    else 
      let f h v pol' = 
	if h = h_port then 
	  Types.Seq (pol', Types.Mod (h, v)) 
	else 
	  Types.Seq (Types.Mod (h, v), pol') in
      let (h, v) = Types.HeaderMap.min_binding a in
      let a' = Types.HeaderMap.remove h a in
      Types.HeaderMap.fold f a' (Types.Mod (h, v))
	
  let set_to_netkat (s:Set.t) : Types.policy =
    if Set.is_empty s then
      Types.Filter Types.False
    else
      let f a pol' = Types.Par (pol', to_netkat a) in
      let a = Set.min_elt s in
      let s' = Set.remove a s in
      Set.fold f s' (to_netkat a)

  let group_to_netkat (g:group) : Types.policy =
    match g with
      | [] ->
        Types.Filter Types.False
      | [s] ->
        set_to_netkat s
      | s::g' ->
        let f pol' s = Types.Choice (pol', set_to_netkat s) in
        List.fold_left f (set_to_netkat s) g'
end

module Pattern = struct
  exception Empty_pat

  type t = Types.header_val_map

  module Set = Set.Make(struct
    type t = Types.header_val_map

    let compare = Types.HeaderMap.compare Pervasives.compare
  end)

  let to_string (x:t) : string =
    if Types.HeaderMap.is_empty x then "true"
    else Printf.sprintf "<%s>" (header_val_map_to_string "=" ", " x)

  let set_to_string (xs:Set.t) : string =
    Printf.sprintf "{%s}"
      (Set.fold
         (fun x acc -> (if acc = "" then "" else acc ^ ", ") ^ to_string x)
         xs "")

  let tru : t = Types.HeaderMap.empty

  let is_tru (x:t) : bool =
    Types.HeaderMap.is_empty x

  let matches (h:Types.header) (v:Types.header_val) (x:t) : bool =
    not (Types.HeaderMap.mem h x)
    || Types.HeaderMap.find h x = v
    
  let subseteq_pat (x:t) (y:t) : bool = 
    let f h vo1 vo2 = match vo1,vo2 with 
      | Some v1, Some v2 -> 
	if v1 <> v2 then raise Empty_pat else Some ()
      | Some v1, None -> 
	Some ()
      | None, Some v1 -> 
	raise Empty_pat
      | None, None -> 
	Some () in 
    try 
      let _ = Types.HeaderMap.merge f x y in 
      true
    with Empty_pat -> 
      false
	
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
      Some (Types.HeaderMap.merge f x y)
    with Empty_pat ->
      None

  let rec seq_act_pat (x:t) (a:Action.t) (y:t) : t option =
    let f h vo1 vo2 = match vo1, vo2 with
      | Some (vo11, Some v12), Some v2 ->
        if v12 <> v2 then raise Empty_pat
        else vo11
      | Some (vo11, Some v12), None ->
        vo11
      | Some (Some v11, None), Some v2 ->
        if v11 <> v2 then raise Empty_pat
        else Some v11
      | Some (vo11, None), None ->
        vo11
      | Some(None,None), Some v2
      | None, Some v2 ->
        Some v2
      | None, None ->
        None in
    let g h vo1 vo2 = Some (vo1, vo2) in
    try
      Some (Types.HeaderMap.merge f
              (Types.HeaderMap.merge g x a) y)
    with Empty_pat ->
      None

  let to_netkat (x:t) : Types.pred =
    if Types.HeaderMap.is_empty x then
      Types.True
    else
      let f h v pol' = Types.And (pol', Types.Test (h, v)) in
      let (h, v) = Types.HeaderMap.min_binding x in
      let x' = Types.HeaderMap.remove h x in
      (Types.HeaderMap.fold f x' (Types.Test (h, v)))

  let set_to_netkat (xs:Set.t) : Types.pred =
    if Set.is_empty xs then
      Types.False
    else
      let x = Set.choose xs in
      let xs' = Set.remove x xs in
      let f x pol = Types.Or(pol, to_netkat x) in
      Set.fold f xs' (to_netkat x)
end

module Atom = struct
  exception Empty_atom

  type t = Pattern.Set.t * Pattern.t

  let compare (xs1,x1) (xs2,x2) =
    if Pattern.Set.mem x2 xs1 then 1
    else if Pattern.Set.mem x1 xs2 then -1
    else
      let cmp = Pattern.Set.compare xs1 xs2 in
      if cmp = 0 then
        Types.HeaderMap.compare Pervasives.compare x1 x2
      else
        cmp

  module Set = Set.Make (struct
    type t = Pattern.Set.t * Pattern.t

    let compare = compare
  end)


  module Map = Map.Make (struct
    type t = Pattern.Set.t * Pattern.t

    let compare = compare
  end)

  let to_string ((xs,x):t) : string =
    Printf.sprintf "%s,%s"
      (Pattern.set_to_string xs) (Pattern.to_string x)

  let set_to_string (rs:Set.t) : string =
    Printf.sprintf "{%s}"
      (Set.fold
         (fun ri acc -> (if acc = "" then acc else acc ^ ", ") ^ to_string ri)
         rs "")

  let tru : t =
    (Pattern.Set.empty, Pattern.tru)

  let fls : t =
    (Pattern.Set.singleton Pattern.tru, Pattern.tru)

    (* "smart" constructor *)
  let mk ((xs,x):t) : t option =
    try
      let xs' =
	Pattern.Set.fold
	  (fun xi acc ->
	    match Pattern.seq_pat x xi with
	      | None ->
		acc
	      | Some x_xi ->
		if Types.HeaderMap.compare Pervasives.compare x x_xi = 0 then
		  raise Empty_atom
		else if 
		    Pattern.Set.exists 
		      (fun xj -> 
			Types.HeaderMap.compare Pervasives.compare xi xj <> 0 &&
			  Pattern.subseteq_pat xi xj) 
		      xs 
		then 
		  acc
		else
		  Pattern.Set.add xi acc)
	  xs Pattern.Set.empty in
      Some (xs',x)
    with Empty_atom ->
      None

  let seq_atom ((xs1,x1):t) ((xs2,x2):t) : t option =
    match Pattern.seq_pat x1 x2 with
      | Some x12 ->
        mk (Pattern.Set.union xs1 xs2, x12)
      | None ->
        None

  let seq_act_atom ((xs1,x1):t) (a:Action.t) ((xs2,x2):t) : t option =
    match Pattern.seq_act_pat x1 a x2 with
      | Some x1ax2 ->
        let xs =
          Pattern.Set.fold
            (fun xs2i acc ->
              match Pattern.seq_act_pat Pattern.tru a xs2i with
                | Some truaxs2i ->
                  Pattern.Set.add truaxs2i acc
                | None ->
                  acc)
            xs2 xs1 in
        mk (xs, x1ax2)
      | None ->
        None

  let diff_atom ((xs1,x1):t) ((xs2,x2):t) : Set.t =
    let acc0 =
      match mk (Pattern.Set.add x2 xs1, x1) with
        | None ->
	  Set.empty
        | Some r ->
	  Set.singleton r in
    Pattern.Set.fold
      (fun x2i acc ->
        match Pattern.seq_pat x1 x2i with
	  | None ->
	    acc
	  | Some x12i ->
            begin match mk (xs1, x12i) with
              | None ->
		acc
              | Some ri ->
		Set.add ri acc
	    end)
      xs2 acc0
end

module Local = struct
  type t = Action.group Atom.Map.t

  let to_string (p:t) : string =
    Atom.Map.fold
      (fun r g acc ->
        Printf.sprintf "%s(%s) => %s\n"
          (if acc = "" then "" else "" ^ acc)
          (Atom.to_string r) (Action.group_to_string g))
      p ""

  let extend (r:Atom.t) (g:Action.group) (p:t) : t =
    match g, Atom.mk r with 
      | [s],_ when Action.is_drop s -> p
      | _, None -> 
	p
      | _, Some (xs,x) ->
	if Atom.Map.mem r p then
          let msg = 
            Printf.sprintf "Local.extend: overlap on atom %s" (Atom.to_string r) in 
          failwith msg
        else
          Atom.Map.add r g p

  let intersect (op:Action.group -> Action.group -> Action.group) (p:t) (q:t) : t =
    if Atom.Map.is_empty p || Atom.Map.is_empty q then
      Atom.Map.empty
    else
      Atom.Map.fold (fun r1 g1 acc ->
        Atom.Map.fold (fun r2 g2 acc ->
          match Atom.seq_atom r1 r2 with
            | None ->
              acc
            | Some r1_seq_r2 ->
              extend r1_seq_r2 (op g1 g2) acc)
          q acc)
        p Atom.Map.empty

  let difference (p:t) (q:t) : t =
    if Atom.Map.is_empty q then
      p
    else
      Atom.Map.fold (fun r1 g1 acc ->
        let rs = Atom.Map.fold (fun r12i _ acc ->
          Atom.Set.fold (fun r1j acc ->
            Atom.Set.union (Atom.diff_atom r1j r12i) acc)
            acc Atom.Set.empty)
          q (Atom.Set.singleton r1) in
        Atom.Set.fold (fun r1i acc -> extend r1i g1 acc) rs Atom.Map.empty)
      p Atom.Map.empty

  let rec bin_local (op:Action.group -> Action.group -> Action.group) (p:t) (q:t) : t =
    if Atom.Map.is_empty p then 
      q
    else if Atom.Map.is_empty q then 
      p 
    else 
      let p_inter_q = intersect op p q in
      let p_only = difference p p_inter_q in
      let q_only = difference q p_inter_q in

      let f r vo1 vo2 = 
        match vo1,vo2 with 
          | Some _, None -> vo1
          | None, Some _ -> vo2
          | Some _, Some _ -> 
            let msg = 
              Printf.sprintf "Local.bin_local: overlap on %s in bin_local"
                (Atom.to_string r) in 
            failwith msg
          | None, None -> None in 
      (* Printf.printf "BIN_LOCAL\nP\n%s\nQ\n%s\nP_INTER_Q\n%s\nP_ONLY\n%s\nQ_ONLY\n%s\n\n%!" *)
      (*   (to_string p) (to_string q) *)
      (*   (to_string p_inter_q) (to_string p_only) (to_string q_only); *)
      Atom.Map.merge f (Atom.Map.merge f p_only q_only) p_inter_q

  let par_local (p:t) (q:t) : t =
    let r = bin_local Action.group_crossproduct p q in
      (* Printf.printf *)
      (* 	"PAR_LOCAL\n%s\n%s\n%s\n\n" *)
      (* 	(to_string p) (to_string q) (to_string r); *)
    r

  let choice_local (p:t) (q:t) : t =
    let r = bin_local Action.group_union p q in
      (* Printf.printf *)
      (* 	"CHOICE_LOCAL\n%s\n%s\n%s\n\n" *)
      (* 	(to_string p) (to_string q) (to_string r); *)
    r

  let cross_merge _ g1o g2o =
    match g1o, g2o with
      | Some g1, None -> Some g1
      | None, Some g2 -> Some g2
      | Some g1, Some g2 -> Some (Action.group_crossproduct g1 g2)
      | None, None -> None 

  let union_merge _ g1o g2o =
    match g1o, g2o with
      | Some g1, None -> Some g1
      | None, Some g2 -> Some g2
      | Some g1, Some g2 -> Some (Action.group_union g1 g2)
      | None, None -> None 
      
  let seq_atom_acts_local (r1:Atom.t) (s1:Action.Set.t) (q:t) : t =
    let seq_act (a:Action.t) : t =
      Atom.Map.fold
	(fun r2 g2 acc ->
          match Atom.seq_act_atom r1 a r2 with
            | None ->
              acc
            | Some r12 ->
              extend r12 (Action.seq_group a g2) acc)
	q Atom.Map.empty in 
    Action.Set.fold
      (fun a acc -> Atom.Map.merge cross_merge acc (seq_act a))
      s1 Atom.Map.empty
	  
  let seq_local (p:t) (q:t) : t =
    let r =
      Atom.Map.fold
        (fun r1 g1 acc ->
	  List.fold_left
            (fun acc si -> 
	      Atom.Map.merge union_merge acc (seq_atom_acts_local r1 si q))
            acc g1)
        p Atom.Map.empty in
      (* Printf.printf *)
      (* 	"SEQ_LOCAL\n%s\n%s\n%s\n\n" *)
      (* 	(to_string p) (to_string q) (to_string r); *)
    r

    (* precondition: t is a predicate *)
  let negate (p:t) : t =
    let rs = 
      Atom.Map.fold
        (fun r g acc ->
	  Atom.Set.fold
	    (fun ri acc -> Atom.Set.union (Atom.diff_atom ri r) acc)
	    acc Atom.Set.empty)
	p (Atom.Set.singleton Atom.tru) in 
    Atom.Set.fold 
      (fun ri acc -> extend ri [Action.id] acc) 
      rs Atom.Map.empty

  let rec of_pred (pr:Types.pred) : t =
    match pr with
      | Types.True ->
        Atom.Map.singleton Atom.tru [Action.id]
      | Types.False ->
        Atom.Map.empty
      | Types.Neg p ->
        negate (of_pred p)
      | Types.Test (h, v) ->
        let p = Types.HeaderMap.singleton h v in
        Atom.Map.singleton (Pattern.Set.empty, p) [Action.id]
      | Types.And (pr1, pr2) ->
        seq_local (of_pred pr1) (of_pred pr2)
      | Types.Or (pr1, pr2) ->
        par_local (of_pred pr1) (of_pred pr2)

  let star_local (p:t) : t =
    let rec loop acc pi =
      let psucci = seq_local p pi in
      let acc' = par_local acc psucci in
      if Atom.Map.compare Action.group_compare acc acc' = 0 then
        acc
      else
        loop acc' psucci in
    let p0 = Atom.Map.singleton Atom.tru [Action.id] in
    loop p0 p0

  let rec of_policy (pol:Types.policy) : t =
    match pol with
      | Types.Filter pr ->
        of_pred pr
      | Types.Mod (h, v) ->
        Atom.Map.singleton Atom.tru [Action.Set.singleton (Types.HeaderMap.singleton h v)]
      | Types.Par (pol1, pol2) ->
        par_local (of_policy pol1) (of_policy pol2)
      | Types.Choice (pol1, pol2) ->
        choice_local (of_policy pol1) (of_policy pol2)
      | Types.Seq (pol1, pol2) ->
        seq_local (of_policy pol1) (of_policy pol2)
      | Types.Star pol ->
        star_local (of_policy pol)
      | Types.Link(sw,pt,sw',pt') ->
	failwith "Not a local policy"

  let to_netkat (p:t) : Types.policy =
    (* "smart" constructors *)
    let mk_par nc1 nc2 =
      match nc1, nc2 with
        | Types.Filter Types.False, _ -> nc2
        | _, Types.Filter Types.False -> nc1
        | _ -> Types.Par(nc1,nc2) in
    let mk_seq nc1 nc2 =
      match nc1, nc2 with
        | Types.Filter Types.True, _ -> nc2
        | _, Types.Filter Types.True -> nc1
        | Types.Filter Types.False, _ -> nc1
        | _, Types.Filter Types.False -> nc2
        | _ -> Types.Seq(nc1,nc2) in
    let mk_and pat1 pat2 =
      match pat1,pat2 with
        | Types.False,_ -> pat1
        | _,Types.False -> pat2
        | Types.True,_ -> pat2
        | _,Types.True -> pat1
        | _ -> Types.And(pat1,pat2) in
    let mk_not pat =
      match pat with
        | Types.False -> Types.True
        | Types.True -> Types.False
        | _ -> Types.Neg(pat) in
    let rec loop p =
      if Atom.Map.is_empty p then
        Types.Filter Types.False
      else
        let r,g = Atom.Map.min_binding p in
        let p' = Atom.Map.remove r p in
        let (xs,x) = r in
        let nc_pred = mk_and (mk_not (Pattern.set_to_netkat xs)) (Pattern.to_netkat x) in
        let nc_pred_acts = mk_seq (Types.Filter nc_pred) (Action.group_to_netkat g) in
        mk_par nc_pred_acts (loop p') in
    loop p

end

module RunTime = struct

  let to_action (a:Action.t) (pto: VInt.t option) : SDN_Types.seq =
    let port = 
      try 
        Types.HeaderMap.find (Types.Header SDN_Types.InPort) a 
      with Not_found -> 
        begin match pto with 
          | Some pt -> pt
          | None -> raise (Invalid_argument "Action.to_action: indeterminate port")
        end in 
    let mods = Types.HeaderMap.remove (Types.Header SDN_Types.InPort) a in
    let mk_mod h v act =
      match h with
        | Types.Switch -> 
	  raise (Invalid_argument "Action.to_action: got switch update")
        | Types.Header h' -> 
	  (SDN_Types.SetField (h', v)) :: act in
      Types.HeaderMap.fold mk_mod mods [SDN_Types.OutputPort port]  

  let set_to_action (s:Action.Set.t) (pto : VInt.t option) : SDN_Types.par =
    let f a par = (to_action a pto)::par in
    Action.Set.fold f s []

  let group_to_action (g:Action.group) (pto:VInt.t option) : SDN_Types.group =
    List.map (fun s -> set_to_action s pto) g

  let to_pattern (sw : SDN_Types.fieldVal) (x:Pattern.t) : SDN_Types.pattern option =
    let f (h : Types.header) (v : Types.header_val) (pat : SDN_Types.pattern) =
      match h with
        | Types.Switch -> pat (* already tested for this *)
        | Types.Header h' -> SDN_Types.FieldMap.add h' v pat in
    if Types.HeaderMap.mem Types.Switch x &&
      Types.HeaderMap.find Types.Switch x <> sw then
      None
    else
      Some (Types.HeaderMap.fold f x SDN_Types.FieldMap.empty)

  type i = Local.t

  let compile (pol:Types.policy) : i =
    Local.of_policy pol

  let decompile (p:i) : Types.policy =
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
    let add_flow x g l =
      let pto = 
        try 
          Some (Types.HeaderMap.find (Types.Header SDN_Types.InPort) x) 
        with Not_found -> 
          None in 
      match to_pattern sw x with
        | None ->
          l
        | Some pat ->
          simpl_flow pat (group_to_action g pto) :: l in
    let rec loop (p:i) acc cover =
      if Atom.Map.is_empty p then
        acc
      else
        let r,g = Atom.Map.min_binding p in
        let (xs,x) = r in
        let p' = Atom.Map.remove r p in
        let ys = Pattern.Set.diff xs cover in
        let acc' = Pattern.Set.fold (fun x acc -> add_flow x [Action.drop] acc) ys acc in
        let acc'' = add_flow x g acc' in
        let cover' = Pattern.Set.add x (Pattern.Set.union xs cover) in
        loop p' acc'' cover' in
    List.rev (loop p [] Pattern.Set.empty)
end
