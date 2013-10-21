(* metavariable conventions
   - a, b, c, actions
   - s, t, u, action sets
   - x, y, z, patterns
   - xs, ys, zs, pattern sets
   - p, q, local
   - r, atoms
*)

module type S = sig
  type policy
  type pred
  type header_val_map

  module Action : sig
    type t = header_val_map
    module Set : Set.S with type elt = t
    type group = Set.t list 
    val to_string : t -> string
    val set_to_string : Set.t -> string
    val group_to_string : group -> string 
    val group_compare : group -> group -> int
    val group_crossproduct : group -> group -> group
    val group_union : group -> group -> group
    val seq_acts : t -> Set.t -> Set.t
    val id : Set.t
    val drop : Set.t
    val group_to_netkat : group -> policy
  end

  module Pattern : sig
    type t = header_val_map
    module Set : Set.S with type elt = t
    val set_to_string : Set.t -> string
    val to_string : t -> string
    val tru : t
    val is_tru : t -> bool
    val seq_pat : t -> t -> t option
    val seq_act_pat : t -> Action.t -> t -> t option
    val set_to_netkat : Set.t -> pred
    val to_netkat : t -> pred
  end

  module Atom : sig
    type t = Pattern.Set.t * Pattern.t
    module Map : Map.S with type key = t
    module Set : Set.S with type elt = t
    val to_string : t -> string
    val set_to_string : Set.t -> string
    val seq_atom : t -> t -> t option
    val seq_act_atom : t -> Action.t -> t -> t option
    val diff_atom: t -> t -> Set.t
    val tru : t
    val fls : t
  end 

  module Local : sig
    type t = Action.group Atom.Map.t
    val of_policy : policy -> t
    val to_netkat : t -> policy
  end
end

module Make 
  (Headers : Semantics.HEADERS) 
  (Syntax : Semantics.S with type header = Headers.header
                         and type header_val = Headers.value
                         and type payload = Headers.payload) : S
  with type policy = Syntax.policy
   and type pred = Syntax.pred
   and type header_val_map = Syntax.header_val_map = struct
     type policy = Syntax.policy
   type pred = Syntax.pred
   type header_val_map = Syntax.header_val_map

  (* utility function *)
  let header_val_map_to_string eq sep m =
    Syntax.HeaderMap.fold
      (fun h v acc ->
        Printf.sprintf "%s%s%s%s"
    (Headers.header_to_string h)
    eq
    (Headers.value_to_string v)
    (if acc = "" then "" else sep ^ acc))
      m ""

  module Action = struct
    type t = Syntax.header_val_map 

    module Set = Set.Make (struct
      type t = Syntax.header_val_map

      let compare = Syntax.HeaderMap.compare Pervasives.compare
    end)

    module Hash = Hashtbl.Make (struct
      type t = Set.t
      let hash = Hashtbl.hash
      let equal s1 s2 = Set.compare s1 s2 = 0
    end)

    type group = Set.t list 

    let to_string (a:t) : string =
      if Syntax.HeaderMap.is_empty a then "id"
      else Printf.sprintf "%s" (header_val_map_to_string ":=" ", " a)
        
    let set_to_string (s:Set.t) : string =
      Printf.sprintf "{%s}"
        (Set.fold
           (fun a acc -> (if acc = "" then "" else acc ^ ", ") ^ to_string a)
           s "")

    let group_to_string (g:group) : string = 
      Printf.sprintf "[%s]"
        (List.fold_left 
           (fun acc s -> set_to_string s ^ (if acc = "" then "" else "; " ^ acc))
           "" g)

    let mk_group (g:group) : group = 
      let h = Hash.create 17 in 
      List.rev 
	(List.fold_left 
	   (fun acc si -> 
	     if Hash.mem h si then 
	       acc
	     else 
	       begin
		 Hash.add h si ();
		 si::acc
	       end)
	   [] g)

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
      mk_group (g1 @ g2) 
 
    let id : Set.t = 
      Set.singleton (Syntax.HeaderMap.empty)

    let drop : Set.t = 
      Set.empty

    let seq_act (a:t) (b:t) : t =
      let f h vo1 vo2 = match vo1, vo2 with
        | (_, Some v2) ->
          Some v2
        | _ -> 
          vo1 in
      Syntax.HeaderMap.merge f a b

    let seq_acts (a:t) (s:Set.t) : Set.t = 
      Set.fold 
        (fun b acc -> Set.add (seq_act a b) acc) 
        s Set.empty

    let seq_group (a:t) (g:group) : group = 
      List.rev 
        (List.fold_left 
           (fun acc si -> seq_acts a si::acc)
           [] g)

    let to_netkat (a:t) : Syntax.policy =
      if Syntax.HeaderMap.is_empty a then
        Syntax.Filter Syntax.True
      else
        let f h v pol' = Syntax.Seq (pol', Syntax.Mod (h, v)) in
        let (h, v) = Syntax.HeaderMap.min_binding a in
        let a' = Syntax.HeaderMap.remove h a in
        Syntax.HeaderMap.fold f a' (Syntax.Mod  (h, v))
          
    let set_to_netkat (s:Set.t) : Syntax.policy =
      if Set.is_empty s then
        Syntax.Filter Syntax.False
      else
        let f a pol' = Syntax.Par (pol', to_netkat a) in
        let a = Set.min_elt s in
        let s' = Set.remove a s in
        Set.fold f s' (to_netkat a)

    let group_to_netkat (g:group) : Syntax.policy =
      match g with 
        | [] -> 
          Syntax.Filter Syntax.False
        | [s] -> 
          set_to_netkat s
        | s::g' -> 
          let f pol' s = Syntax.Choice (set_to_netkat s, pol') in
          List.fold_left f (set_to_netkat s) g'
  end

  module Pattern = struct
    exception Empty_pat

    type t = Syntax.header_val_map 

    module Set = Set.Make(struct
      type t = Syntax.header_val_map 
          
      let compare = Syntax.HeaderMap.compare Pervasives.compare
    end)

    let to_string (x:t) : string =
      if Syntax.HeaderMap.is_empty x then "true"
      else Printf.sprintf "%s" (header_val_map_to_string "=" ", " x)
        
    let set_to_string (xs:Set.t) : string =
      Printf.sprintf "{%s}"
        (Set.fold
           (fun x acc -> (if acc = "" then "" else acc ^ ", ") ^ to_string x)
           xs "")

    let tru : t = Syntax.HeaderMap.empty

    let is_tru (x:t) : bool = 
      Syntax.HeaderMap.is_empty x

    let matches (h:Syntax.header) (v:Syntax.header_val) (x:t) : bool =
      not (Syntax.HeaderMap.mem h x) 
      || Syntax.HeaderMap.find h x = v
        
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
        Some (Syntax.HeaderMap.merge f x y)
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
        | _, Some v2 -> 
          Some v2
        | None, None ->
          None in 
      let g h vo1 vo2 = Some (vo1, vo2) in 
      try
        Some (Syntax.HeaderMap.merge f 
                (Syntax.HeaderMap.merge g x a) y)
      with Empty_pat -> 
        None

    let to_netkat (x:t) : Syntax.pred =  
      if Syntax.HeaderMap.is_empty x then
        Syntax.True
      else
        let f h v pol' = Syntax.And (pol', Syntax.Test (h, v)) in
        let (h, v) = Syntax.HeaderMap.min_binding x in
        let x' = Syntax.HeaderMap.remove h x in
        (Syntax.HeaderMap.fold f x' (Syntax.Test (h, v)))

    let set_to_netkat (xs:Set.t) : Syntax.pred = 
      if Set.is_empty xs then 
        Syntax.False
      else
        let x = Set.choose xs in 
        let xs' = Set.remove x xs in 
        let f x pol = Syntax.Or(pol, to_netkat x) in 
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
          Syntax.HeaderMap.compare Pervasives.compare x1 x2 
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
		  if Syntax.HeaderMap.compare Pervasives.compare x x_xi = 0 then
		    raise Empty_atom
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

    let extend (op:Action.group -> Action.group -> Action.group) (r:Atom.t) (g:Action.group) (p:t) : t = 
      match Atom.mk r with
        | None -> 
          p
        | Some (xs,x) -> 
          if Pattern.Set.mem Pattern.tru xs then 
	    p
          else if Atom.Map.mem r p then
            let g_old = Atom.Map.find r p in
            Atom.Map.add r (op g_old g) p
          else
            Atom.Map.add r g p

    let rec bin_local (op:Action.group -> Action.group -> Action.group) (p:t) (q:t) : t =
        Atom.Map.fold (fun ((xs1,x1) as r1) g1 acc ->
          Atom.Map.fold (fun ((xs2,x2) as r2) g2 acc ->
            match Atom.seq_atom r1 r2 with
              | None ->
		extend op r1 g1 (extend op r2 g2 acc) 
              | Some r1_seq_r2 ->
                let f gi = (fun ri acc -> extend op ri gi acc) in 
                let r1_diff_r2 = Atom.diff_atom r1 r2 in 
                let r2_diff_r1 = Atom.diff_atom r2 r1 in 
		extend op r1_seq_r2 (op g1 g2)
                  (Atom.Set.fold (f g1) r1_diff_r2
                     (Atom.Set.fold (f g2) r2_diff_r1 acc)))
            p acc)
          q Atom.Map.empty 

    let par_local (p:t) (q:t) : t = 
      let r = bin_local Action.group_crossproduct p q in 
      (* Printf.printf  *)
      (* 	"PAR_LOCAL\n%s\n%s\n%s\n\n" *)
      (* 	(to_string p) (to_string q) (to_string r); *)
      r

    let choice_local (p:t) (q:t) : t =
      let r = bin_local Action.group_union p q in 
      (* Printf.printf  *)
      (* 	"CHOICE_LOCAL\n%s\n%s\n%s\n\n" *)
      (* 	(to_string p) (to_string q) (to_string r); *)
      r
        
    (* TODO(jnf) this is a helper function; give it a different name? *)
    let seq_atom_act_local (r1:Atom.t) (a:Action.t) (q:t) (acc:t) : t = 
      Atom.Map.fold
        (fun r2 g2 acc ->
          match Atom.seq_act_atom r1 a r2 with
            | None ->
              acc
            | Some r12 ->
              extend Action.group_crossproduct r12 (Action.seq_group a g2) acc)
        q acc

    let seq_atom_acts_local_acc (r1:Atom.t) (s1:Action.Set.t) (q:t) (acc:t) : t = 
      Action.Set.fold
        (fun a acc -> seq_atom_act_local r1 a q acc)
        s1 acc

    let seq_local (p:t) (q:t) : t = 
      let r = 
	Atom.Map.fold
          (fun r1 g1 acc ->
            match g1 with
              | [] -> 
		assert false
              | [s1] when Action.Set.is_empty s1 -> 
		extend Action.group_crossproduct r1 [s1] acc
              | _ -> 
		List.fold_left
                  (fun acc si -> seq_atom_acts_local_acc r1 si q acc)
                  acc g1)
          p Atom.Map.empty in 
      (* Printf.printf  *)
      (* 	"SEQ_LOCAL\n%s\n%s\n%s\n\n" *)
      (* 	(to_string p) (to_string q) (to_string r); *)
      r
        
    (* precondition: t is a predicate *)
    let negate (p:t) : t = 
      Atom.Map.fold
        (fun r g acc -> 
          match g with 
            | [s] when Action.Set.is_empty s -> 
              Atom.Map.add r [Action.id] acc
            | _ -> 
              Atom.Map.add r [Action.drop] acc)
        p Atom.Map.empty

    let rec of_pred (pr:Syntax.pred) : t = 
      match pr with
        | Syntax.True ->
          Atom.Map.singleton Atom.tru [Action.id]
        | Syntax.False ->
          Atom.Map.singleton Atom.tru [Action.drop]
        | Syntax.Neg p ->
          negate (of_pred p)
        | Syntax.Test (h, v) ->
          let p = Syntax.HeaderMap.singleton h v in
          Atom.Map.add (Pattern.Set.empty, p) [Action.id]
            (Atom.Map.singleton (Pattern.Set.singleton p, Pattern.tru) [Action.drop])
        | Syntax.And (pr1, pr2) ->
          seq_local (of_pred pr1) (of_pred pr2)
        | Syntax.Or (pr1, pr2) ->
          par_local (of_pred pr1) (of_pred pr2)
      
    let star_local (p:t) : t =
      let rec loop acc = 
        let seq' = seq_local acc p in
        let acc' = par_local acc seq' in
        if Atom.Map.compare Action.group_compare acc acc' = 0 then
          acc
        else 
          loop acc' in
      loop (Atom.Map.singleton Atom.tru [Action.id])

    let rec of_policy (pol:Syntax.policy) : t = 
      match pol with
        | Syntax.Filter pr ->
          of_pred pr
        | Syntax.Mod (h, v) ->
          Atom.Map.singleton Atom.tru [Action.Set.singleton (Syntax.HeaderMap.singleton h v)]
        | Syntax.Par (pol1, pol2) ->
          par_local (of_policy pol1) (of_policy pol2)
        | Syntax.Choice (pol1, pol2) -> 
          choice_local (of_policy pol1) (of_policy pol2)
        | Syntax.Seq (pol1, pol2) ->
          seq_local (of_policy pol1) (of_policy pol2)
        | Syntax.Star pol -> 
          star_local (of_policy pol)

    let to_netkat (p:t) : Syntax.policy = 
      (* "smart" constructors *)
      let mk_par nc1 nc2 = 
        match nc1, nc2 with 
          | Syntax.Filter Syntax.False, _ -> nc2
          | _, Syntax.Filter Syntax.False -> nc1
          | _ -> Syntax.Par(nc1,nc2) in       
      let mk_seq nc1 nc2 = 
        match nc1, nc2 with
          | Syntax.Filter Syntax.True, _ -> nc2
          | _, Syntax.Filter Syntax.True -> nc1
          | Syntax.Filter Syntax.False, _ -> nc1
          | _, Syntax.Filter Syntax.False -> nc2 
          | _ -> Syntax.Seq(nc1,nc2) in 
      let mk_and pat1 pat2 = 
        match pat1,pat2 with 
          | Syntax.False,_ -> pat1
          | _,Syntax.False -> pat2
          | Syntax.True,_ -> pat2
          | _,Syntax.True -> pat1
          | _ -> Syntax.And(pat1,pat2) in 
      let mk_not pat = 
        match pat with 
          | Syntax.False -> Syntax.True
          | Syntax.True -> Syntax.False
          | _ -> Syntax.Neg(pat) in 
      let rec loop p = 
        if Atom.Map.is_empty p then 
          Syntax.Filter Syntax.False
        else
          let r,g = Atom.Map.min_binding p in
          let p' = Atom.Map.remove r p in 
          let (xs,x) = r in 
          let nc_pred = mk_and (mk_not (Pattern.set_to_netkat xs)) (Pattern.to_netkat x) in 
          let nc_pred_acts = mk_seq (Syntax.Filter nc_pred) (Action.group_to_netkat g) in 
          mk_par nc_pred_acts (loop p') in 
      loop p

  end
end

module Compiler = Make (SDN_Headers) (NetKAT_Types)
module Local = Compiler.Local
module Action = Compiler.Action
module Pattern = Compiler.Pattern
module Atom = Compiler.Atom

module RunTime = struct

  let to_action (a:Action.t) : SDN_Types.seq =
    if not (NetKAT_Types.HeaderMap.mem (SDN_Headers.Header SDN_Types.InPort) a) then
      []
    else
      let port = NetKAT_Types.HeaderMap.find (SDN_Headers.Header SDN_Types.InPort) a in  
      let mods = NetKAT_Types.HeaderMap.remove (SDN_Headers.Header SDN_Types.InPort) a in
      let mk_mod h v act = 
        match h with
          | SDN_Headers.Switch -> raise (Invalid_argument "Action.to_action got switch update")
          | SDN_Headers.Header h' ->  (SDN_Types.SetField (h', v)) :: act in
      NetKAT_Types.HeaderMap.fold mk_mod mods [SDN_Types.OutputPort port]
        
  let set_to_action (s:Action.Set.t) : SDN_Types.par =
    let f a par = (to_action a)::par in
    Action.Set.fold f s []

  let group_to_action (g:Action.group) : SDN_Types.group = 
    List.map set_to_action g

  let to_pattern (sw : SDN_Types.fieldVal) (x:Pattern.t) : SDN_Types.pattern option =
    let f (h : NetKAT_Types.header) (v : NetKAT_Types.header_val) (pat : SDN_Types.pattern) =
      match h with
        | SDN_Headers.Switch -> pat (* already tested for this *)
        | SDN_Headers.Header h' -> SDN_Types.FieldMap.add h' v pat in
    if NetKAT_Types.HeaderMap.mem SDN_Headers.Switch x &&
      NetKAT_Types.HeaderMap.find SDN_Headers.Switch x <> sw then
      None
    else 
      Some (NetKAT_Types.HeaderMap.fold f x SDN_Types.FieldMap.empty)

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
    let add_flow x g l = 
      match to_pattern sw x with
        | None -> 
          l
        | Some pat -> 
          simpl_flow pat (group_to_action g) :: l in 
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
