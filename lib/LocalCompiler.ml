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
    val set_to_string : Set.t -> string
    val seq_acts : t -> Set.t -> Set.t
    val id : Set.t
    val drop : Set.t
    val set_to_netkat : Set.t -> policy
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
    val to_string : t -> string
    val seq_atom : t -> t -> t option
    val seq_act_atom : t -> Action.t -> t -> t option
    val tru : t
    val fls : t
  end 

  module Local : sig
    type t = Action.Set.t Atom.Map.t
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
    (if acc = "" then "" else acc ^ sep)
    (Headers.header_to_string h)
    eq
    (Headers.value_to_string v))
      m ""

  module Action = struct
    type t = Syntax.header_val_map 

    module Set = Set.Make (struct
      type t = Syntax.header_val_map

      let compare = Syntax.HeaderMap.compare Pervasives.compare
    end)

    let to_string (a:t) : string =
      if Syntax.HeaderMap.is_empty a then "id"
      else Printf.sprintf "%s" (header_val_map_to_string ":=" ", " a)
        
    let set_to_string (s:Set.t) : string =
      Printf.sprintf "{%s}"
        (Set.fold
           (fun a acc -> to_string a ^ if acc = "" then "" else ", " ^ acc)
           s "")
        
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

    let to_netkat (pol:t) : Syntax.policy =
      if Syntax.HeaderMap.is_empty pol then
        Syntax.Filter Syntax.True
      else
        let f h v pol' = Syntax.Seq (pol', Syntax.Mod (h, v)) in
        let (h, v) = Syntax.HeaderMap.min_binding pol in
        let pol' = Syntax.HeaderMap.remove h pol in
        Syntax.HeaderMap.fold f pol' (Syntax.Mod  (h, v))
          
    let set_to_netkat (pol:Set.t) : Syntax.policy =
      if Set.is_empty pol then
        Syntax.Filter Syntax.False
      else
        let f seq pol' = Syntax.Par (pol', to_netkat seq) in
        let seq = Set.min_elt pol in
        let pol' = Set.remove seq pol in
        Set.fold f pol' (to_netkat seq)
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
           (fun x acc -> to_string x ^ if acc = "" then "" else ", " ^ acc)
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
    type t = Pattern.Set.t * Pattern.t

    module Map = Map.Make (struct
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
    end)

    let to_string ((xs,x):t) : string = 
      Printf.sprintf "%s,%s" 
        (Pattern.set_to_string xs) (Pattern.to_string x)

    let tru : t = 
      (Pattern.Set.empty, Pattern.tru)

    let fls : t = 
      (Pattern.Set.singleton Pattern.tru, Pattern.tru) 

    (* "smart" constructor *)
    let mk ((xs,x):t) : t option = 
      if Pattern.Set.mem x xs then 
        None
      else 
        Some (xs,x)
        
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
              (fun x2i acc -> 
                match Pattern.seq_act_pat Pattern.tru a x2i with
                  | Some truax2i -> 
                    Pattern.Set.add truax2i acc
                  | None -> 
                    acc)
              xs2 xs1 in 
          mk (xs, x1ax2)
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

    (* extend r s t = (r --> s) U t *)
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

    let rec of_pred (pr:Syntax.pred) : t = 
      match pr with
        | Syntax.True ->
          Atom.Map.singleton Atom.tru Action.id
        | Syntax.False ->
          Atom.Map.singleton Atom.tru Action.drop
        | Syntax.Neg p ->
          negate (of_pred p)
        | Syntax.Test (h, v) ->
          let p = Syntax.HeaderMap.singleton h v in 
          Atom.Map.add (Pattern.Set.empty, p) Action.id
            (Atom.Map.singleton (Pattern.Set.singleton p, Pattern.tru) Action.drop)
        | Syntax.And (pr1, pr2) ->
          seq_local (of_pred pr1) (of_pred pr2)
        | Syntax.Or (pr1, pr2) ->
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

    let rec of_policy (pol:Syntax.policy) : t = 
      match pol with
        | Syntax.Filter pr ->
          of_pred pr
        | Syntax.Mod (h, v) ->
          Atom.Map.singleton Atom.tru (Action.Set.singleton (Syntax.HeaderMap.singleton h v))
        | Syntax.Par (pol1, pol2) ->
          par_local (of_policy pol1) (of_policy pol2)
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
          let r,s = Atom.Map.min_binding p in
          let p' = Atom.Map.remove r p in 
          let (xs,x) = r in 
          let nc_pred = mk_and (mk_not (Pattern.set_to_netkat xs)) (Pattern.to_netkat x) in 
          let nc_pred_acts = mk_seq (Syntax.Filter nc_pred) (Action.set_to_netkat s) in 
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
    let add_flow x s l = 
      match to_pattern sw x with
        | None -> l
        | Some pat -> simpl_flow pat [set_to_action s] :: l in 
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
