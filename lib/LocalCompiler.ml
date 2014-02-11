open Core.Std
open Sexplib.Conv

let debug = Printf.printf

open SDN_Types
  
module type FIELDS = sig
  type t = (field * fieldVal) list with sexp
  module Set : Set.S with type Elt.t = t
  val to_string : ?init:string -> ?sep:string -> t -> string
  val set_to_string : ?init:string -> ?sep:string -> Set.t -> string
  val compare : t -> t -> int    
  val empty : t
  val mk : field -> fieldVal -> t
  val is_empty : t -> bool
  val seq : t -> t -> t option
  val diff : t -> t -> t
  val subseteq : t -> t -> bool
end

module Fields : FIELDS = struct

  type t = (field * fieldVal) list sexp_opaque with sexp
        
  let to_string ?init:(init="") ?sep:(sep="=") (x:t) : string =
    match x with 
      | [] -> 
        init
      | _ -> 
        List.fold x ~init:""
          ~f:(fun acc (f, v) ->
            Printf.sprintf "%s%s%s%s"
              (if acc = "" then "" else acc ^ ", ")
              (NetKAT_Pretty.string_of_field f)
              sep
              (NetKAT_Pretty.value_to_string v))

  type this_t = t with sexp

  let compare (x:t) (y:t) : int = 
    List.compare x y 
      ~cmp:(fun (f1,v1) (f2,v2) -> 
          let cmp = compare f1 f2 in 
          if cmp <> 0 then cmp 
          else Pervasives.compare v1 v2)

  module Set = Set.Make(struct
    type t = this_t with sexp
    let compare = compare
  end)

  let set_to_string ?init:(init="[]") ?sep:(sep="=") (s:Set.t) : string =
    Printf.sprintf "%s"
      (Set.fold s
         ~init:""
         ~f:(fun acc x -> 
           Printf.sprintf "%s%s"
             (if acc = "" then "" else acc ^ ", ")
             (to_string ~init:init ~sep:sep x)))

  let empty : t = 
    []

  let is_empty (x:t) : bool = 
    match x with 
      | [] -> true
      | _ -> false

  let mk (f:field) (v:fieldVal) : t = 
    [(f,v)]
      
  let rec subseteq (x:t) (y:t) : bool =  
    match x,y with 
      | _,[] -> true
      | [],_::_ -> false
      | (fx,vx)::xrest, (fy,vy)::yrest -> 
        let cmp = Pervasives.compare fx fy in 
        if cmp = 0 then 
          vx = vy && subseteq xrest yrest
        else if cmp < 0 then 
          subseteq xrest y
        else (* cmp > 0 *)
          false 

  (* module Memo = Hashtbl.Make(struct *)
  (*   type t = this_t * this_t with sexp *)
  (*   let compare (l11,l12) (l21,l22) =  *)
  (*     let cmp = compare l11 l21 in  *)
  (*     if cmp <> 0 then cmp  *)
  (*     else compare l12 l22   *)
  (*   let hash = Hashtbl.hash *)
  (* end) *)

  (* let memo = Memo.create () *)

  let map_option f = function
    | None -> None
    | Some x -> Some (f x)  

  let rec seq_loop x y k = 
    match x,y with 
      | _,[] -> 
        k (Some x)
      | [],_::_ -> 
        k (Some y)
      | (fx,vx)::xrest, (fy,vy)::yrest -> 
        let cmp = Pervasives.compare fx fy in 
        if cmp = 0 then 
          begin 
            if vx = vy then 
              seq_loop xrest yrest 
                (fun o -> k (map_option (fun l -> (fx,vx)::l) o))
            else 
              k None
          end
        else if cmp < 0 then 
          seq_loop xrest y 
            (fun o -> k (map_option (fun l -> (fx,vx)::l) o))
        else (* cmp > 0 *)
          seq_loop x yrest
            (fun o -> k (map_option (fun l -> (fy,vy)::l) o))  
      
  let rec seq (x:t) (y:t) : t option = 
    seq_loop x y (fun o -> o) 

  let diff (x:t) (y:t) : t = 
    let rec loop x y k = 
      match x,y with 
        | _,[] -> 
          k x
        | [],_::_ -> 
          k x
        | (fx,vx)::xrest, (fy,vy)::yrest -> 
          let cmp = Pervasives.compare fx fy in 
          if cmp = 0 then 
            loop xrest yrest k 
          else if cmp < 0 then 
            loop xrest y (fun l -> (fx,vx)::l)
          else (* cmp > 0 *)
            loop x yrest k in 
    loop x y (fun o -> o)
end

module type ACTION = sig
  type t = Fields.t
  module Set : Set.S with type Elt.t = t
  type group = Set.t list
  val to_string : t -> string
  val set_to_string : Set.t -> string
  val group_to_string : group -> string
  val mk : field -> fieldVal -> t
  val seq : t -> t -> t
  val set_seq : t -> Set.t -> Set.t
  val group_seq : t -> group -> group
  val diff : t -> t -> t
  val group_mk : Set.t -> group
  val set_compare : Set.t -> Set.t -> int
  val group_compare : group -> group -> int
  val group_union : group -> group -> group
  val group_cross : group -> group -> group
  val id : Set.t 
  val drop : Set.t 
  val is_id : Set.t -> bool
  val is_drop : Set.t -> bool
  val group_id : group
  val group_drop : group
  val group_is_id : group -> bool
  val group_is_drop : group -> bool
  val to_netkat : t -> NetKAT_Types.policy
  val set_to_netkat : Set.t -> NetKAT_Types.policy
  val group_to_netkat : group -> NetKAT_Types.policy
end

module Action : ACTION = struct

  type t = Fields.t with sexp

  type this_t = t with sexp

  module SetSet = Set.Make(Fields.Set)

  module Set = Fields.Set

  type group = Set.t list

  let compare = Fields.compare

  let set_compare = Set.compare

  let group_compare = List.compare ~cmp:set_compare

  let to_string : t -> string = 
    Fields.to_string ~init:"id" ~sep:":="

  let set_to_string (s:Set.t) : string = 
    if Set.is_empty s then "drop"
    else Fields.set_to_string ~init:"id" ~sep:":=" s

  let group_to_string (g:group) : string = 
    Printf.sprintf "[%s]"
      (List.fold g
         ~init:""
         ~f:(fun acc s -> 
           Printf.sprintf "%s%s"
             (if acc = "" then "" else acc ^ " + ")
             (set_to_string s)))

  let mk (f:field) (v:VInt.t) : t = 
    Fields.mk f v

  (* module Memo = Hashtbl.Make(struct *)
  (*   type t = this_t * this_t with sexp *)
  (*   let compare (l11,l12) (l21,l22) =  *)
  (*     let cmp = compare l11 l21 in  *)
  (*     if cmp <> 0 then cmp  *)
  (*     else compare l12 l22   *)
  (*   let hash = Hashtbl.hash *)
  (* end) *)

  (* let memo = Memo.create ()  *)

  let rec seq_loop a1 a2 k = 
    match a1,a2 with 
      | _,[] -> 
        k a1
      | [],_::_ -> 
        k a2
      | (f1,v1)::a1rest, (f2,v2)::a2rest -> 
        let cmp = Pervasives.compare f1 f2 in 
        if cmp = 0 then 
          seq_loop a1rest a2rest (fun a -> k ((f2,v2)::a))
        else if cmp < 0 then 
          seq_loop a1rest a2 (fun a -> k ((f1,v1)::a))
        else (* cmp > 0 *)
          seq_loop a1 a2rest (fun a -> k ((f2,v2)::a))               

  let seq a1 a2 = 
    seq_loop a1 a2 (fun a -> a) 

  let set_seq a s = 
    Set.map s (seq a)

  let group_seq a g = 
    List.map g ~f:(set_seq a)

  let diff : t -> t -> t = 
    Fields.diff

  let group_mk (s:Set.t) : group = 
    [s]

  let group_union (g1:group) (g2:group) : group = 
    let ss = 
      List.fold g2
        ~init:SetSet.empty
        ~f:SetSet.add in 
    let rec loop g ss k = 
      match g with 
        | [] -> k g2
        | s::grest -> 
          loop grest ss (fun l -> k (s::l)) in 
    loop g1 ss (fun l -> l)

  let group_cross (g1:group) (g2:group) : group = 
    fst (List.fold_right g1
           ~init:([],SetSet.empty)
           ~f:(fun s1i acc -> 
                 List.fold_right g2
                   ~init:acc
                   ~f:(fun s2j acc -> 
                         let g,ss = acc in 
                         let s1is2j = Set.union s1i s2j in 
                         if SetSet.mem ss s1is2j then acc
                         else (s1is2j::g, SetSet.add ss s1is2j))))

  let id : Set.t = 
    Set.singleton (Fields.empty)

  let drop : Set.t = 
    Set.empty

  let is_id (s:Set.t) : bool = 
    Set.length s = 1 && 
    match Set.min_elt s with 
      | None -> false
      | Some a -> Fields.is_empty a

  let is_drop (s:Set.t) : bool = 
    Set.is_empty s 

  let group_id : group = 
    [id]
      
  let group_drop : group = 
    [drop]

  let group_is_id (g:group) : bool = 
    match g with 
      | [s] -> is_id s
      | _ -> false

  let group_is_drop (g:group) : bool = 
    match g with 
      | [s] -> is_drop s
      | _ -> false  

  let to_netkat (a:t) : NetKAT_Types.policy =
    let f pol (f,v)  = 
      let pol' = NetKAT_Types.Mod (NetKAT_Types.Header f, v) in 
      if f =  InPort then 
	NetKAT_Types.Seq (pol, pol') 
      else 
	NetKAT_Types.Seq (pol', pol) in
    match a with 
      | [] -> 
        NetKAT_Types.Filter NetKAT_Types.True
      | (h,v)::arest -> 
        List.fold arest
          ~init:(NetKAT_Types.Mod(NetKAT_Types.Header h,v))
          ~f:f

  let set_to_netkat (s:Set.t) : NetKAT_Types.policy =
    if Set.is_empty s then
      NetKAT_Types.Filter NetKAT_Types.False
    else
      let f pol a = NetKAT_Types.Union (pol, to_netkat a) in
      let a = Set.min_elt_exn s in
      let s' = Set.remove s a in
      Set.fold s' ~f:f ~init:(to_netkat a)

  let group_to_netkat (g:group) : NetKAT_Types.policy =
    match g with
      | [] ->
        NetKAT_Types.Filter NetKAT_Types.False
      | [s] ->
        set_to_netkat s
      | s::g' ->
        let f pol' s = NetKAT_Types.Union (pol', set_to_netkat s) in
        List.fold g' ~init:(set_to_netkat s) ~f:f
end

module type PATTERN = sig
  type t = Fields.t
  module Set : Set.S with type Elt.t = t
  val to_string : t -> string
  val set_to_string : Set.t -> string
  val compare : t -> t -> int
  val mk : field -> fieldVal -> t
  val seq : t -> t -> t option
  val seq_act : t -> Action.t -> t -> t option
  val diff : t -> t -> t
  val subseteq : t -> t -> bool
  val tru : t
  val to_netkat : t -> NetKAT_Types.pred
  val set_to_netkat : Set.t -> NetKAT_Types.pred
end

module Pattern : PATTERN = struct

  type t = Fields.t

  module Set = Fields.Set

  let to_string : t -> string = 
    Fields.to_string ~init:"true" ~sep:"="

  let set_to_string (xs:Set.t) : string = 
    Printf.sprintf "{%s}"
      (Fields.set_to_string ~init:"true" ~sep:"=" xs)

  let compare : t -> t -> int = 
    Fields.compare

  let mk (f:field) (v:VInt.t) = 
    Fields.mk f v

  let seq : t -> t -> t option = 
    Fields.seq

  let seq_act x a y = 
    (* TODO(jnf): can optimize into a single loop *)
    (* Printf.printf "  SEQ_ACT\n  X=%s\n  A=%s\n  Y=%s\n  " *)
    (*   (to_string x) *)
    (*   (Action.to_string a) *)
    (*   (to_string y); *)
    match Fields.seq a y with 
      | None -> 
        (* Printf.printf "Z=None\n"; *)
        None
      | Some z -> 
        (* Printf.printf "Z=Some (%s)\n  " (to_string z); *)
        (* Printf.printf "D=%s\n  " (to_string (Fields.diff z a)); *)
        (* Printf.printf "R=%s\n" (match (Fields.seq x (Fields.diff z a)) with None -> "None" | Some r -> to_string r); *)
        Fields.seq x (Fields.diff z a)
      
  let diff : t -> t -> t = 
    Fields.diff

  let subseteq : t -> t -> bool = 
    Fields.subseteq
      
  let tru : t = 
    Fields.empty

  let to_netkat (x:t) : NetKAT_Types.pred =
    let rec loop x k = 
      match x with 
        | [] -> 
          k NetKAT_Types.True
        | [(f,v)] -> 
          k (NetKAT_Types.Test (NetKAT_Types.Header f,v))
        | (f,v)::x1 -> 
          loop x1 (fun pr -> NetKAT_Types.And(NetKAT_Types.Test(NetKAT_Types.Header f,v),pr)) in 
    loop x (fun x -> x)

  let set_to_netkat (xs:Set.t) : NetKAT_Types.pred =
    match Set.choose xs with 
      | None -> 
        NetKAT_Types.False
      | Some x -> 
        let xs' = Set.remove xs x in
        let f pol x = NetKAT_Types.Or(pol, to_netkat x) in
        Set.fold xs' ~init:(to_netkat x) ~f:f
end

module type ATOM = sig
  type t = Pattern.Set.t * Pattern.t
  module Set : Set.S with type Elt.t = t
  module DepMap : Map.S with type Key.t = t
  module Map : Map.S with type Key.t = t
  val to_string : t -> string
  val compare : t -> t -> int 
  val mk : Pattern.t -> t
  val tru : t
  val neg : t -> Set.t
  val seq : t -> t -> t option 
  val seq_act : t -> Action.t -> t -> t option
end

module Atom : ATOM = struct

  type t = (Pattern.Set.t * Pattern.t) sexp_opaque with sexp

  let compare ((xs1,x1):t) ((xs2,x2):t) : int = 
    let cmp = Pattern.Set.compare xs1 xs2 in 
    if cmp <> 0 then cmp
    else Pattern.compare x1 x2  

  let shadows (xs1,x1) (xs2,x2) = 
    let ys = 
      Pattern.Set.fold xs1 ~init:Pattern.Set.empty 
        ~f:(fun acc xi -> 
          match Pattern.seq x1 xi with
            | None -> acc
            | Some x1_xi -> Pattern.Set.add acc x1_xi) in 
    Pattern.Set.mem ys x2

  let dep_compare ((xs1,x1) as r1) ((xs2,x2) as r2) = 
    let r = 
      if shadows r2 r1 then 
        -1
      else if shadows r1 r2 then 
        1
      else 
        let cmp = Pattern.Set.compare xs1 xs2 in 
        if cmp = 0 then 
          Pattern.compare x1 x2 
        else 
          cmp in 
    (* Printf.printf "COMPARE %s %s = %d\n%!" (to_string (xs1,x1)) (to_string (xs2,x2)) r; *)
    r

  type this_t = t with sexp

  module Set = Set.Make(struct
    type t = this_t with sexp
    let compare = compare
  end)

  module DepMap = Map.Make(struct
    type t = this_t with sexp
    let compare = dep_compare
  end)

  module Map = Map.Make(struct
    type t = this_t with sexp
    let compare = compare
  end)

  let to_string ((xs,x):t) : string = 
    Printf.sprintf "%s,%s"
      (Pattern.set_to_string xs)
      (Pattern.to_string x)

  let mk (x:Pattern.t) : t =
    (Pattern.Set.empty, x)

  let tru : t = 
    mk Pattern.tru

  let check ((xs,x):t) : t option =
    if Pattern.Set.exists xs (fun xi -> Pattern.subseteq x xi) then
      None
    else 
      let xs' = 
        Pattern.Set.filter xs
          ~f:(fun xi -> Pattern.seq x xi <> None) in 
      Some (xs',x)
    
  let seq ((xs1,x1):t) ((xs2,x2):t) : t option =
    match Pattern.seq x1 x2 with
      | Some x12 ->
        check (Pattern.Set.union xs1 xs2, x12)
      | None ->
        None

  let seq_act (xs1,x1) a (xs2,x2) = 
    match Pattern.seq_act x1 a x2 with
      | None -> 
        None
      | Some x1ax2 ->
        let xs =
          Pattern.Set.fold xs2 
            ~init:xs1
            ~f:(fun acc xs2i ->
              match Pattern.seq_act Pattern.tru a xs2i with
                | Some truaxs2i ->
                  Pattern.Set.add acc truaxs2i 
                | None ->
                  acc) in 
        check (xs, x1ax2)  

  let neg (xs,x) : Set.t = 
    let init = 
      match check (Pattern.Set.singleton x, Pattern.tru) with 
        | None -> Set.empty
        | Some r -> Set.singleton r in 
    Pattern.Set.fold xs
      ~init:init
      ~f:(fun acc xi -> Set.add acc (mk xi))
end

module type OPTIMIZE = sig
  open NetKAT_Types 
  val mk_and : pred -> pred -> pred
  val mk_or : pred -> pred -> pred
  val mk_not : pred -> pred
  val mk_filter : pred -> policy
  val mk_mod : field -> fieldVal -> policy
  val mk_seq : policy -> policy -> policy
  val mk_par : policy -> policy -> policy
  val mk_star : policy -> policy
  val specialize_pred : switchId -> pred -> pred
  val specialize_policy : switchId -> policy -> policy
end

module Optimize : OPTIMIZE = struct
  let mk_and pr1 pr2 = 
    match pr1, pr2 with 
      | NetKAT_Types.True, _ -> 
        pr2
      | _, NetKAT_Types.True -> 
        pr1
      | NetKAT_Types.False, _ -> 
        NetKAT_Types.False
      | _, NetKAT_Types.False -> 
        NetKAT_Types.False
      | _ -> 
        NetKAT_Types.And(pr1, pr2)

  let mk_or pr1 pr2 = 
    match pr1, pr2 with 
      | NetKAT_Types.True, _ -> 
        NetKAT_Types.True
      | _, NetKAT_Types.True -> 
        NetKAT_Types.True
      | NetKAT_Types.False, _ -> 
        pr2
      | _, NetKAT_Types.False -> 
        pr2
      | _ -> 
        NetKAT_Types.Or(pr1, pr2)

  let mk_not pat =
    match pat with
      | NetKAT_Types.False -> NetKAT_Types.True
      | NetKAT_Types.True -> NetKAT_Types.False
      | _ -> NetKAT_Types.Neg(pat) 

  let mk_mod f v = 
    NetKAT_Types.Mod (NetKAT_Types.Header f,v)

  let mk_filter pr = 
    NetKAT_Types.Filter (pr)

  let mk_par pol1 pol2 = 
    match pol1, pol2 with
      | NetKAT_Types.Filter NetKAT_Types.False, _ -> 
        pol2
      | _, NetKAT_Types.Filter NetKAT_Types.False -> 
        pol1
      | _ -> 
        NetKAT_Types.Union(pol1,pol2) 

  let mk_seq pol1 pol2 =
    match pol1, pol2 with
      | NetKAT_Types.Filter NetKAT_Types.True, _ -> 
        pol2
      | _, NetKAT_Types.Filter NetKAT_Types.True -> 
        pol1
      | NetKAT_Types.Filter NetKAT_Types.False, _ -> 
        pol1
      | _, NetKAT_Types.Filter NetKAT_Types.False -> 
        pol2
      | _ -> 
        NetKAT_Types.Seq(pol1,pol2) 

  let mk_star pol = 
    match pol with 
      | NetKAT_Types.Filter NetKAT_Types.True -> 
        pol
      | NetKAT_Types.Filter NetKAT_Types.False -> 
        NetKAT_Types.Filter NetKAT_Types.True
      | NetKAT_Types.Star(pol1) -> pol
      | _ -> NetKAT_Types.Star(pol)
  
  let specialize_pred sw pr = 
    let rec loop pr k = 
      match pr with
        | NetKAT_Types.True ->
          k pr
        | NetKAT_Types.False ->
          k pr
        | NetKAT_Types.Neg pr1 ->
          loop pr1 (fun pr -> k (mk_not pr))
        | NetKAT_Types.Test (NetKAT_Types.Switch, v) ->
          if v = VInt.Int64 sw then 
            k NetKAT_Types.True
          else
            k NetKAT_Types.False
        | NetKAT_Types.Test (h, v) ->
          k pr
        | NetKAT_Types.And (pr1, pr2) ->
          loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_and p1 p2)))
        | NetKAT_Types.Or (pr1, pr2) ->
          loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_or p1 p2))) in 
    loop pr (fun x -> x)

  let specialize_policy sw pol = 
    let rec loop pol k = 
      match pol with  
        | NetKAT_Types.Filter pr ->
          k (NetKAT_Types.Filter (specialize_pred sw pr))
        | NetKAT_Types.Mod (h, v) ->
          k pol 
        | NetKAT_Types.Union (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_par p1 p2)))
        | NetKAT_Types.Seq (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_seq p1 p2)))
        | NetKAT_Types.Star pol ->
          loop pol (fun p -> k (mk_star p))
        | NetKAT_Types.Link(sw,pt,sw',pt') ->
	  failwith "Not a local policy" in 
    loop pol (fun x -> x) 
end

module type LOCAL = sig
  type t = Action.group Atom.Map.t
  val to_string : t -> string
  val of_pred : NetKAT_Types.pred -> t
  val of_policy : NetKAT_Types.policy -> t
  val to_netkat : t -> NetKAT_Types.policy
end 

module Local : LOCAL = struct

  type t = Action.group Atom.Map.t

  let compare p q = 
    Atom.Map.compare Action.group_compare p q
      
  let to_string (m:t) : string = 
    Printf.sprintf "%s"
      (Atom.Map.fold m 
         ~init:""
         ~f:(fun ~key:r ~data:g acc ->
             Printf.sprintf "%s(%s) => %s\n"
               acc
               (Atom.to_string r) 
               (Action.group_to_string g)))

  let extend (r:Atom.t) (g:Action.group) (m:t) : t = 
    if Atom.Map.mem m r then
      begin 
        Printf.printf "OVERLAP\nM=\n%s\nR=\n%s\n"
          (to_string m)
          (Atom.to_string r);
        failwith "Local.extend: overlap"
      end
    else
      Atom.Map.add m r g

  let intersect (op:Action.group -> Action.group -> Action.group) (p:t) (q:t) : t = 
    Atom.Map.fold p
      ~init:Atom.Map.empty
      ~f:(fun ~key:r1 ~data:g1 acc ->
        Atom.Map.fold q
          ~init:acc
          ~f:(fun ~key:r2 ~data:g2 acc ->
            match Atom.seq r1 r2 with 
              | None -> 
                acc
              | Some r1_r2 -> 
                extend r1_r2 (op g1 g2) acc))  
    
  let par p q = 
    let r = intersect Action.group_cross p q in 
    (* debug "### PAR ###\n%s\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string q) *)
    (*   (to_string r); *)
    r
      
  let seq p q = 
    let cross_merge ~key:_ v =
      match v with 
        | `Left g1 -> Some g1
        | `Right g2 -> Some g2
        | `Both (g1,g2) -> Some (Action.group_cross g1 g2) in 
          
    let union_merge ~key:_ v = 
      match v with 
        | `Left g1 -> Some g1
        | `Right g2 -> Some g2
        | `Both (g1,g2) -> Some (Action.group_union g1 g2) in 

    let seq_act r1 a q = 
      Atom.Map.fold q 
        ~init:Atom.Map.empty 
        ~f:(fun ~key:r2 ~data:g2 acc -> 
          match Atom.seq_act r1 a r2 with
            | None -> 
              acc
            | Some r12 -> 
              extend r12 (Action.group_seq a g2) acc) in
    
    let seq_atom_acts_local r1 s1 q = 
      if Action.Set.is_empty s1 then 
        Atom.Map.singleton r1 (Action.group_mk s1)
      else
        Action.Set.fold s1
          ~init:Atom.Map.empty
          ~f:(fun acc a -> 
            let acc' = seq_act r1 a q in 
            Atom.Map.merge ~f:cross_merge acc acc') in 
    
    let r = 
      Atom.Map.fold p 
        ~init:Atom.Map.empty
        ~f:(fun ~key:r1 ~data:g1 acc -> 
          List.fold g1
            ~init:acc
            ~f:(fun acc si -> 
              let acc' = seq_atom_acts_local r1 si q in 
              Atom.Map.merge ~f:union_merge acc acc')) in 
    (* debug "### SEQ ###\n%s\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string q) *)
    (*   (to_string r); *)
    r
        
  let neg (p:t) : t= 
    let r =
      Atom.Map.map p 
        ~f:(fun g -> 
          if Action.group_is_drop g then Action.group_id 
          else if Action.group_is_id g then Action.group_drop
          else failwith "neg: not a predicate") in 
    (* debug "### NEGATE ###\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string r); *)
    r

  let star p = 
    let rec loop acc pi =
      let psucci = seq p pi in
      let acc' = par acc psucci in
      if compare acc acc' = 0 then
        acc
      else
        loop acc' psucci in
    let p0 = Atom.Map.singleton Atom.tru Action.group_id in
    let r = loop p0 p0 in 
    (* debug "### STAR ###\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string r); *)
    r

  let rec of_pred (pr:NetKAT_Types.pred) : t =
    let rec loop pr k = 
      match pr with
      | NetKAT_Types.True ->
        k (Atom.Map.singleton Atom.tru Action.group_id) 
      | NetKAT_Types.False ->
        k (Atom.Map.singleton Atom.tru Action.group_drop) 
      | NetKAT_Types.Neg pr ->
        loop pr (fun (p:t) -> k (neg p))
      | NetKAT_Types.Test (NetKAT_Types.Switch, v) ->
        failwith "Not a local policy"
      | NetKAT_Types.Test (NetKAT_Types.Header f, v) ->
        let r = Atom.mk (Pattern.mk f v) in 
        let m = 
          Atom.Set.fold (Atom.neg r)
            ~init:(Atom.Map.singleton r Action.group_id)
            ~f:(fun acc r -> extend r Action.group_drop acc) in
        k m
      | NetKAT_Types.And (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (seq p1 p2)))
      | NetKAT_Types.Or (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (par p1 p2))) in 
    loop pr (fun x -> x)

  let of_policy (pol:NetKAT_Types.policy) : t =
    let rec loop pol k =  
      match pol with
        | NetKAT_Types.Filter pr ->
          k (of_pred pr)
        | NetKAT_Types.Mod (NetKAT_Types.Switch, v) ->
          failwith "Not a local policy"
        | NetKAT_Types.Mod (NetKAT_Types.Header f, v) -> 
          let a = Action.mk f v in 
          let g = Action.group_mk (Action.Set.singleton a) in 
          let m = Atom.Map.singleton Atom.tru g in 
          k m
        | NetKAT_Types.Union (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (par p1 p2)))
        | NetKAT_Types.Seq (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (seq p1 p2)))
        | NetKAT_Types.Star pol ->
          loop pol (fun p -> k (star p))
        | NetKAT_Types.Link(sw,pt,sw',pt') ->
	  failwith "Not a local policy" in 
    loop pol (fun p -> p)

  let to_netkat (m:t) : NetKAT_Types.policy =
    let open Optimize in 
    let rec loop m =
    match Atom.Map.min_elt m with 
      | None -> 
        NetKAT_Types.Filter NetKAT_Types.False
      | Some (r,g) -> 
        let m' = Atom.Map.remove m r in
        let (xs,x) = r in
        let nc_pred = mk_and (mk_not (Pattern.set_to_netkat xs)) (Pattern.to_netkat x) in
        let nc_pred_acts = mk_seq (NetKAT_Types.Filter nc_pred) (Action.group_to_netkat g) in
        mk_par nc_pred_acts  (loop m') in
    loop m
end

module RunTime = struct

  let to_action (a:Action.t) (pto: fieldVal option) : seq =
    let port = 
      match List.Assoc.find a InPort, pto with 
        | None, None -> 
          failwith "indeterminate port"
        | Some pt,_ -> pt
        | _, Some pt -> pt in 
    let mods = List.Assoc.remove a InPort in 
    let mk_mod act (f, v) = SetField(f,v)::act in 
    List.fold mods ~init:[OutputPort port] ~f:mk_mod

  let set_to_action (s:Action.Set.t) (pto : fieldVal option) : par =
    let f par a = (to_action a pto)::par in
    Action.Set.fold s ~f:f ~init:[]

  let group_to_action (g:Action.group) (pto:fieldVal option) : group =
    List.map g ~f:(fun s -> set_to_action s pto)

  let to_pattern (x:Pattern.t) : pattern =
    List.fold x 
      ~init:SDN_Types.FieldMap.empty
      ~f:(fun acc (f,v) -> SDN_Types.FieldMap.add f v acc)
      
  type i = Local.t

  let compile (sw:switchId) (pol:NetKAT_Types.policy) : i =
    let pol' = Optimize.specialize_policy sw pol in 
    let n,n' = Semantics.size pol, Semantics.size pol' in 
    Printf.printf " [compression: %d -> %d = %.3f] " 
      n n' (Float.of_int n' /. Float.of_int n);
    Local.of_policy pol'

  let decompile (p:i) : NetKAT_Types.policy =
    Local.to_netkat p

  let simpl_flow (p : pattern) (a : group) : flow =
    { pattern = p;
      action = a;
      cookie = 0L;
      idle_timeout = Permanent;
      hard_timeout = Permanent }

  (* Prunes out rules that apply to other switches. *)
  let to_table (m:i) : flowTable =
    let dm = 
      Atom.Map.fold m
        ~init:Atom.DepMap.empty
        ~f:(fun ~key:r ~data:g acc -> Atom.DepMap.add acc r g) in 
    let add_flow x g l =
      let pat = to_pattern x in 
      let act = group_to_action g (List.Assoc.find x InPort) in 
      simpl_flow pat act::l in
    let rec loop dm acc cover =
      match Atom.DepMap.min_elt dm with 
        | None -> 
          acc 
        | Some (r,g) -> 
          let (xs,x) = r in
          let dm' = Atom.DepMap.remove dm r in
          let ys = 
            Pattern.Set.fold
              xs ~init:Pattern.Set.empty
              ~f:(fun acc xi -> 
                match Pattern.seq xi x with 
                  | None -> acc
                  | Some xi_x -> Pattern.Set.add acc xi_x) in 
          let zs = 
            Pattern.Set.fold ys 
              ~init:Pattern.Set.empty
              ~f:(fun acc yi -> 
                if Pattern.Set.exists cover ~f:(Pattern.subseteq yi) then 
                  acc
                else
                  Pattern.Set.add acc yi) in 
          let acc' = 
            Pattern.Set.fold zs 
              ~init:acc 
              ~f:(fun acc x -> add_flow x Action.group_drop acc) in
          let acc'' = add_flow x g acc' in
          let cover' = Pattern.Set.add (Pattern.Set.union zs cover) x in
          loop dm' acc'' cover' in
    List.rev (loop dm [] Pattern.Set.empty)
end

(* exports *)
type t = RunTime.i

let of_policy sw pol = 
  Local.of_policy (Optimize.specialize_policy sw pol)

let to_netkat = 
  Local.to_netkat

let compile = 
  RunTime.compile

let decompile = 
  RunTime.decompile

let to_table = 
  RunTime.to_table
