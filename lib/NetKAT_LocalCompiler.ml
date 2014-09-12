open Core.Std
open Sexplib.Conv
open SDN_Types

module type FIELD = sig
  type t with sexp
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val any : t
  val is_any : t -> bool
  val inter : t -> t -> t option
  val subseteq : t -> t -> bool
  val join : t -> t -> t option
end

(* Option functor *)
module Option (H:NetKAT_Types.Headers.HEADER) =
struct
  type t = H.t option with sexp
  let compare o1 o2 =
    match o1,o2 with
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> 0
      | Some x1, Some x2 -> H.compare x1 x2
  let equal o1 o2 =
    compare o1 o2 = 0
  let to_string o =
    match o with
      | None -> "None"
      | Some x -> Printf.sprintf "Some(%s)" (H.to_string x)
  let any = 
    None
  let is_any o =
    o = None
  let inter o1 o2 = 
    match o1,o2 with 
    | None, _ -> Some o2
    | _, None -> Some o1
    | Some x1, Some x2 -> 
      if H.equal x1 x2 then Some o1
      else None
  let subseteq o1 o2 = 
    match o1,o2 with 
    | _,None -> true
    | None,_ -> false
    | Some x1, Some x2 -> 
      H.equal x1 x2
  let join o1 o2 = 
    match o1,o2 with
    | None, _ -> Some None
    | _, None -> Some None
    | Some x1, Some x2 -> 
      if H.equal x1 x2 then Some o1 
      else None
end

module PrefixTable (F:FIELD) = struct
  type v = F.t with sexp

  type t = (v * bool) list with sexp

  let singleton x = 
    if F.is_any x then 
      [ (x,true) ]
    else
      [ (x,true)
      ; (F.any, false) ]

  let rec compare (a:t) (b:t) =
    match (a,b) with
    | [],[] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | (p1,b1)::t1, (p2,b2)::t2 ->       
      let cmp1 = F.compare p1 p2 in 
      if cmp1 <> 0 then cmp1
      else 
	let cmp2 = Pervasives.compare b1 b2 in 
	if cmp2 <> 0 then cmp2
	else compare t1 t2

  let equal t1 t2 = 
    compare t1 t2 = 0

  let to_string (t:t) =
    Printf.sprintf "[%s]"
      (List.fold_left t ~init:""
	 ~f:(fun acc (x,b) -> 
               Printf.sprintf "%s%s(%s:%b)" 
		 acc
		 (if acc = "" then acc else " ")
		 (F.to_string x) 
		 b))

  let any =
    [F.any, true]

  module S = Set.Make(F)
  let set_to_string s = 
    S.fold s
      ~init:""
      ~f:(fun acc x -> 
    	    Printf.sprintf "%s%s%s"
	      acc
	      (if acc = "" then "" else ", ")
	      (F.to_string x))
    
  let footprint t : S.t = 
    let rec loop y l s = 
      match l with 
      | [] -> 
	Set.add s y
      | h::t -> 
	(match F.join h y with 
	| Some z -> 
	  loop z ((S.to_list s) @ t) S.empty
	| None -> 
	  loop y t (S.add s h)) in 
    begin 
      match List.map t ~f:fst with 
      | [] -> S.empty
      | x::t -> loop x t S.empty
    end

  let is_shadowed x t = 
    let f = footprint t in 
    let b = S.exists f (fun y -> F.subseteq x y) in 
    (* Printf.printf "IS_SHADOWED: %s\n%s\n{%s}\n%b\n" *)
    (*   (F.to_string x) (to_string t) (set_to_string f) b; *)
    b
	 
  let remove_shadowed (t:t) : t =
    List.rev (
      List.fold_left
        ~f:(fun acc (x,b) -> 
	  let s = is_shadowed x acc in 
	  (* Printf.printf "ACC=%s\nP=%s,%b\n%b\n" *)
	  (*   (to_string acc) (F.to_string x) b s; *)
	  if s then acc else ((x,b)::acc))
        ~init:[]
        t)

  let is_any (t:t) : bool =
    List.for_all (remove_shadowed t) ~f:snd
      
  let neg t = 
    List.map t ~f:(fun (p,b) -> (p,not b))

  let empty =
    [F.any, false]

  let is_empty t = 
    not (List.exists (remove_shadowed t) ~f:snd)

  let inter t1 t2 =
    let r = 
      List.fold_left t1 ~init:[]
        ~f:(fun acc (p1,b1) ->
          List.fold_left t2 ~init:acc
            ~f:(fun acc (p2, b2) ->
              match F.inter p1 p2 with
              | None -> 
		acc
              | Some p -> 
		(p, b1 && b2)::acc)) in 
    let r = remove_shadowed (List.rev r) in 
    (* Printf.printf "INTER\n  %s\n  %s\n  %s\n" *)
    (*   (to_string t1) *)
    (*   (to_string t2) *)
    (*   (to_string r); *)
    r
	
  let diff t1 t2 =
    inter t1 (neg t2)

  (* (\* semantic compare *\) *)
  (* let compare t1 t2 = *)
  (*   if is_empty (diff t1 t2) && *)
  (*      is_empty (diff t2 t1) then *)
  (*     0 *)
  (*   else compare t1 t2 *)

  (* let equal t1 t2 = *)
  (*   compare t1 t2 = 0 *)

  let to_netkat_pred (v_to_pred: v -> NetKAT_Types.pred) (t:t) =
    let open NetKAT_Types in 
    let open Optimize in 
    fst (List.fold_left t
   	  ~init:(False,True)
	  ~f:(fun (pr,prs) (v,b) -> 
	    let v_pr = v_to_pred v in 
	    let pr' = mk_or pr (mk_and prs (if b then v_pr else False)) in 
	    let prs' = mk_and prs (mk_not v_pr) in 
	    (pr',prs')))

  let rec drop_false l =
    match l with
    | (_,false)::t -> 
      drop_false t
    | _ -> 
      List.rev l
  
  let expand t =
    let x = drop_false (List.rev t) in 
    (* Printf.printf "EXPAND\n"; *)
    (* List.iter x *)
    (*   ~f:(fun (o,b) ->  *)
    (* 	Printf.printf  *)
    (* 	  "%s : %b\n" *)
    (* 	  (match o with None -> "None" | Some p -> F.to_string p) *)
    (* 	  b); *)
    x

  let obscures (t1:t) (t2:t) : bool =
    Printf.printf
      "OBSCURES\n%s\n%s\n"
      (to_string t1)
      (to_string t2);
    (* Does any rule in (expand t1) shadow a rule in t2? *)
    let r,_ = 
      List.fold_right (drop_false t2)
	~init:(true, expand t1)
	~f:(fun (x,b) (nob,t) -> 
          Printf.printf "  X=%s\n" (F.to_string x);
          Printf.printf "  T=%s\n" (to_string t);
          Printf.printf "  SD=%b\n" (is_shadowed x t);
	  let ok' = nob && not (is_shadowed x t) in 
	  let t' = (x,b)::t in 
	  (ok',t')) in 
    Printf.printf "RESULT=%b\n" (not r);
    not r
end

module OL = Option(NetKAT_Types.LocationHeader)
module O48 = Option(NetKAT_Types.Int64Header)
module O32 = Option(NetKAT_Types.Int32Header)
module O16 = Option(NetKAT_Types.IntHeader)
module O8 = Option(NetKAT_Types.IntHeader)
module OIp = Option(NetKAT_Types.Int32TupleHeader)

module PTL = PrefixTable(OL)
module PT48 = PrefixTable(O48)
module PT32 = PrefixTable(O32)
module PT16 = PrefixTable(O16)
module PT8 = PrefixTable(O8)
module PTIp = PrefixTable(NetKAT_Types.Int32TupleHeader)

module HeadersOptionalValues =
  NetKAT_Types.Headers.Make
    (OL)
    (O48)
    (O48)
    (O16)
    (O8)
    (O16)
    (O8)
    (OIp)
    (OIp)
    (O16)
    (O16)

module HeadersPrefixTable =
  NetKAT_Types.Headers.Make
    (PTL)
    (PT48)
    (PT48)
    (PT16)
    (PT8)
    (PT16)
    (PT8)
    (PTIp)
    (PTIp)
    (PT16)
    (PT16)

(* H to the izz-O, V to the izz-A... *)
module HOV = HeadersOptionalValues
module HOVSet = Set.Make(HOV)
module HOVMap = Map.Make(HOV)

module HPT = HeadersPrefixTable
module HPTSet = Set.Make(HPT)
module HPTMap = Map.Make(HPT)

module Action = struct

  type t = HOV.t with sexp

  module Set = HOVSet

  let action_to_netkat (a:t) : NetKAT_Types.policy =
    let open NetKAT_Types in
    let open HOV in
    let g setter (acc:policy) f : policy =
      match Field.get f a with
        | Some v -> Optimize.mk_seq (Mod (setter v)) acc
        | _ -> acc in
    HOV.Fields.fold
      ~init:id (* identity policy *)
      ~location:(g (fun v -> Location v))
      ~ethSrc:(g (fun v -> EthSrc v))
      ~ethDst:(g (fun v ->EthDst v))
      ~vlan:(g (fun v -> Vlan v))
      ~vlanPcp:(g (fun v -> VlanPcp v))
      ~ethType:(g (fun v -> EthType v))
      ~ipProto:(g (fun v -> IPProto v))
      ~ipSrc:(g (fun (n,m) -> IP4Src(n,m)))
      ~ipDst:(g (fun (n,m) -> IP4Dst(n,m)))
      ~tcpSrcPort:(g (fun v -> TCPSrcPort v))
      ~tcpDstPort:(g (fun v -> TCPDstPort v))

  let set_to_netkat (s:Set.t) : NetKAT_Types.policy =
    let open NetKAT_Types in
    Set.fold s
      ~init:drop
      ~f:(fun acc a -> Optimize.mk_union acc (action_to_netkat a))

  let to_string : t -> string =
    HOV.to_string ~init:"id" ~sep:":="

  let set_to_string (s:Set.t) : string =
    if Set.is_empty s then "drop"
    else
      Set.fold s
        ~init:""
        ~f:(fun acc a ->
          Printf.sprintf "%s%s"
            (if acc = "" then acc else acc ^ ", ")
            (HOV.to_string ~init:"id" ~sep:":=" a))

  let action_id : t =
    let open HOV in
        { location = None;
          ethSrc = None;
          ethDst = None;
          vlan = None;
          vlanPcp = None;
          ethType = None;
          ipProto = None;
          ipSrc = None;
          ipDst = None;
          tcpSrcPort = None;
          tcpDstPort = None }

  let is_action_id (x:t) : bool =
    HOV.compare x action_id = 0

  let mk_location l = { action_id with HOV.location = Some l }
  let mk_ethSrc n = { action_id with HOV.ethSrc = Some n }
  let mk_ethDst n = { action_id with HOV.ethDst = Some n }
  let mk_vlan n = { action_id with HOV.vlan = Some n }
  let mk_vlanPcp n = { action_id with HOV.vlanPcp = Some n }
  let mk_ethType n = { action_id with HOV.ethType = Some n }
  let mk_ipProto n = { action_id with HOV.ipProto = Some n }
  let mk_ipSrc n = { action_id with HOV.ipSrc = Some n }
  let mk_ipDst n = { action_id with HOV.ipDst = Some n }
  let mk_tcpSrcPort n = { action_id with HOV.tcpSrcPort = Some n }
  let mk_tcpDstPort n = { action_id with HOV.tcpDstPort = Some n }

  let seq (x:t) (y:t) : t =
    let g (acc:t) f : t =
      match Field.get f y with
        | Some _ as o2 ->
          Field.fset f acc o2
        | _ -> acc in
    HOV.Fields.fold
      ~init:x
      ~location:g
      ~ethSrc:g
      ~ethDst:g
      ~vlan:g
      ~vlanPcp:g
      ~ethType:g
      ~ipProto:g
      ~ipSrc:g
      ~ipDst:g
      ~tcpSrcPort:g
      ~tcpDstPort:g

  let set_seq a s =
    Set.map s (seq a)

  let id : Set.t =
    Set.singleton (action_id)

  let drop : Set.t =
    Set.empty

  let is_id (s:Set.t) : bool =
    Set.length s = 1 &&
    match Set.min_elt s with
      | None -> false
      | Some a -> is_action_id a

  let is_drop (s:Set.t) : bool =
    Set.is_empty s
end

module Pattern = struct
  type t = HPT.t with sexp

  module Set = HPTSet
  module Map = HPTMap

  let compare (x:t) (y:t) : int =
    HPT.compare x y

  let to_string ?init ?sep (x:t) =
    HPT.to_string ?init ?sep x

  let to_netkat_pred (p:t) : NetKAT_Types.pred =
    let open NetKAT_Types in
    let open HPT in
    let g field_to_pred v_to_pred acc f : pred =
      Optimize.mk_and acc (field_to_pred v_to_pred (Field.get f p)) in
    let l f = function
      | None -> True
      | Some x -> Test (f x) in 
    Fields.fold
      ~init:True
      ~location:(g PTL.to_netkat_pred (l (fun v -> Location v)))
      ~ethSrc:(g PT48.to_netkat_pred (l (fun v -> EthSrc v)))
      ~ethDst:(g PT48.to_netkat_pred (l (fun v -> EthDst v)))
      ~vlan:(g PT16.to_netkat_pred (l (fun v -> Vlan v)))
      ~vlanPcp:(g PT8.to_netkat_pred (l (fun v -> VlanPcp v)))
      ~ethType:(g PT16.to_netkat_pred (l (fun v -> EthType v)))
      ~ipProto:(g PT8.to_netkat_pred (l (fun v -> IPProto v)))
      ~ipSrc:(g PTIp.to_netkat_pred (fun (v,m) -> 
	                               if m = 0l then True
				       else Test (IP4Src (v,m))))
      ~ipDst:(g PTIp.to_netkat_pred (fun (v,m) -> 
	                               if m = 0l then True 
				       else Test (IP4Dst (v,m))))
      ~tcpSrcPort:(g PT16.to_netkat_pred (l (fun v -> TCPSrcPort v)))
      ~tcpDstPort:(g PT16.to_netkat_pred (l (fun v -> TCPDstPort v)))

  let any : t =
    let open HPT in
        { location = PTL.any;
          ethSrc = PT48.any;
          ethDst = PT48.any;
          vlan = PT16.any;
          vlanPcp = PT8.any;
          ethType = PT16.any;
          ipProto = PT8.any;
          ipSrc = PTIp.any;
          ipDst = PTIp.any;
          tcpSrcPort = PT16.any;
          tcpDstPort = PT16.any }

  let empty : t =
    let open HPT in
        { location = PTL.empty;
          ethSrc = PT48.empty;
          ethDst = PT48.empty;
          vlan = PT16.empty;
          vlanPcp = PT8.empty;
          ethType = PT16.empty;
          ipProto = PT8.empty;
          ipSrc = PTIp.empty;
          ipDst = PTIp.empty;
          tcpSrcPort = PT16.empty;
          tcpDstPort = PT16.empty }

  let is_any (x:t) : bool =
    let g is_any f = is_any (Field.get f x) in
    HPT.Fields.for_all
      ~location:(g PTL.is_any)
      ~ethSrc:(g PT48.is_any)
      ~ethDst:(g PT48.is_any)
      ~vlan:(g PT16.is_any)
      ~vlanPcp:(g PT8.is_any)
      ~ethType:(g PT16.is_any)
      ~ipProto:(g PT8.is_any)
      ~ipSrc:(g PTIp.is_any)
      ~ipDst:(g PTIp.is_any)
      ~tcpSrcPort:(g PT16.is_any)
      ~tcpDstPort:(g PT16.is_any)

  let is_empty (x:t) : bool =
    let g is_empty f = is_empty (Field.get f x) in
    HPT.Fields.exists
      ~location:(g PTL.is_empty)
      ~ethSrc:(g PT48.is_empty)
      ~ethDst:(g PT48.is_empty)
      ~vlan:(g PT16.is_empty)
      ~vlanPcp:(g PT8.is_empty)
      ~ethType:(g PT16.is_empty)
      ~ipProto:(g PT8.is_empty)
      ~ipSrc:(g PTIp.is_empty)
      ~ipDst:(g PTIp.is_empty)
      ~tcpSrcPort:(g PT16.is_empty)
      ~tcpDstPort:(g PT16.is_empty)

  let mk_location l = { any with HPT.location = PTL.singleton (Some l) }
  let mk_ethSrc n = { any with HPT.ethSrc = PT48.singleton (Some n) }
  let mk_ethDst n = { any with HPT.ethDst = PT48.singleton (Some n) }
  let mk_vlan n = { any with HPT.vlan = PT16.singleton (Some n) }
  let mk_vlanPcp n = { any with HPT.vlanPcp = PT8.singleton (Some n) }
  let mk_ethType n = { any with HPT.ethType = PT16.singleton (Some n) }
  let mk_ipProto n = { any with HPT.ipProto = PT16.singleton (Some n) }
  let mk_ipSrc n = { any with HPT.ipSrc = PTIp.singleton n }
  let mk_ipDst n = { any with HPT.ipDst = PTIp.singleton n }
  let mk_tcpSrcPort n = { any with HPT.tcpSrcPort = PT16.singleton (Some n) }
  let mk_tcpDstPort n = { any with HPT.tcpDstPort = PT16.singleton (Some n) }

  (* A pattern
   *   f1 = c1 ; ... fk = ck
   * represents a conjunction of constraints
   *   f1 = c1 /\ ... /\ fk = ck
   * with the constraints expressed using Pos/Neg sets.
   *
   * To negate a conjunction, we use DeMorgan's law:
   *   neg (f1 = c1 /\ ... /\ fk = ck)
   * = neg(f1 = c1) \/ ... \/ neg (fk = ck)
   *
   * The final set represents this disjunction, with obvious
   * contradictions removed. *)
  let neg (x:t) : Set.t =
    let open HPT in
        let g is_empty neg acc f =
          let z = neg (Field.get f x) in
          if is_empty z then acc
          else Set.add acc (Field.fset f any z) in
        Fields.fold
          ~init:Set.empty
          ~location:PTL.(g is_empty neg)
          ~ethSrc:PT48.(g is_empty neg)
          ~ethDst:PT48.(g is_empty neg)
          ~vlan:PT16.(g is_empty neg)
          ~vlanPcp:PT8.(g is_empty neg)
          ~ethType:PT16.(g is_empty neg)
          ~ipProto:PT8.(g is_empty neg)
          ~ipSrc:PTIp.(g is_empty neg)
          ~ipDst:PTIp.(g is_empty neg)
          ~tcpSrcPort:PT16.(g is_empty neg)
          ~tcpDstPort:PT16.(g is_empty neg)

  let obscures (x:t) (y:t) : bool =
    let open NetKAT_Types.Headers in
        let g c f = c (Field.get f x) (Field.get f y) in
        HPT.Fields.for_all
          ~location:(g PTL.obscures)
          ~ethSrc:(g PT48.obscures)
          ~ethDst:(g PT48.obscures)
          ~vlan:(g PT16.obscures)
          ~vlanPcp:(g PT8.obscures)
          ~ethType:(g PT16.obscures)
          ~ipProto:(g PT8.obscures)
          ~ipSrc:(g PTIp.obscures)
          ~ipDst:(g PTIp.obscures)
          ~tcpSrcPort:(g PT16.obscures)
          ~tcpDstPort:(g PT16.obscures)

  let seq (x:t) (y:t) : t option =
    let open HPT in
        let g is_empty inter acc f =
          match acc with
            | None ->
              None
            | Some z ->
              let pn = inter (Field.get f x) (Field.get f y) in
              if is_empty pn then None
              else Some (Field.fset f z pn) in
        Fields.fold
          ~init:(Some any)
          ~location:PTL.(g is_empty inter)
          ~ethSrc:PT48.(g is_empty inter)
          ~ethDst:PT48.(g is_empty inter)
          ~vlan:PT16.(g is_empty inter)
          ~vlanPcp:PT8.(g is_empty inter)
          ~ethType:PT16.(g is_empty inter)
          ~ipProto:PT8.(g is_empty inter)
          ~ipSrc:PTIp.(g is_empty inter)
          ~ipDst:PTIp.(g is_empty inter)
          ~tcpSrcPort:PT16.(g is_empty inter)
          ~tcpDstPort:PT16.(g is_empty inter)

  let seq_act (x:t) (a:Action.t) (y:t) : t option =
    let open HPT in
    let g get is_empty inter singleton acc f =
      match acc with
      | None ->
        None
      | Some z ->
        begin match Field.get f x, get a, Field.get f y with
        | pn1, None, pn2 ->
          let pn = inter pn1 pn2 in
          if is_empty pn then None
          else Some (Field.fset f z pn)
        | pn1, Some n, pn2 ->
          if is_empty (inter (singleton n) pn2) then None
          else Some (Field.fset f z pn1)
        end in
    let lo f n = f (Some n) in 
    HPT.Fields.fold
      ~init:(Some any)
      ~location:PTL.(g HOV.location is_empty inter (lo singleton))
      ~ethSrc:PT48.(g HOV.ethSrc is_empty inter (lo singleton))
      ~ethDst:PT48.(g HOV.ethDst is_empty inter (lo singleton))
      ~vlan:PT16.(g HOV.vlan is_empty inter (lo singleton))
      ~vlanPcp:PT8.(g HOV.vlanPcp is_empty inter (lo singleton))
      ~ethType:PT16.(g HOV.ethType is_empty inter (lo singleton))
      ~ipProto:PT8.(g HOV.ipProto is_empty inter (lo singleton))
      ~ipSrc:PTIp.(g HOV.ipSrc is_empty inter singleton)
      ~ipDst:PTIp.(g HOV.ipDst is_empty inter singleton)
      ~tcpSrcPort:PT16.(g HOV.tcpSrcPort is_empty inter (lo singleton))
      ~tcpDstPort:PT16.(g HOV.tcpDstPort is_empty inter (lo singleton))
      
  let diff (x:t) (y:t) : Set.t =
    let negged:Set.t = neg y in
    let intersect x a =
      match seq x a with
        Some s -> s
      | None -> empty in
    Set.map ~f:(fun a -> intersect x a) negged
end

module Local = struct

  (* TODO(arjun): Why Action.Set.t? Don't we want to know that each
     action affects a distinct field? Shouldn't this be Action.Map.t? *)
  type t = Action.Set.t Pattern.Map.t

  let rule_to_netkat p a : NetKAT_Types.policy =
    let open NetKAT_Types in
    Optimize.mk_seq (Filter (Pattern.to_netkat_pred p)) (Action.set_to_netkat a)

  let to_netkat (t:t) : NetKAT_Types.policy =
    let open NetKAT_Types in
    Pattern.Map.fold t
      ~init:drop
      ~f:(fun ~key ~data acc -> Optimize.mk_union acc (rule_to_netkat key data))

  let compare p q =
    Pattern.Map.compare Action.Set.compare p q

  let to_string (m:t) : string =
    Printf.sprintf "%s"
      (Pattern.Map.fold m
         ~init:""
         ~f:(fun ~key:r ~data:g acc ->
           Printf.sprintf "%s(%s) => %s\n"
             acc
             (Pattern.to_string r)
             (Action.set_to_string g)))

  exception IllFormed
  (* check for well-formedness of a value of type t *)
  let check (m:t) : bool = 
    try 
      Pattern.Map.iter m
        ~f:(fun ~key:x1 ~data:s1 -> 
          Pattern.Map.iter m
            ~f:(fun ~key:x2 ~data:s2 -> 
              if Pattern.obscures x1 x2 && 
                Pattern.obscures x2 x1 && 
                Action.Set.compare s1 s2 <> 0
              then
                (Printf.printf "Local.check failed:\n%s=>%s\n%s=>%s\n"
                   (Pattern.to_string x1) (Action.set_to_string s1)
                   (Pattern.to_string x2) (Action.set_to_string s2);
                 raise IllFormed)));
      true
    with _ -> 
      false
            
  let extend (x:Pattern.t) (s:Action.Set.t) (m:t) : t =
    let r = match Pattern.Map.find m x with
      | None ->
        Pattern.Map.add m x s
      | Some s' ->
        Pattern.Map.add m x (Action.Set.union s s') in
    (* Printf.printf "EXTEND\nM=%s\nX=%s\nS=%s\nR=%s\n" *)
    (*   (to_string m)  *)
    (*   (Pattern.to_string x) *)
    (*   (Action.set_to_string s) *)
    (*   (to_string r); *)
    r


  let intersect (op:Action.Set.t -> Action.Set.t -> Action.Set.t) (p:t) (q:t) : t =
    Pattern.Map.fold p
      ~init:Pattern.Map.empty
      ~f:(fun ~key:r1 ~data:s1 acc ->
        Pattern.Map.fold q
          ~init:acc
          ~f:(fun ~key:r2 ~data:s2 acc ->
            match Pattern.seq r1 r2 with
              | None ->
                acc
              | Some r1_r2 ->
                extend r1_r2 (op s1 s2) acc))

  let par p q =
    if Pattern.Map.is_empty p then q
    else if Pattern.Map.is_empty q then p
    else 
      let r = intersect Action.Set.union p q in
      (* Printf.printf "### PAR ###\n%s\n%s\n%s" *)
      (*   (to_string p) *)
      (*   (to_string q) *)
      (*   (to_string r); *)
      r

  let seq (p:t) (q:t) : t =
    let merge ~key:_ v =
      match v with
        | `Left s1 -> Some s1
        | `Right s2 -> Some s2
        | `Both (s1,s2) -> Some (Action.Set.union s1 s2) in

    let seq_act r1 a q : t =
      Pattern.Map.fold q
        ~init:Pattern.Map.empty
        ~f:(fun ~key:r2 ~data:s2 acc ->
          match Pattern.seq_act r1 a r2 with
            | None ->
              acc
            | Some r12 ->
              extend r12 (Action.set_seq a s2) acc) in

    let seq_atom_acts_local r1 s1 q : t =
      if Action.Set.is_empty s1 then
        Pattern.Map.singleton r1 s1
      else
        Action.Set.fold s1
          ~init:Pattern.Map.empty
          ~f:(fun acc a -> 
            let acc' = seq_act r1 a q in 
            par acc acc') in

    let r : t = 
      Pattern.Map.fold p
        ~init:Pattern.Map.empty
        ~f:(fun ~key:r1 ~data:s1 acc ->
          let acc' = seq_atom_acts_local r1 s1 q in
          Pattern.Map.merge ~f:merge acc acc') in
    (* Printf.printf "### SEQ ###\n%s\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string q) *)
    (*   (to_string r); *)
    r

  let neg (p:t) : t=
    let r =
      Pattern.Map.map p
        ~f:(fun s ->
          if Action.is_drop s then Action.id
          else if Action.is_id s then Action.drop
          else failwith "neg: not a predicate") in
    (* Printf.printf "### NEGATE ###\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string r); *)
    r

  let star (p:t) : t =
    (* let c = ref 0 in  *)
    let rec loop acc pi =
      (* Printf.printf "STAR LOOP %d\n" (incr c; !c); *)
      let psucci = seq p pi in
      let acc' = par acc psucci in
      if Pattern.Map.compare Action.Set.compare acc acc' = 0 then
        acc
      else
        loop acc' psucci in
    let p0 = Pattern.Map.singleton Pattern.any Action.id in
    let r = loop p0 p0 in
    (* Printf.printf "### STAR ###\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string r); *)
    r

  let rec of_pred (pr:NetKAT_Types.pred) : t =
    let rec loop pr k =
      match pr with
        | NetKAT_Types.True ->
          k (Pattern.Map.singleton Pattern.any Action.id)
        | NetKAT_Types.False ->
          k (Pattern.Map.singleton Pattern.any Action.drop)
        | NetKAT_Types.Neg pr ->
          loop pr (fun (p:t) -> k (neg p))
        | NetKAT_Types.Test hv ->
          let x = match hv with
            | NetKAT_Types.Switch n ->
              failwith "Not a local policy"
            | NetKAT_Types.Location l ->
              Pattern.mk_location l
            | NetKAT_Types.EthType n ->
              Pattern.mk_ethType n
            | NetKAT_Types.EthSrc n ->
              Pattern.mk_ethSrc n
            | NetKAT_Types.EthDst n ->
              Pattern.mk_ethDst n
            | NetKAT_Types.Vlan n ->
              Pattern.mk_vlan n
            | NetKAT_Types.VlanPcp n ->
              Pattern.mk_vlanPcp n
            | NetKAT_Types.IPProto n ->
              Pattern.mk_ipProto n
            | NetKAT_Types.IP4Src (n,m) ->
              Pattern.mk_ipSrc (n,m)
            | NetKAT_Types.IP4Dst (n,m) ->
	      Pattern.mk_ipDst (n,m) 
            | NetKAT_Types.TCPSrcPort n ->
              Pattern.mk_tcpSrcPort n
            | NetKAT_Types.TCPDstPort n ->
              Pattern.mk_tcpDstPort n in
          let m =
            Pattern.Set.fold (Pattern.neg x)
              ~init:(Pattern.Map.singleton x Action.id)
              ~f:(fun acc y -> extend y Action.drop acc) in
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
        | NetKAT_Types.Mod hv ->
          let a = match hv with
            | NetKAT_Types.Switch n ->
              failwith "Not a local policy"
            | NetKAT_Types.Location l ->
              Action.mk_location l
            | NetKAT_Types.EthType n ->
              Action.mk_ethType n
            | NetKAT_Types.EthSrc n ->
              Action.mk_ethSrc n
            | NetKAT_Types.EthDst n ->
              Action.mk_ethDst n
            | NetKAT_Types.Vlan n ->
              Action.mk_vlan n
            | NetKAT_Types.VlanPcp n ->
              Action.mk_vlanPcp n
            | NetKAT_Types.IPProto n ->
              Action.mk_ipProto n
            | NetKAT_Types.IP4Src (n,m) ->
	      assert (m = 32l);
              Action.mk_ipSrc (n,m)
            | NetKAT_Types.IP4Dst (n,m) ->
	      assert (m = 32l);
              Action.mk_ipDst (n,m)
            | NetKAT_Types.TCPSrcPort n ->
              Action.mk_tcpSrcPort n
            | NetKAT_Types.TCPDstPort n ->
              Action.mk_tcpDstPort n in
          let s = Action.Set.singleton a in
          let m = Pattern.Map.singleton Pattern.any s in
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
end

module RunTime = struct

  let to_action (a:Action.t) (pto: portId option) : seq =
    let generate init =
      let g h act f =
        match Field.get f a with
          | None -> act
          | Some v -> Modify(h v)::act in
      HOV.Fields.fold
        ~init
        ~location:(fun act _ -> act)
        ~ethSrc:(g (fun v -> SetEthSrc v))
        ~ethDst:(g (fun v -> SetEthDst v))
        ~vlan:(g (fun v -> SetVlan (Some(v))))
        ~vlanPcp:(g (fun v -> SetVlanPcp v))
        ~ethType:(g (fun v -> SetEthTyp v))
        ~ipProto:(g (fun v -> SetIPProto v))
        ~ipSrc:(g (fun (n,m) -> assert (m = 32l); SetIP4Src n))
        ~ipDst:(g (fun (n,m) -> assert (m = 32l); SetIP4Dst n))
        ~tcpSrcPort:(g (fun v -> SetTCPSrcPort v))
        ~tcpDstPort:(g (fun v -> SetTCPDstPort v)) in
    (* If an action sets the location to a pipe, ignore all other modifications.
     * They will be applied at the controller by Semantics.eval. Otherwise, the
     * port must be determined either by the pattern or by the action. The pto
     * is the port determined by the pattern, if it exists. If the port is not
     * determinate, then send it back out the port it came in.
     * *)
    match HOV.location a, pto with
      | Some (NetKAT_Types.Pipe(_)), _ ->
        [Output(Controller 128)]
      | Some (NetKAT_Types.Physical pt), _
      | None, Some pt ->
        generate [Output(Physical pt)]
      | None, None ->
        generate [Output(InPort)]

  (* XXX(seliopou, jnf) unimplementable actions will still produce bogus
   * outputs. For example, the following policy:
   *
   *   (f := 3; port := 1) | (g := 2; poirt := 2)
   *
   * requires two copies of the packet at the switch, which is not possible.
   * Policies like these must be implemented at the controller.
   * *)
  let set_to_action (s:Action.Set.t) (pto : portId option) : par =
    let f par a = 
      let act = to_action a pto in 
      if List.mem par act then par 
      else act::par in
    Action.Set.fold s ~f:f ~init:[]

  let simpl_flow (p : SDN_Types.Pattern.t) (a : par) : flow =
    { pattern = p;
      action = [a];
      cookie = 0L;
      idle_timeout = Permanent;
      hard_timeout = Permanent }

  let expand_rules (x:Pattern.t) (s:Action.Set.t) : flowTable =
    let m : HPT.t = 
      let open HPT in 
      List.iter x.HPT.location ~f:(function
        | (Some(NetKAT_Types.Pipe _), _) -> failwith "indetermiate port in pattern"
        | _                              -> ());
      { location = PTL.(expand x.HPT.location);
        ethSrc = PT48.(expand x.HPT.ethSrc);
        ethDst = PT48.(expand x.HPT.ethDst);
        vlan = PT16.(expand x.HPT.vlan);
        vlanPcp = PT8.(expand x.HPT.vlanPcp);
        ethType = PT16.(expand x.HPT.ethType);
        ipProto = PT8.(expand x.HPT.ipProto);
        ipSrc = PTIp.(expand x.HPT.ipSrc);
        ipDst = PTIp.(expand x.HPT.ipDst);
        tcpSrcPort = PT16.(expand x.HPT.tcpSrcPort);
        tcpDstPort = PT16.(expand x.HPT.tcpDstPort) } in 
    (* computes a cross product *)
    let rec cross m : (HOV.t * bool) list =
      let empty = let open HOV in
      { location = None; 
	ethSrc = None; 
	ethDst = None;
        vlan = None; 
	vlanPcp = None; 
	ethType = None;
        ipProto = None; 
	ipSrc = None; 
	ipDst = None;
        tcpSrcPort = None; 
	tcpDstPort = None;
      } in
      let g h rs f =
        let l = Field.get f m in
        List.fold_right
          l
          ~init:[]
          ~f:(fun (o, b) acc ->
            List.fold_right
              rs
              ~init:acc
              ~f:(fun (p, c) acc -> (h p o, b && c)::acc)) in
      HPT.Fields.fold
        ~init:[(empty, true)]
        ~location:(g (fun p o -> { p with HOV.location = o }))
        ~ethSrc:(g (fun p o -> { p with HOV.ethSrc = o }))
        ~ethDst:(g (fun p o -> { p with HOV.ethDst = o }))
        ~vlan:(g (fun p o -> { p with HOV.vlan = o }))
        ~vlanPcp:(g (fun p o -> { p with HOV.vlanPcp = o }))
        ~ethType:(g (fun p o -> { p with HOV.ethType = o }))
        ~ipProto:(g (fun p o -> { p with HOV.ipProto = o }))
        ~ipSrc:(g (fun p o -> 
	             if NetKAT_Types.Int32TupleHeader.is_any o then p 
		     else { p with HOV.ipSrc = Some o }))
        ~ipDst:(g (fun p o -> 
	             if NetKAT_Types.Int32TupleHeader.is_any o then p 
		     else { p with HOV.ipDst = Some o }))
        ~tcpSrcPort:(g (fun p o -> { p with HOV.tcpSrcPort = o }))
        ~tcpDstPort:(g (fun p o -> { p with HOV.tcpDstPort = o })) in 
    
    (* helper function to generate the actual (pattern * par) rules for the SDN_Types.flowTable *)
    let go (cd : (HOV.t * bool) list) : flowTable =
      let il x = match x with
        | NetKAT_Types.Pipe _ ->
          failwith "indeterminate port"
        | NetKAT_Types.Physical n ->
          n in
      List.map cd ~f:(fun (x, b) ->
        let default_port = Core_kernel.Option.map ~f:il x.HOV.location in
        let actions = if b then set_to_action s default_port else [] in
        let pattern =
          { SDN_Types.Pattern.dlSrc = x.HOV.ethSrc
          ; SDN_Types.Pattern.dlDst = x.HOV.ethDst
          ; SDN_Types.Pattern.dlTyp = x.HOV.ethType
          ; SDN_Types.Pattern.dlVlan = x.HOV.vlan
          ; SDN_Types.Pattern.dlVlanPcp = x.HOV.vlanPcp
          ; SDN_Types.Pattern.nwSrc = x.HOV.ipSrc
          ; SDN_Types.Pattern.nwDst = x.HOV.ipDst
          ; SDN_Types.Pattern.nwProto = x.HOV.ipProto
          ; SDN_Types.Pattern.tpSrc = x.HOV.tcpSrcPort
          ; SDN_Types.Pattern.tpDst = x.HOV.tcpDstPort
          ; SDN_Types.Pattern.inPort = default_port }
        in
        simpl_flow pattern actions) in
    let c = cross m in 
    (* Printf.printf "\nCROSS:\n%!"; *)
    (* List.iter c *)
    (*   ~f:(fun (h,b) ->  *)
    (* 	    Printf.printf "%s : %b\n" *)
    (* 	      (HOV.to_string h) b); *)
    let r = go c in 
    (* Printf.printf "FLOWTABLE:\n%s\n"  *)
    (*   (SDN_Types.string_of_flowTable r); *)
    r

  type i = Local.t

  let compile (sw:switchId) (pol:NetKAT_Types.policy) : i =
    let pol' = Optimize.specialize_policy sw pol in
    Local.of_policy pol'

  module Dep = Algo.Topological(struct
    type t = Pattern.t * Action.Set.t

    let compare (x1,s1) (x2,s2) =
      match Pattern.compare x1 x2 with
        | 0 -> Action.Set.compare s1 s2
        | n -> n

    let dep_compare (x1,s1) (x2,s2) : int =
      (* Printf.printf "DEP_COMPARE:\n"; *)
      (* Printf.printf "  x1=%s=>" (Pattern.to_string x1); *)
      (* Printf.printf "%s\n" (Action.set_to_string s1); *)
      (* Printf.printf "  x2=%s=>" (Pattern.to_string x2); *)
      (* Printf.printf "%s\n" (Action.set_to_string s2); *)
      (* Printf.printf "  o1=%b\n  o2=%b\n" (Pattern.obscures x1 x2) (Pattern.obscures x2 x1); *)
      let r = 
        let ac = Action.Set.compare s1 s2 in
        if ac = 0 then
          0
        else
          let o1 = Pattern.obscures x1 x2 in
          let o2 = Pattern.obscures x2 x1 in
          (* sanity check: no circular dependencies *)
	  if o1 && o2 then 
            begin
              Printf.printf "Circular dependency between\n%s => %s\nand\n%s => %s\n" 
                (Pattern.to_string x1) (Action.set_to_string s1)
                (Pattern.to_string x2) (Action.set_to_string s2);
              assert false
            end;
          if o1 then -1
          else if o2 then 1
          else 0 in 
      (* Printf.printf "r=%d\n" r; *)
      r
  end)

  let to_table ?(optimize_fall_through=true) (m:i) : flowTable =
    Printf.printf "\nTO_TABLE\n";
    List.iter
      (Dep.sort (Pattern.Map.to_alist m))
      ~f:(fun (p,a) ->
           Printf.printf "   %s => %s\n\n" (Pattern.to_string p) (Action.set_to_string a));
    let annotated_table () : (flow * Pattern.t * Action.Set.t) list =
      (* Returns a flow table with each entry annotated with the Pattern.t
       * from which it was generated. *)
      List.concat_map
        ~f:(fun (p,s) -> List.map ~f:(fun x -> (x,p,s)) (expand_rules p s))
        (Dep.sort (Pattern.Map.to_alist m)) in
    let patterns_intersect (p: Pattern.t) (q: Pattern.t) : bool =
      match Pattern.seq p q with
        Some s -> not (Pattern.is_empty s)
      | None -> false in
    let pattern_diff_empty (xp:Pattern.t) (p:Pattern.t) : bool =
      Set.for_all (Pattern.diff xp p) ~f:Pattern.is_empty in
    (* A pattern falls through if it is covered by patterns below it in the
     * table each of which has the same action, and no pattern with a different
     * action intersects it within the range containing the cover. *)
    let rec falls_through
        ((xf,xp,xa): flow * Pattern.t * Action.Set.t)
        (table: (flow * Pattern.t * Action.Set.t) list) : bool =
      match table with
        [] -> false
      | (f,p,a)::t -> (
        if Set.equal xa a then (
          if pattern_diff_empty xp p then true
          else falls_through (xf,xp,xa) t)
        else (
          if patterns_intersect xp p then false
          else falls_through (xf,xp,xa) t)) in
    if optimize_fall_through then
      List.map
        ~f:(fun (x,_,_) -> x)
        (List.fold_right
          ~f:(fun x acc -> if falls_through x acc then acc else (x::acc))
          ~init:[]
          (annotated_table ()))
    else
      List.concat_map (Dep.sort (Pattern.Map.to_alist m))
        ~f:(fun (p,s) -> expand_rules p s)
end

module Local_Optimize = struct
  (*
   * Optimize a flow table by removing rules which are shadowed by other rules.
   *)
  let remove_shadowed_rules (table: flowTable) : flowTable =
    let flow_is_shadowed f t =
      List.exists t
        ~f:(fun x -> SDN_Types.Pattern.less_eq f.pattern x.pattern) in
    List.rev (
      List.fold_left
        ~f:(fun acc x -> if flow_is_shadowed x acc then acc else (x::acc))
        ~init:[]
        table)
end

(* exports *)
type t = RunTime.i

let of_policy sw pol =
  Local.of_policy (Optimize.specialize_policy sw pol)

let to_netkat =
  Local.to_netkat

let compile sw p =
  RunTime.compile sw p

let to_table ?(optimize_fall_through=false) t =
  Local_Optimize.remove_shadowed_rules
    (RunTime.to_table t ~optimize_fall_through:optimize_fall_through)

let to_string = 
  Local.to_string 
