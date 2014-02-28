open Core.Std
open Sexplib.Conv
open SDN_Types

type location = NetKAT_Types.location

(* Option functor *)
module Option (H:NetKAT_Types.Headers.HEADER) =
struct
  type t = H.t option with sexp
  let compare o1 o2 =
    match o1,o2 with
      | Some x1, Some x2 -> H.compare x1 x2
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> 0
  let equal o1 o2 =
    compare o1 o2 = 0
  let to_string o =
    match o with
      | None -> "None"
      | Some x -> Printf.sprintf "Some(%s)" (H.to_string x)
  let is_wild o =
    o = None
end

(* PosNeg functor *)
module PosNeg (H:NetKAT_Types.Headers.HEADER) = struct
  (* TODO(jnf): add the universe, so that operations on Pos and Neg
     make sense *)
  module S = Set.Make(H)
  type t =
      Pos of S.t
    | Neg of S.t
  with sexp
  let singleton x = Pos (S.singleton x)
  let compare y1 y2 = match y1,y2 with
    | Pos _, Neg _ -> -1
    | Neg _, Pos _ -> 1
    | Pos s, Pos s' ->
      S.compare s s'
    | Neg s, Neg s' ->
      -1 * (S.compare s s')
  let equal y1 y2 =
    compare y1 y2 = 0
  let fold y = match y with
    | Pos s -> S.fold s
    | Neg s -> S.fold s
  let to_string y =
    let f acc v =
      Printf.sprintf "%s%s"
        (if acc = "" then acc else acc ^ ", ")
        (H.to_string v) in
    let c = match y with
      | Pos s -> ""
      | Neg s -> "~" in
    Printf.sprintf "%s{%s}"
      c (fold y ~init:"" ~f:f)
  let any =
    Neg (S.empty)
  let is_any y =
    match y with
      | Neg s -> S.is_empty s
      | Pos _ -> false
  let empty =
    Pos (S.empty)
  let is_empty y =
    match y with
      | Pos s ->
        S.is_empty s
      | Neg _ ->
        false
  let is_wild = is_any
  let expand y =
    match y with
      | Pos s ->
        S.fold s
          ~init:[]
          ~f:(fun acc x -> (Some x, true)::acc)
      | Neg s ->
        S.fold s
          ~init:[(None,true)]
          ~f:(fun acc x -> (Some x, false)::acc)
  let subseteq y1 y2 =
    match y1, y2 with
      | Pos s1, Pos s2 ->
        Set.subset s1 s2
      | Pos s1, Neg s2 ->
        Set.is_empty (Set.inter s1 s2)
      | Neg s1, Pos s2 ->
        (* TODO(jnf): true if (univ \ s1) <= s2 *)
        false
      | Neg s1, Neg s2 ->
        Set.subset s2 s1


  (* Intuition for obscures
   *  r1: (f = Neg {v1,...,vk } => s1)
   *  ----
   *  f = v1 => drop
   *   ...
   *  f = vk => drop
   *  f = * => s1
   *  ====
   *  r2:(f = vl => s1)
   *  ----
   *  f = vl => s2
   * We want to have that r1 obscures r2
   *)
  let obscures y1 y2 =
    match y1, y2 with
      | Pos s1, Pos s2 ->
        (* A positive obscures another if their sets have any
           overlap *)
        not (Set.is_empty (Set.inter s1 s2))
      | Pos s1, Neg s2 ->
        (* A positive obscures a negative unless their sets are complementary *)
        not (Set.is_empty (Set.diff s1 s2))
      | Neg _, _ ->
        (* A negative obscures anything else *)
        true

  let inter y1 y2 =
    match y1, y2 with
      | Pos s1, Pos s2 ->
        Pos (Set.inter s1 s2)
      | Pos s1, Neg s2 ->
        Pos (Set.diff s1 s2)
      | Neg s1, Pos s2 ->
        Pos (Set.diff s2 s1)
      | Neg s1, Neg s2 ->
        Neg (Set.union s1 s2)
  let neg y =
    match y with
      | Pos s -> Neg s
      | Neg s -> Pos s
end

module OL = Option(NetKAT_Types.LocationHeader)
module O48 = Option(NetKAT_Types.Int64Header)
module O32 = Option(NetKAT_Types.Int32Header)
module O16 = Option(NetKAT_Types.IntHeader)
module O8 = Option(NetKAT_Types.IntHeader)
module OIp = Option(NetKAT_Types.Int32Header)

module PNL = PosNeg(NetKAT_Types.LocationHeader)
module PN48 = PosNeg(NetKAT_Types.Int64Header)
module PN32 = PosNeg(NetKAT_Types.Int32Header)
module PN16 = PosNeg(NetKAT_Types.IntHeader)
module PN8 = PosNeg(NetKAT_Types.IntHeader)
module PNIp = PosNeg(NetKAT_Types.Int32Header)

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

module HeadersPosNeg =
  NetKAT_Types.Headers.Make
    (PNL)
    (PN48)
    (PN48)
    (PN16)
    (PN8)
    (PN16)
    (PN8)
    (PNIp)
    (PNIp)
    (PN16)
    (PN16)

(* H to the izz-O, V to the izz-A... *)
module HOV = HeadersOptionalValues
module HOVSet = Set.Make(HOV)
module HOVMap = Map.Make(HOV)

module HPN = HeadersPosNeg
module HPNSet = Set.Make(HPN)
module HPNMap = Map.Make(HPN)

module Action = struct

  type t = HOV.t with sexp

  module Set = HOVSet

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
  type t = HPN.t with sexp

  module Set = HPNSet
  module Map = HPNMap

  let compare (x:t) (y:t) : int =
    HPN.compare x y

  let to_string ?init ?sep (x:t) =
    HPN.to_string ?init ?sep x

  let any : t =
    let open HPN in
        { location = PNL.any;
          ethSrc = PN48.any;
          ethDst = PN48.any;
          vlan = PN16.any;
          vlanPcp = PN8.any;
          ethType = PN16.any;
          ipProto = PN8.any;
          ipSrc = PNIp.any;
          ipDst = PNIp.any;
          tcpSrcPort = PN16.any;
          tcpDstPort = PN16.any }

  let empty : t =
    let open HPN in
        { location = PNL.empty;
          ethSrc = PN48.empty;
          ethDst = PN48.empty;
          vlan = PN16.empty;
          vlanPcp = PN8.empty;
          ethType = PN16.empty;
          ipProto = PN8.empty;
          ipSrc = PNIp.empty;
          ipDst = PNIp.empty;
          tcpSrcPort = PN16.empty;
          tcpDstPort = PN16.empty }

  let is_any (x:t) : bool =
    let g is_any f = is_any (Field.get f x) in
    HPN.Fields.for_all
      ~location:(g PNL.is_any)
      ~ethSrc:(g PN48.is_any)
      ~ethDst:(g PN48.is_any)
      ~vlan:(g PN16.is_any)
      ~vlanPcp:(g PN8.is_any)
      ~ethType:(g PN16.is_any)
      ~ipProto:(g PN8.is_any)
      ~ipSrc:(g PNIp.is_any)
      ~ipDst:(g PNIp.is_any)
      ~tcpSrcPort:(g PN16.is_any)
      ~tcpDstPort:(g PN16.is_any)

  let is_empty (x:t) : bool =
    let g is_empty f = is_empty (Field.get f x) in
    HPN.Fields.exists
      ~location:(g PNL.is_empty)
      ~ethSrc:(g PN48.is_empty)
      ~ethDst:(g PN48.is_empty)
      ~vlan:(g PN16.is_empty)
      ~vlanPcp:(g PN8.is_empty)
      ~ethType:(g PN16.is_empty)
      ~ipProto:(g PN8.is_empty)
      ~ipSrc:(g PNIp.is_empty)
      ~ipDst:(g PNIp.is_empty)
      ~tcpSrcPort:(g PN16.is_empty)
      ~tcpDstPort:(g PN16.is_empty)

  let mk_location l = { any with HPN.location = PNL.singleton l }
  let mk_ethSrc n = { any with HPN.ethSrc = PN48.singleton n }
  let mk_ethDst n = { any with HPN.ethDst = PN48.singleton n }
  let mk_vlan n = { any with HPN.vlan = PN16.singleton n }
  let mk_vlanPcp n = { any with HPN.vlanPcp = PN8.singleton n }
  let mk_ethType n = { any with HPN.ethType = PN16.singleton n }
  let mk_ipProto n = { any with HPN.ipProto = PN16.singleton n }
  let mk_ipSrc n = { any with HPN.ipSrc = PNIp.singleton n }
  let mk_ipDst n = { any with HPN.ipDst = PNIp.singleton n }
  let mk_tcpSrcPort n = { any with HPN.tcpSrcPort = PN16.singleton n }
  let mk_tcpDstPort n = { any with HPN.tcpDstPort = PN16.singleton n }

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
    let open HPN in
        let g is_empty neg acc f =
          let z = neg (Field.get f x) in
          if is_empty z then acc
          else Set.add acc (Field.fset f any z) in
        Fields.fold
          ~init:Set.empty
          ~location:PNL.(g is_empty neg)
          ~ethSrc:PN48.(g is_empty neg)
          ~ethDst:PN48.(g is_empty neg)
          ~vlan:PN16.(g is_empty neg)
          ~vlanPcp:PN8.(g is_empty neg)
          ~ethType:PN16.(g is_empty neg)
          ~ipProto:PN8.(g is_empty neg)
          ~ipSrc:PNIp.(g is_empty neg)
          ~ipDst:PNIp.(g is_empty neg)
          ~tcpSrcPort:PN16.(g is_empty neg)
          ~tcpDstPort:PN16.(g is_empty neg)

  let subseteq (x:t) (y:t) : bool =
    let open NetKAT_Types.Headers in
        let g c f = c (Field.get f x) (Field.get f y) in
        HPN.Fields.for_all
          ~location:(g PNL.subseteq)
          ~ethSrc:(g PN48.subseteq)
          ~ethDst:(g PN48.subseteq)
          ~vlan:(g PN16.subseteq)
          ~vlanPcp:(g PN8.subseteq)
          ~ethType:(g PN16.subseteq)
          ~ipProto:(g PN8.subseteq)
          ~ipSrc:(g PNIp.subseteq)
          ~ipDst:(g PNIp.subseteq)
          ~tcpSrcPort:(g PN16.subseteq)
          ~tcpDstPort:(g PN16.subseteq)

  let obscures (x:t) (y:t) : bool =
    let open NetKAT_Types.Headers in
        let g c f = c (Field.get f x) (Field.get f y) in
        HPN.Fields.for_all
          ~location:(g PNL.obscures)
          ~ethSrc:(g PN48.obscures)
          ~ethDst:(g PN48.obscures)
          ~vlan:(g PN16.obscures)
          ~vlanPcp:(g PN8.obscures)
          ~ethType:(g PN16.obscures)
          ~ipProto:(g PN8.obscures)
          ~ipSrc:(g PNIp.obscures)
          ~ipDst:(g PNIp.obscures)
          ~tcpSrcPort:(g PN16.obscures)
          ~tcpDstPort:(g PN16.obscures)

  let seq (x:t) (y:t) : t option =
    let open HPN in
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
          ~location:PNL.(g is_empty inter)
          ~ethSrc:PN48.(g is_empty inter)
          ~ethDst:PN48.(g is_empty inter)
          ~vlan:PN16.(g is_empty inter)
          ~vlanPcp:PN8.(g is_empty inter)
          ~ethType:PN16.(g is_empty inter)
          ~ipProto:PN8.(g is_empty inter)
          ~ipSrc:PNIp.(g is_empty inter)
          ~ipDst:PNIp.(g is_empty inter)
          ~tcpSrcPort:PN16.(g is_empty inter)
          ~tcpDstPort:PN16.(g is_empty inter)

  let seq_act (x:t) (a:Action.t) (y:t) : t option =
    let open HPN in
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
        HPN.Fields.fold
          ~init:(Some any)
          ~location:PNL.(g HOV.location is_empty inter singleton)
          ~ethSrc:PN48.(g HOV.ethSrc is_empty inter singleton)
          ~ethDst:PN48.(g HOV.ethDst is_empty inter singleton)
          ~vlan:PN16.(g HOV.vlan is_empty inter singleton)
          ~vlanPcp:PN8.(g HOV.vlanPcp is_empty inter singleton)
          ~ethType:PN16.(g HOV.ethType is_empty inter singleton)
          ~ipProto:PN8.(g HOV.ipProto is_empty inter singleton)
          ~ipSrc:PNIp.(g HOV.ipSrc is_empty inter singleton)
          ~ipDst:PNIp.(g HOV.ipDst is_empty inter singleton)
          ~tcpSrcPort:PN16.(g HOV.tcpSrcPort is_empty inter singleton)
          ~tcpDstPort:PN16.(g HOV.tcpDstPort is_empty inter singleton)
end

module Optimize = struct
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
        | NetKAT_Types.Test (NetKAT_Types.Switch v) ->
          if v = sw then
            k NetKAT_Types.True
          else
            k NetKAT_Types.False
        | NetKAT_Types.Test _ ->
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
        | NetKAT_Types.Mod hv ->
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

module Local = struct

  type t = Action.Set.t Pattern.Map.t

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

    let seq_act r1 a q =
      Pattern.Map.fold q
        ~init:Pattern.Map.empty
        ~f:(fun ~key:r2 ~data:s2 acc ->
          match Pattern.seq_act r1 a r2 with
            | None ->
              acc
            | Some r12 ->
              extend r12 (Action.set_seq a s2) acc) in

    let seq_atom_acts_local r1 s1 q =
      if Action.Set.is_empty s1 then
        Pattern.Map.singleton r1 s1
      else
        Action.Set.fold s1
          ~init:Pattern.Map.empty
          ~f:(fun acc a ->
            let acc' = seq_act r1 a q in
            Pattern.Map.merge ~f:merge acc acc') in

    let r =
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
    let rec loop acc pi =
      let psucci = seq p pi in
      let acc' = par acc psucci in
      if compare acc acc' = 0 then
        acc
      else
        loop acc' psucci in
    let p0 = Pattern.Map.singleton Pattern.any Action.id in
    let r = loop p0 p0 in
    (* debug "### STAR ###\n%s\n%s" *)
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
              Pattern.mk_ipSrc n
            | NetKAT_Types.IP4Dst (n,m) ->
              Pattern.mk_ipDst n
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
              Action.mk_ipSrc n
            | NetKAT_Types.IP4Dst (n,m) ->
              Action.mk_ipDst n
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

  let to_action (a:Action.t) (pto: fieldVal option) : seq =
    let i8 x = VInt.Int8 x in
    let i16 x = VInt.Int16 x in
    let i32 x = VInt.Int32 x in
    let i48 x = VInt.Int64 x in
    let port = match HOV.location a, pto with
      | Some (NetKAT_Types.Physical pt),_ ->
        VInt.Int32 pt
      | _, Some pt ->
        pt
      | _, None ->
        failwith "indeterminate port" in
    let g h c act f =
      match Field.get f a with
        | None -> act
        | Some v -> SetField(h,c v)::act in
    HOV.Fields.fold
      ~init:[OutputPort port]
      ~location:(fun act _ -> act)
      ~ethSrc:(g EthSrc i48)
      ~ethDst:(g EthDst i48)
      ~vlan:(g Vlan i16)
      ~vlanPcp:(g VlanPcp i8)
      ~ethType:(g EthType i16)
      ~ipProto:(g IPProto i8)
      ~ipSrc:(g IP4Src i32)
      ~ipDst:(g IP4Src i32)
      ~tcpSrcPort:(g TCPSrcPort i16)
      ~tcpDstPort:(g TCPDstPort i16)

  let set_to_action (s:Action.Set.t) (pto : fieldVal option) : par =
    let f par a = (to_action a pto)::par in
    Action.Set.fold s ~f:f ~init:[]

  let expand_rules (x:Pattern.t) (s:Action.Set.t) : (pattern * par) list =
    let i8 x = VInt.Int8 x in
    let i16 x = VInt.Int16 x in
    let i32 x = VInt.Int32 x in
    let i48 x = VInt.Int64 x in
    let il x = match x with
      | NetKAT_Types.Pipe _ ->
        failwith "indeterminate port"
      | NetKAT_Types.Physical n ->
        VInt.Int32 n in
    let g os c h acc f =
      let v = Field.get f x in
      let l =
        List.map (os v)
          ~f:(fun (o,b) ->
            match o with
              | Some v -> (Some (c v), b)
              | None -> (None, b)) in
      FieldMap.add h l acc in
    let m : ((fieldVal option * bool) list) FieldMap.t =
      HPN.Fields.fold
        ~init:FieldMap.empty
        ~location:PNL.(g expand il InPort)
        ~ethSrc:PN48.(g expand i48 EthSrc)
        ~ethDst:PN48.(g expand i48 EthDst)
        ~vlan:PN16.(g expand i16 Vlan)
        ~vlanPcp:PN8.(g expand i8 VlanPcp)
        ~ethType:PN16.(g expand i16 EthType)
        ~ipProto:PN8.(g expand i8 IPProto)
        ~ipSrc:PNIp.(g expand i32 IP4Src)
        ~ipDst:PNIp.(g expand i32 IP4Dst)
        ~tcpSrcPort:PN16.(g expand i16 TCPSrcPort)
        ~tcpDstPort:PN16.(g expand i16 TCPDstPort) in
    let rec loop m acc : (fieldVal option FieldMap.t * bool) list =
      if FieldMap.is_empty m then
        acc
      else
        let h,l = FieldMap.min_binding m in
        let r = loop (FieldMap.remove h m) acc in
        List.fold_left l
          ~init:[]
          ~f:(fun acc (o,b) ->
            List.map r
              ~f:(fun (p,c) -> (FieldMap.add h o p, b && c))) in
    let go l =
      List.fold_left l
        ~init:[]
        ~f:(fun acc (x,b) ->
          let pto = FieldMap.find InPort x in
          let a = if b then set_to_action s pto else [] in
          let y =
            FieldMap.fold
              (fun h o acc ->
                match o with
                  | None -> acc
                  | Some v -> FieldMap.add h v acc)
              x FieldMap.empty in
          ((y,a)::acc)) in
    go (loop m [(FieldMap.empty,true)])

  type i = Local.t

  let compile (sw:switchId) (pol:NetKAT_Types.policy) : i =
    let pol' = Optimize.specialize_policy sw pol in
    let n,n' = Semantics.size pol, Semantics.size pol' in
    Printf.printf " [compression: %d -> %d = %.3f]\n\n"
      n n' (Float.of_int n' /. Float.of_int n);
    Local.of_policy pol'

  let dep_compare (x1,s1) (x2,s2) : int =
    let pc = Pattern.compare x1 x2 in
    let ac = Action.Set.compare s1 s2 in
    let o1 = Pattern.obscures x1 x2 in
    let o2 = Pattern.obscures x2 x1 in
    (* sanity check: no circular dependencies *)
    assert (not (ac <> 0 && o1 && o2));
    if pc = 0 && ac = 0 then 0
    else if ac = 0 then pc
    else if o2 then -1
    else 1

  let dep_sort (p:i) : (Pattern.t * Action.Set.t) list =
    List.sort
      (Pattern.Map.fold p
         ~init:[]
         ~f:(fun ~key:x ~data:s acc -> (x,s)::acc))
      ~cmp:dep_compare

  let simpl_flow (p : pattern) (a : par) : flow =
    { pattern = p;
      action = [a];
      cookie = 0L;
      idle_timeout = Permanent;
      hard_timeout = Permanent }

  let to_table (m:i) : flowTable =
    let rec loop l acc =
      match l with
        | [] ->
          List.fold_left acc
            ~init:[]
            ~f:(fun acc r ->
              List.fold_right r
                ~init:acc
                ~f:(fun (p,a) acc -> (simpl_flow p a)::acc))
        | (p,s)::rest ->
          loop rest (expand_rules p s::acc) in
    loop (dep_sort m) []
end

(* exports *)
type t = RunTime.i

let of_policy sw pol =
  Local.of_policy (Optimize.specialize_policy sw pol)

let to_netkat _ =
  failwith "NYI"

let compile =
  RunTime.compile

let decompile _ =
  failwith "NYI"

let to_table =
  RunTime.to_table
