open Core.Std
open Sexplib.Conv
open SDN_Types

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

type 'a posneg =
  | Pos of 'a
  | Neg of 'a
  with sexp

(* PosNeg functor *)
module PosNeg (H:NetKAT_Types.Headers.HEADER) = struct
  (* TODO(jnf): add the universe, so that operations on Pos and Neg
     make sense *)
  module S = Set.Make(H)
  type t = S.t posneg
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
        List.rev
          (S.fold s
             ~init:[]
             ~f:(fun acc x -> (Some x, true)::acc))
      | Neg s ->
        List.rev
          ((None,true)::
              S.fold s
              ~init:[]
              ~f:(fun acc x -> (Some x, false)::acc))
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

  let diff y1 y2 = inter y1 (neg y2)
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
      (* TODO(arjun): I assume this means an exact match /32 *)
      ~ipSrc:(g (fun v -> IP4Src (v, 32l)))
      ~ipDst:(g (fun v -> IP4Dst (v, 32l)))
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
  type t = HPN.t with sexp

  module Set = HPNSet
  module Map = HPNMap

  let compare (x:t) (y:t) : int =
    HPN.compare x y

  let to_string ?init ?sep (x:t) =
    HPN.to_string ?init ?sep x

  (* For [set = {x1, ... , xn}] returns [x1 || ... || xn].
    [set_fold] must be a function to fold over [set].
    [tester x_i] must return a [header_val].
   *)
  let make_disjunction set_fold tester set =
    let open NetKAT_Types in
    set_fold set ~init:False
      ~f:(fun acc x -> Optimize.mk_or acc (Test (tester x)))

  let to_netkat_pred (p:t) : NetKAT_Types.pred =
    let open NetKAT_Types in
    let open HPN in
    (* Maps [Pos {x1, ..., xn}] to [x1 || ... || xn]
       Map [Neg {x1, ..., xn}] to [!(x1 || ... || xn)] *)
    let h set_fold tester = function
      | Pos v -> make_disjunction set_fold tester v
      | Neg v -> Optimize.mk_not (make_disjunction set_fold tester v) in
    let g set_fold tester (acc:pred) f : pred =
      Optimize.mk_and acc (h set_fold tester (Field.get f p)) in
    Fields.fold
      ~init:True
      ~location:(g PNL.S.fold (fun v -> Location v))
      ~ethSrc:(g PN48.S.fold (fun v -> EthSrc v))
      ~ethDst:(g PN48.S.fold (fun v ->EthDst v))
      ~vlan:(g PN16.S.fold (fun v -> Vlan v))
      ~vlanPcp:(g PN8.S.fold (fun v -> VlanPcp v))
      ~ethType:(g PN16.S.fold (fun v -> EthType v))
      ~ipProto:(g PN8.S.fold (fun v -> IPProto v))
      (* TODO(arjun): I assume this means an exact match /32 *)
      ~ipSrc:(g PNIp.S.fold (fun v -> IP4Src (v, 32l)))
      ~ipDst:(g PNIp.S.fold (fun v -> IP4Dst (v, 32l)))
      ~tcpSrcPort:(g PN16.S.fold (fun v -> TCPSrcPort v))
      ~tcpDstPort:(g PN16.S.fold (fun v -> TCPDstPort v))

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

  let diff (x:t) (y:t) : t =
    let open HPN in
    let g c f = c (Field.get f x) (Field.get f y) in
    Fields.map
      ~location:(g PNL.diff)
      ~ethSrc:(g PN48.diff)
      ~ethDst:(g PN48.diff)
      ~vlan:(g PN16.diff)
      ~vlanPcp:(g PN8.diff)
      ~ethType:(g PN16.diff)
      ~ipProto:(g PN8.diff)
      ~ipSrc:(g PNIp.diff)
      ~ipDst:(g PNIp.diff)
      ~tcpSrcPort:(g PN16.diff)
      ~tcpDstPort:(g PN16.diff)

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

  let to_action (a:Action.t) (pto: portId option) : seq =
    (* If an action sets the location to a pipe, ignore all other modifications.
     * They will be applied at the controller by Semantics.eval. Otherwise, the
     * port must be determined either by the pattern or by the action. The pto
     * is the port determined by the pattern, if it exists. If the port is not
     * determinate, we fail though it is technically acceptable to send it out
     * InPort... if SDN_Types exposed that.
     * *)
    match HOV.location a, pto with
      | Some (NetKAT_Types.Pipe(_)), _ ->
        [Controller 128]
      | Some (NetKAT_Types.Physical pt), _
      | None, Some pt ->
        let g h act f =
          match Field.get f a with
            | None -> act
            | Some v -> Modify(h v)::act in
        HOV.Fields.fold
          ~init:[OutputPort pt]
          ~location:(fun act _ -> act)
          ~ethSrc:(g (fun v -> SetEthSrc v))
          ~ethDst:(g (fun v -> SetEthDst v))
          ~vlan:(g (fun v -> SetVlan (Some(v))))
          ~vlanPcp:(g (fun v -> SetVlanPcp v))
          ~ethType:(g (fun v -> SetEthTyp v))
          ~ipProto:(g (fun v -> SetIPProto v))
          ~ipSrc:(g (fun v -> SetIP4Src v))
          ~ipDst:(g (fun v -> SetIP4Dst v))
          ~tcpSrcPort:(g (fun v -> SetTCPSrcPort v))
          ~tcpDstPort:(g (fun v -> SetTCPDstPort v))
      | None, None ->
        failwith "indeterminate location"

  (* XXX(seliopou, jnf) unimplementable actions will still produce bogus
   * outputs. For example, the following policy:
   *
   *   (f := 3; port := 1) | (g := 2; poirt := 2)
   *
   * requires two copies of the packet at the switch, which is not possible.
   * Policies like these must be implemented at the controller.
   * *)
  let set_to_action (s:Action.Set.t) (pto : portId option) : par =
    let f par a = (to_action a pto)::par in
    List.dedup (Action.Set.fold s ~f:f ~init:[])

  let simpl_flow (p : pattern) (a : par) : flow =
    { pattern = p;
      action = [a];
      cookie = 0L;
      idle_timeout = Permanent;
      hard_timeout = Permanent }

  module Expanded(H:NetKAT_Types.Headers.HEADER) = struct
    type t = (H.t option * bool) list with sexp
    let compare = Pervasives.compare
    let equal = (=)
    let to_string hs =
      Printf.sprintf "[%s]" 
        (List.fold_left hs
          ~init:"" ~f:(fun acc (mh, b) ->
            Printf.sprintf "%s(%s,%b)"
            (if acc = "" then acc else "; ")
            (match mh with None -> "None" | Some(h) -> H.to_string h)
            b))
    let is_wild _ = false
  end

  module EHeaders = struct
    include NetKAT_Types.Headers.Make
    (Expanded(NetKAT_Types.LocationHeader))
    (Expanded(NetKAT_Types.Int64Header))
    (Expanded(NetKAT_Types.Int64Header))
    (Expanded(NetKAT_Types.IntHeader))
    (Expanded(NetKAT_Types.IntHeader))
    (Expanded(NetKAT_Types.IntHeader))
    (Expanded(NetKAT_Types.IntHeader))
    (Expanded(NetKAT_Types.Int32Header))
    (Expanded(NetKAT_Types.Int32Header))
    (Expanded(NetKAT_Types.IntHeader))
    (Expanded(NetKAT_Types.IntHeader))

    let empty =
      { location = []; 
        ethSrc = [];
        ethDst = [];
        vlan = [];
        vlanPcp = [];
        ethType = [];
        ipProto = [];
        ipSrc = [];
        ipDst = [];
        tcpSrcPort = [];
        tcpDstPort = [] }
  end

  let expand_rules (x:Pattern.t) (s:Action.Set.t) : flowTable =
    let m : EHeaders.t = let open EHeaders in
      { location = PNL.(expand x.HPN.location);
        ethSrc = PN48.(expand x.HPN.ethSrc);
        ethDst = PN48.(expand x.HPN.ethDst);
        vlan = PN16.(expand x.HPN.vlan);
        vlanPcp = PN8.(expand x.HPN.vlanPcp);
        ethType = PN16.(expand x.HPN.ethType);
        ipProto = PN8.(expand x.HPN.ipProto);
        ipSrc = PNIp.(expand x.HPN.ipSrc);
        ipDst = PNIp.(expand x.HPN.ipDst);
        tcpSrcPort = PN16.(expand x.HPN.tcpSrcPort);
        tcpDstPort = PN16.(expand x.HPN.tcpDstPort);
      }
    in
    (* computes a cross product *)
    let rec cross m : (HOV.t * bool) list =
      let empty = let open HOV in
      { location = None; ethSrc = None; ethDst = None;
        vlan = None; vlanPcp = None; ethType = None;
        ipProto = None; ipSrc = None; ipDst = None;
        tcpSrcPort = None; tcpDstPort = None;
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
      EHeaders.Fields.fold
        ~init:[(empty, true)]
        ~location:(g (fun p o -> { p with HOV.location = o }))
        ~ethSrc:(g (fun p o -> { p with HOV.ethSrc = o }))
        ~ethDst:(g (fun p o -> { p with HOV.ethDst = o }))
        ~vlan:(g (fun p o -> { p with HOV.vlan = o }))
        ~vlanPcp:(g (fun p o -> { p with HOV.vlanPcp = o }))
        ~ethType:(g (fun p o -> { p with HOV.ethType = o }))
        ~ipProto:(g (fun p o -> { p with HOV.ipProto = o }))
        ~ipSrc:(g (fun p o -> { p with HOV.ipSrc = o }))
        ~ipDst:(g (fun p o -> { p with HOV.ipDst= o }))
        ~tcpSrcPort:(g (fun p o -> { p with HOV.tcpSrcPort = o }))
        ~tcpDstPort:(g (fun p o -> { p with HOV.tcpDstPort = o }))
      in

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
          { SDN_Types.dlSrc = x.HOV.ethSrc
          ; SDN_Types.dlDst = x.HOV.ethDst
          ; SDN_Types.dlTyp = x.HOV.ethType
          ; SDN_Types.dlVlan = x.HOV.vlan
          ; SDN_Types.dlVlanPcp = x.HOV.vlanPcp
          ; SDN_Types.nwSrc = x.HOV.ipSrc
          ; SDN_Types.nwDst = x.HOV.ipDst
          ; SDN_Types.nwProto = x.HOV.ipProto
          ; SDN_Types.tpSrc = x.HOV.tcpSrcPort
          ; SDN_Types.tpDst = x.HOV.tcpSrcPort
          ; SDN_Types.inPort = default_port }
        in
        simpl_flow pattern actions) in
    go (cross m)

  type i = Local.t

  let compile (sw:switchId) (pol:NetKAT_Types.policy) : i =
    let pol' = Optimize.specialize_policy sw pol in
    Local.of_policy pol'

  module Dep = Algo.Topological(struct
    type t = Pattern.t * Action.Set.t

    let compare (x1,s1) (x2,s2) : int =
      let pc = Pattern.compare x1 x2 in
      let ac = Action.Set.compare s1 s2 in
      let o1 = Pattern.obscures x1 x2 in
      let o2 = Pattern.obscures x2 x1 in
      (* sanity check: no circular dependencies *)
      assert (not (ac <> 0 && o1 && o2));
      if pc = 0 && ac = 0 then 0
      else if ac = 0 then -pc
      else if o1 then -1
      else 1
  end)

  let to_table ?(optimize_fall_through=true) (m:i) : flowTable =
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
          if Pattern.is_empty (Pattern.diff xp p) then true
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
      List.concat_map (Dep.sort (Pattern.Map.to_alist m)) ~f:(fun (p,s) ->
        expand_rules p s)

end

module Local_Optimize = struct

  (*
   * A pattern p shadows another pattern q if
   *
   *   ∀f, (p(f) ∩ q(f)) = q(f)
   *
   * ... where f ranges over all the fields of the pattern. In other words, p(f)
   * is a broader match than, but includes, q(f).
   *
   * The current reprentation of patterns encodes an any-value wildcard for a
   * field as the absence of the field in the pattern. In addition, patterns do
   * not support prefix matches or any other more complex match, so the
   * intersection is just an equality check on values. The property restated
   * to take into account the pattern representation is then:
   *
   *   ∀f, f ∈ p ⇒ f ∈ q ∧ (p(f) = q(f))
   *)
  let pattern_shadows (p: pattern) (q: pattern) : bool =
    let check m1 m2 =
      match m1 with
        | None -> true
        | Some(v1) ->
          begin match m2 with
            | None -> false
            | Some(v2) -> v1 = v2
          end
    in
    let open SDN_Types in
    check p.dlSrc q.dlSrc && check p.dlDst q.dlDst && check p.dlTyp q.dlTyp
      && check p.dlVlan q.dlVlan && check p.dlVlanPcp q.dlVlanPcp
      && check p.nwSrc q.nwSrc && check p.nwDst q.nwDst
      && check p.nwProto q.nwProto
      && check p.tpSrc q.tpSrc && check p.tpDst q.tpDst
      && check p.inPort q.inPort

  (*
   * Optimize a flow table by removing rules which are shadowed by other rules.
   *)
  let remove_shadowed_rules (table: flowTable) : flowTable =
    let flow_is_shadowed f t =
      List.exists t
        ~f:(fun x -> pattern_shadows x.pattern f.pattern) in
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

let compile =
  RunTime.compile

let to_table ?(optimize_fall_through=false) t =
  Local_Optimize.remove_shadowed_rules
    (RunTime.to_table t ~optimize_fall_through:optimize_fall_through)
