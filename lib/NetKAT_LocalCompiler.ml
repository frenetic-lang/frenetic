open Core.Std

module SDN = SDN_Types


(** Packet field.

    Packet fields are the variables that network functions are defined over.
    This module implements the the [Variable] signature from the Tdk package. *)
module Field = struct

  type t
    = Switch
    | Vlan
    | VlanPcp
    | EthType
    | IPProto
    | EthSrc
    | EthDst
    | IP4Src
    | IP4Dst
    | TCPSrcPort
    | TCPDstPort
    | Location
    with sexp
  (** The type of packet fields. This is an enumeration whose ordering has an
      effect on the performance of Tdk operations, as well as the size of the
      flowtables that the compiler will produce. *)

  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let to_string = function
    | Switch -> "Switch"
    | Location -> "Location"
    | EthSrc -> "EthSrc"
    | EthDst -> "EthDst"
    | Vlan -> "Vlan"
    | VlanPcp -> "VlanPcp"
    | EthType -> "EthType"
    | IPProto -> "IPProto"
    | IP4Src -> "IP4Src"
    | IP4Dst -> "IP4Dst"
    | TCPSrcPort -> "TCPSrcPort"
    | TCPDstPort -> "TCPDstPort"
end

(** Packet field values.

    Each packet field can take on a certain range of values that in general have
    a lattice structure. This sometimes enables multiple tests on fields to be
    compressed into a single test. This module implements the [Lattice]
    siganture from the Tdk package. *)
module Value = struct

  type t
    = Const of Int64.t
    | Mask of Int64.t * int
    | Pipe of string
    | Query of string
    with sexp
  (** The packet field value type. This is a union of all the possible values
      that all fields can take on. All integer bit widths are represented by an
      [Int64.t] and will be cast to the appropriate bit width for use during
      final translation to flowtables.

      A simple bitmask variant is also supported. [Mask(n, m)] indicates that
      the first [m] bits of the value [n] are fixed, while the rest should be
      treated as wildcards.

      Because this is a big union of possible value types, it's possible for the
      programmer to construct [(Field.t, Value.t)] pairs that do not make any
      sense, e.g., [(Field.EthSrc, Value.Pipe "learn")]. This will be detected
      during flowtable generation, though the syntax of the NetKAT language will
      prevent programs from generating these ill-formed predicates. *)

  let subset_eq a b =
    (* A partial order on values that should be reflexive, transitive, and
       antisymmetric. This should also satisfy certain properites related to
       [join] and [meet] which will be mentioned along with those functions. *)
    let subset_eq_mask a m b n =
      if m < n
        then false
        else Int64.(shift_right_logical a m) = Int64.(shift_right_logical b m)
    in
    match a, b with
    | Const  a   , Const b
    | Mask(a, 64), Const b -> a = b
    | Pipe   a   , Pipe  b
    | Query  a   , Query b -> a = b
    | Mask     _ , Const _
    | Pipe     _ ,       _
    | Query    _ ,       _
    | _          , Pipe  _
    | _          , Query _ -> false
    | Mask(a, m) , Mask(b, n) -> subset_eq_mask a m  b n
    | Const a    , Mask(b, n) -> subset_eq_mask a 64 b n

  let meet ?(tight=false) a b =
    (* Determines the greatest lower bound of two elements, if one exists. This
       operation should be associative, commutative, and idempotent. If [tight]
       is false, then this is the typical meet operation on a lattice. If
       [tight] is true, then the retuned value [r] must in addition satisfy the
       following property:

         ∀x, [subset_eq r x] <=> [subset_eq a x || subset_eq b x || equal r x].

       In other words, any elements related to [r] should do so transitively
       through [a] or [b], or be equal to [r] itself. *)
    let meet_mask a m b n =
      let lt = subset_eq (Mask(a, m)) (Mask(b, n)) in
      let gt = subset_eq (Mask(b, n)) (Mask(a, m)) in
      if lt && gt then
        Some(Mask(a, m))
      else if lt then
        if (not tight) || (m = (n + 1)) then Some(Mask(a, m)) else None
      else if gt then
        if (not tight) || (n = (m + 1)) then Some(Mask(b, n)) else None
      else
        if (not tight) || m = n then
          let x, y = (Mask(a, m + 1), Mask(b, n + 1)) in
          if subset_eq x y && subset_eq y x then Some(x) else None
        else
          None (* XXX(seliopou): complete definition *)
    in
    match a, b with
    | Const  a   , Const b
    | Mask(a, 64), Const b -> if a = b then Some(Const a) else None
    | Pipe   a   , Pipe  b -> if a = b then Some(Pipe a) else None
    | Query  a   , Query b -> if a = b then Some(Query a) else None
    | Mask     _ , Const _
    | Pipe     _ ,       _
    | Query    _ ,       _
    | _          , Pipe  _
    | _          , Query _ -> None
    | Mask(a, m) , Mask(b, n) -> meet_mask a m  b n
    | Const a, Mask(b, n)     -> meet_mask a 64 b n

  let join ?(tight=false) a b =
    (* Determines the least upper bound of two elements, if one exists. This
       operation should be associative, commutative, and idempotent. If [tight]
       is false, then this is the typical join operation on a lattice. If
       [tight] is true, then the retuned value [r] must in addition satisfy the
       following property:

         ∀x, [subset_eq x r] <=> [subset_eq x a || subset_eq x b || equal x r].

       In other words, any elements related to [r] should do so transitively
       through [a] or [b], or be equal to [r] itself. *)
    let join_mask a m b n =
      let lt = subset_eq (Mask(a, m)) (Mask(b, n)) in
      let gt = subset_eq (Mask(b, n)) (Mask(a, m)) in
      if lt && gt then
        Some(Mask(a, m))
      else if lt then
        if (not tight) || (n = (m - 1)) then Some(Mask(b, n)) else None
      else if gt then
        if (not tight) || (m = (n - 1)) then Some(Mask(a, m)) else None
      else
        if (not tight) || m = n then
          let x, y = (Mask(a, m - 1), Mask(b, n - 1)) in
          if subset_eq x y && subset_eq y x then Some(x) else None
        else
          None (* XXX(seliopou): complete definition *)
    in
    match a, b with
    | Const  a   , Const b
    | Mask(a, 64), Const b -> if a = b then Some(Const a) else None
    | Pipe   a   , Pipe  b -> if a = b then Some(Pipe a) else None
    | Query  a   , Query b -> if a = b then Some(Query a) else None
    | Mask     _ , Const _
    | Pipe     _ ,       _
    | Query    _ ,       _
    | _          , Pipe  _
    | _          , Query _ -> None
    | Mask(a, m) , Mask(b, n) -> join_mask a m  b n
    | Const a, Mask(b, n)     -> join_mask a 64 b n

  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let to_string = function
    | Const(a)   -> Printf.sprintf "Const(%Lu)" a
    | Mask(a, m) -> Printf.sprintf "Mask(%Lu, %d)" a m
    | Pipe(p) -> Printf.sprintf "Pipe(%s)" p
    | Query(p) -> Printf.sprintf "Query(%s)" p

  let of_int   t = Const (Int64.of_int   t)
  let of_int32 t = Const (Int64.of_int32 t)
  let of_int64 t = Const t
end

exception FieldValue_mismatch of Field.t * Value.t
exception Non_local


(* Packet patterns.

   This module contains operations related to the deicsion variables of the
   diagram used by the compiler, including functions to convert to and from the
   [header_value], building up flow tables. *)
module Pattern = struct
  type t = Field.t * Value.t

  let compare a b =
    let c = Field.compare a b in
    if c <> 0 then c else Value.compare a b

  let to_string (f, v) =
    Printf.sprintf "%s = %s" (Field.to_string f) (Value.to_string v)

  let to_int = Int64.to_int_exn
  let to_int32 = Int64.to_int32_exn

  module NetKAT = NetKAT_Types

  let of_hv hv =
    let open NetKAT in
    match hv with
    | Switch sw_id -> (Field.Switch, Value.(Const sw_id))
    | Location(Physical p) -> (Field.Location, Value.of_int32 p)
    | Location(Pipe p)  -> (Field.Location, Value.(Pipe p))
    | Location(Query p) -> (Field.Location, Value.(Query p))
    | EthSrc(dlAddr) -> (Field.EthSrc, Value.(Const dlAddr))
    | EthDst(dlAddr) -> (Field.EthDst, Value.(Const dlAddr))
    | Vlan(vlan) -> (Field.Vlan, Value.of_int vlan)
    | VlanPcp(vlanPcp) -> (Field.VlanPcp, Value.of_int vlanPcp)
    | EthType(dlTyp) -> (Field.EthType, Value.of_int dlTyp)
    | IPProto(nwProto) -> (Field.IPProto, Value.of_int nwProto)
    | IP4Src(nwAddr, mask) ->
      (Field.IP4Src, Value.(Mask(Int64.of_int32 nwAddr, 32 + (Int32.to_int_exn mask))))
    | IP4Dst(nwAddr, mask) ->
      (Field.IP4Dst, Value.(Mask(Int64.of_int32 nwAddr, 32 + (Int32.to_int_exn mask))))
    | TCPSrcPort(tpPort) -> (Field.TCPSrcPort, Value.of_int tpPort)
    | TCPDstPort(tpPort) -> (Field.TCPDstPort, Value.of_int tpPort)

  let to_hv (f, v) =
    let open Field in
    let open Value in
    match f, v with
    | (Switch  , Const sw) -> NetKAT.Switch sw
    | (Location, Const p) -> NetKAT.(Location (Physical (to_int32 p)))
    | (Location, Pipe  p) -> NetKAT.(Location (Pipe p))
    | (Location, Query q) -> NetKAT.(Location (Query q))
    | (EthSrc  , Const dlAddr) -> NetKAT.(EthSrc dlAddr)
    | (EthDst  , Const dlAddr) -> NetKAT.(EthDst dlAddr)
    | (Vlan    , Const vlan) -> NetKAT.(Vlan(to_int vlan))
    | (VlanPcp , Const vlanPcp) -> NetKAT.(VlanPcp (to_int vlanPcp))
    | (EthType , Const dlTyp) -> NetKAT.(EthType (to_int dlTyp))
    | (IPProto , Const nwProto) -> NetKAT.(IPProto (to_int nwProto))
    | (IP4Src  , Mask(nwAddr, mask)) -> NetKAT.(IP4Src(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Src  , Const nwAddr) -> NetKAT.(IP4Src(to_int32 nwAddr, 32l))
    | (IP4Dst  , Mask(nwAddr, mask)) -> NetKAT.(IP4Dst(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Dst  , Const nwAddr) -> NetKAT.(IP4Dst(to_int32 nwAddr, 32l))
    | (TCPSrcPort, Const tpPort) -> NetKAT.(TCPSrcPort(to_int tpPort))
    | (TCPDstPort, Const tpPort) -> NetKAT.(TCPDstPort(to_int tpPort))
    | _, _ -> raise (FieldValue_mismatch(f, v))

  let to_pred (f, v) =
    NetKAT_Types.Test (to_hv (f, v))

  let to_sdn (f, v) : SDN.Pattern.t -> SDN.Pattern.t =
    (* Converts a [Pattern.t] into a function that will modify a [SDN.Pattern.t]
       to check the condition represented by the [Pattern.t]. *)
    let open Field in
    let open Value in
    match f, v with
    | (Switch, Const _) -> assert false
    | (Switch, v)       -> raise (FieldValue_mismatch(Switch, v))
    | (Location, Const p) -> fun pat ->
      { pat with SDN.Pattern.inPort = Some(to_int32 p) }
    | (EthSrc, Const dlAddr) -> fun pat ->
      { pat with SDN.Pattern.dlSrc = Some(dlAddr) }
    | (EthDst, Const dlAddr) -> fun pat ->
      { pat with SDN.Pattern.dlDst = Some(dlAddr) }
    | (Vlan    , Const vlan) -> fun pat ->
      { pat with SDN.Pattern.dlVlan = Some(to_int vlan) }
    | (VlanPcp , Const vlanPcp) -> fun pat ->
      { pat with SDN.Pattern.dlVlanPcp = Some(to_int vlanPcp) }
    | (EthType, Const dlTyp) -> fun pat ->
      { pat with SDN.Pattern.dlTyp = Some(to_int dlTyp) }
    | (IPProto , Const nwProto) -> fun pat ->
      { pat with SDN.Pattern.nwProto = Some(to_int nwProto) }
    | (IP4Src  , Mask(nwAddr, mask)) -> fun pat ->
      { pat with SDN.Pattern.nwSrc =
          Some(to_int32 nwAddr, Int32.of_int_exn (mask - 32)) }
    | (IP4Src  , Const nwAddr) -> fun pat ->
      { pat with SDN.Pattern.nwSrc = Some(to_int32 nwAddr, 0l) }
    | (IP4Dst  , Mask(nwAddr, mask)) -> fun pat ->
      { pat with SDN.Pattern.nwDst =
          Some(to_int32 nwAddr, Int32.of_int_exn (mask - 32)) }
    | (IP4Dst  , Const nwAddr) -> fun pat ->
      { pat with SDN.Pattern.nwDst = Some(to_int32 nwAddr, 0l) }
    | (TCPSrcPort, Const tpPort) -> fun pat ->
      { pat with SDN.Pattern.tpSrc = Some(to_int tpPort) }
    | (TCPDstPort, Const tpPort) -> fun pat ->
      { pat with SDN.Pattern.tpDst = Some(to_int tpPort) }
    | _, _ -> raise (FieldValue_mismatch(f, v))

end

(* Packet actions

   This module impelements packet actions for NetKAT. They are modeled as a set
   of maps from fields to values. The inner maps represent a sequential
   composition of field modifications. The outer set represents a parallel
   composition of sequential compositions. *)
module Action = struct

  module Seq = Map.Make(struct
    type t = Field.t with sexp
    let compare = Field.compare
  end)

  module Par = Set.Make(struct
    type t = Value.t Seq.t with sexp
    let compare = Seq.compare_direct Value.compare
  end)

  type t = Par.t with sexp

  let one = Par.singleton Seq.empty
  let zero = Par.empty

  let sum (a:t) (b:t) : t =
    (* This implements parallel composition specifically for NetKAT
       modifications. *)
    if Par.is_empty a then b            (* 0 + p = p *)
    else if Par.is_empty b then a       (* p + 0 = p *)
    else Par.union a b

  let prod (a:t) (b:t) : t =
    (* This implements sequential composition specifically for NetKAT
       modifications and makes use of NetKAT laws to simplify results.*)
    if Par.is_empty a then zero         (* 0; p == 0 *)
    else if Par.is_empty b then zero    (* p; 0 == 0 *)
    else if Par.equal a one then b      (* 1; p == p *)
    else if Par.equal b one then a      (* p; 1 == p *)
    else
      Par.fold a ~init:zero ~f:(fun acc seq1 ->
        let r = Par.map b ~f:(fun seq2 ->
          (* Favor modifications to the right *)
          Seq.merge seq1 seq2 ~f:(fun ~key m ->
            match m with | `Both(_, v) | `Left v | `Right v -> Some(v)))
        in
        Par.union acc r)

  let negate t : t =
    (* This implements negation for the [zero] and [one] actions. Any
       non-[zero] action will be mapped to [zero] by this function. *)
    if compare t zero = 0 then one else zero

  let to_sdn ?(in_port:Int64.t option) (t:t) : SDN.par =
    (* Convert a NetKAT action to an SDN action. At the moment this function
       assumes that fields are assigned to proper bitwidth integers, and does
       no validation along those lines. If the input is derived from a NetKAT
       surface syntax program, then this assumption likely holds. *)
    let to_int = Int64.to_int_exn in
    let to_int32 = Int64.to_int32_exn in
    let t = Par.filter t ~f:(fun seq ->
      (* Queries are equivalent to drop, so remove any [Seq.t]'s from the
       * [Par.t] that set the location to a query. *)
      match Seq.find seq Field.Location with
      | Some(Value.Query _) -> false
      | _                   -> true)
    in
    let to_port p = match in_port with
      | Some(p') when p = p' -> SDN.InPort
      | _                    -> SDN.(Physical(to_int32 p))
    in
    Par.fold t ~init:[] ~f:(fun acc seq ->
      let open Field in
      let open Value in
      let init =
        match Seq.find seq Location with
        | None           -> [SDN.(Output(InPort))]
        | Some (Const p) -> [SDN.(Output(to_port p))]
        | Some (Pipe  _) -> [SDN.(Output(Controller 128))]
        | Some (Query _) -> assert false
        | Some mask      -> raise (FieldValue_mismatch(Location, mask))
      in
      Seq.fold (Seq.remove seq Location) ~init ~f:(fun ~key ~data acc ->
        match key, data with
        | Switch  , _ -> raise (FieldValue_mismatch(Switch, data))
        | Location, _ -> assert false
        | EthSrc  , Const dlAddr  -> SDN.(Modify(SetEthSrc dlAddr)) :: acc
        | EthDst  , Const dlAddr  -> SDN.(Modify(SetEthDst dlAddr)) :: acc
        | Vlan    , Const vlan    -> SDN.(Modify(SetVlan(Some(to_int vlan)))) :: acc
        | VlanPcp , Const vlanPcp -> SDN.(Modify(SetVlanPcp (to_int vlanPcp))) :: acc
        | EthType , Const dlTyp   -> SDN.(Modify(SetEthTyp (to_int dlTyp))) :: acc
        | IPProto , Const nwProto -> SDN.(Modify(SetIPProto (to_int nwProto))) :: acc
        | IP4Src  , Mask (nwAddr, 64)
        | IP4Src  , Const nwAddr   -> SDN.(Modify(SetIP4Src(to_int32 nwAddr))) :: acc
        | IP4Dst  , Mask (nwAddr, 64)
        | IP4Dst  , Const nwAddr   -> SDN.(Modify(SetIP4Dst(to_int32 nwAddr))) :: acc
        | TCPSrcPort, Const tpPort -> SDN.(Modify(SetTCPSrcPort(to_int tpPort))) :: acc
        | TCPDstPort, Const tpPort -> SDN.(Modify(SetTCPDstPort(to_int tpPort))) :: acc
        | _, _ -> raise (FieldValue_mismatch(key, data))
      ) :: acc)

  let demod (f, v) t =
    Par.fold t ~init:zero ~f:(fun acc seq ->
      let seq' = match Seq.find seq f with
        | Some(v')
            when Value.compare v v' = 0 -> Seq.remove seq f
        | _                             -> seq
      in
      sum acc (Par.singleton seq'))

  let to_policy t =
    let open NetKAT_Types in
    Par.fold t ~init:drop ~f:(fun acc seq ->
      let seq' = Seq.fold seq ~init:id ~f:(fun ~key ~data acc ->
        let hv = match Pattern.to_hv (key, data) with
          | IP4Src(nwAddr, 32l) -> IP4Src(nwAddr, 32l)
          | IP4Dst(nwAddr, 32l) -> IP4Dst(nwAddr, 32l)
          | IP4Src _ | IP4Dst _ -> raise (FieldValue_mismatch(key, data))
          | hv -> hv
        in
        Optimize.mk_seq (Mod(hv)) acc)
      in
      Optimize.mk_union seq' acc)

  let iter_fv t ~f =
    Par.iter t ~f:(fun seq ->
      Seq.iter seq ~f:(fun ~key ~data -> f key data))

  let pipes t =
    let module S = Set.Make(String) in
    let s = ref S.empty in
    iter_fv t ~f:(fun key data ->
      match key, data with
      | Field.Location, Value.Pipe q -> s := S.add !s q
      | _, _ -> ());
    !s

  let queries t =
    let module S = Set.Make(String) in
    let s = ref S.empty in
    iter_fv t ~f:(fun key data ->
      match key, data with
      | Field.Location, Value.Query q -> s := S.add !s q
      | _, _ -> ());
    S.to_list !s

  let hash t =
    (* XXX(seliopou): Hashtbl.hash does not work because the same set can have
     * multiple representations. Pick a better hash function. *)
    Hashtbl.hash (List.map (Par.to_list t) ~f:(fun seq -> Seq.to_alist seq))

  let compare =
    Par.compare

  let size =
    Par.fold ~init:0 ~f:(fun acc seq -> acc + (Seq.length seq))

  let to_string t =
    Printf.sprintf "[%s]" (SDN.string_of_par (to_sdn t))
end

module Repr = struct
  module T = Tdk.Vlr.Make(Field)(Value)(Action)

  type t = T.t

  let of_test hv =
    T.atom (Pattern.of_hv hv) Action.one Action.zero

  let of_mod hv =
    let k, v = Pattern.of_hv hv in
    T.const Action.(Par.singleton (Seq.singleton k v))

  let restrict hv t =
    T.restrict [Pattern.of_hv hv] t

  let cond v t f =
    if T.equal t f then
      t
    else
      T.(sum (prod (atom v Action.one Action.zero) t)
             (prod (atom v Action.zero Action.one) f))

  let seq t u =
    (* Compute the sequential composition of [t] and [u] as a fold over [t]. In
       the case of a leaf node, each sequence [seq] of modifications is used to
       [restrict] the diagram for [u] and produce a new diagram [u'] that
       assumes (but does not explicitly represent) the state of the packet after
       passing through [t]'s modifications. [seq] and [u'] are then mulitplied as
       decision diagrams, which will distribute [seq] to all the leaf nodes of
       [u'] to produce the result. All such [seq]s in the [par] are then summed
       together.

       In the case of a branch node, the true and false branches are combined so
       that packets satisfying [v] are handled by the true branch, and packets
       not satisfying [v] are handled by the false branch. *)
    match T.peek u with
    | Some _ -> T.prod t u (* This is an optimization. If [u] is an
                              [Action.Par.t], then it will compose with [t]
                              regardless of however [t] modifies packets. None
                              of the decision variables in [u] need to be
                              removed because there are none. *)
    | None   ->
      T.fold
        (fun par ->
          Action.Par.fold par ~init:(T.const Action.zero) ~f:(fun acc seq ->
            let u' = T.restrict Action.Seq.(to_alist seq) u in
            T.(sum (prod (const Action.Par.(singleton seq)) u') acc)))
        (fun v t f -> cond v t f)
      t

  let union t u =
    (* Compute the union of [t] and [u] by using the sum operation. This will
       appropriately combine actions for overlapping patterns. *)
    if T.equal t u then
      t
    else
      T.sum t u

  let star t =
    (* Compute [star t] by iterating to a fixed point.

       NOTE that the equality check is not semantic equivalence, so this may not
       terminate when expected. In practice though, it should. *)
    let rec loop acc =
      let acc' = union (T.const Action.one) (seq t acc) in
      if T.equal acc acc'
        then acc
        else loop acc'
    in
    loop t

  let rec of_pred p =
    let open NetKAT_Types in
    match p with
    | True      -> T.const Action.one
    | False     -> T.const Action.zero
    | Test(hv)  -> of_test hv
    | And(p, q) -> T.prod (of_pred p) (of_pred q)
    | Or (p, q) -> T.sum (of_pred p) (of_pred q)
    | Neg(q)    -> T.map_r Action.negate (of_pred q)

  let rec of_policy p =
    let open NetKAT_Types in
    match p with
    | Filter   p  -> of_pred p
    | Mod      m  -> of_mod  m
    | Union(p, q) -> union (of_policy p) (of_policy q)
    | Seq  (p, q) -> seq   (of_policy p) (of_policy q)
    | Star p      -> star  (of_policy p)
    | Link _ -> raise Non_local

  let to_policy =
    T.fold
      (fun r -> Action.to_policy r)
      (fun v t f ->
        let p = Pattern.to_pred v in
        let open NetKAT_Types in
        match t, f with
        | Filter t, Filter f ->
          Optimize.(mk_filter (mk_or (mk_and p t)
                                     (mk_and (mk_not p) f)))
        | _       , _        ->
          Optimize.(mk_union (mk_seq (mk_filter p) t)
                             (mk_seq (mk_filter (mk_not p)) f)))

  let equal =
    T.equal

  let to_string =
    T.to_string
end

include Repr

let compile =
  of_policy

let to_table sw_id t =
  (* Convert a [t] to a flowtable for switch [sw_id]. This is implemented as a
     fold over the [t]. Leaf nodes emit a single rule flowtable that mach all
     packets and perform the action represented by the [Action.t] at the leaf.

     Branch nodes convert the variable [v] to a function [guard] that modifies a
     pattern to check for the condition represented by that [v]. The [guard]
     function is then mapped across the flowtable generated from the true branch
     of the node. That result, together with the flowtable for the false branch
     are appended to produce the flowtable at that node.

     No additional guarding is necessary to ensure that no unintended packets
     hit the flowtable for the false branch. All flowtables generated by this
     algorithm will forward or drop all packets; they will not allow any packets
     to fall through. This is true in the base case of a leaf node. Inductively,
     this is also holds for the true and false tables of a branch node. Guarding
     the true table with the pattern represented by [v] means that it will match
     all packets that satisfy [v]. The false branch will therefore only apply to
     packets that don't satisfy [v]. Appending the guarded true table and the
     unguarded false tables will produce a table that will match all packets. *)
  let mk_flow pattern action =
    let open SDN in
    { pattern
    ; action
    ; cookie = 0L
    ; idle_timeout = Permanent
    ; hard_timeout = Permanent
    }
  in
  let ft = Repr.T.fold
    (fun r -> [(SDN.Pattern.match_all, None, r)])
    (fun v t f ->
      let t' = List.map t ~f:(fun (pattern, in_port, action) ->
        let in_port = match v with
          | (Field.Location, Value.Const p) -> Some(p)
          | _ -> in_port
        in
        (Pattern.to_sdn v pattern, in_port, Action.demod v action))
      in
      t' @ f)
    Repr.T.(restrict [(Field.Switch, Value.Const sw_id)] t)
  in
  List.map ft ~f:(fun (pattern, in_port, action) ->
    mk_flow pattern [Action.to_sdn ?in_port action])

let pipes t =
  let module S = Set.Make(String) in
  let ps = Repr.T.fold
    (fun r -> Action.pipes r)
    (fun _ t f -> S.union t f)
    t
  in
  S.to_list ps

let queries t =
  let module S = Set.Make(struct
    type t = string * NetKAT_Types.pred sexp_opaque with sexp
    let compare = Pervasives.compare
  end) in
  let qs = Repr.T.fold
    (fun r ->
      let qs = Action.queries r in
      S.of_list (List.map qs ~f:(fun q -> (q, NetKAT_Types.True))))
    (fun v t f ->
      let p = Pattern.to_pred v in
      let open Optimize in
      S.(union (map t ~f:(fun (q, p') -> (q, mk_and p p')))
               (map t ~f:(fun (q, p') -> (q, mk_and (mk_not p) p')))))
    t
  in
  S.to_list qs

let size =
  Repr.T.fold
    (fun r -> 1)
    (fun v t f -> 1 + t + f)
