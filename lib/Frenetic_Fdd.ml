open Core.Std

module SDN = Frenetic_OpenFlow

(** Packet field.

    Packet fields are the variables that network functions are defined over.
    This module implements the the [Variable] signature from the Tdk package. *)
module Field = struct

  type t
    = Switch
    | Vlan
    | VlanPcp
    | VSwitch
    | VPort
    | EthType
    | IPProto
    | EthSrc
    | EthDst
    | IP4Src
    | IP4Dst
    | TCPSrcPort
    | TCPDstPort
    | Location
    | VFabric
    [@@deriving sexp, enumerate, enum]

  (** The type of packet fields. This is an enumeration whose ordering has an
      effect on the performance of Tdk operations, as well as the size of the
      flowtables that the compiler will produce. *)

  let num_fields = max + 1

  let hash = Hashtbl.hash

  let of_string = function
    | "Switch" -> Switch
    | "Location" -> Location
    | "EthSrc" -> EthSrc
    | "EthDst" -> EthDst
    | "Vlan" -> Vlan
    | "VlanPcp" -> VlanPcp
    | "EthType" -> EthType
    | "IPProto" -> IPProto
    | "IP4Src" -> IP4Src
    | "IP4Dst" -> IP4Dst
    | "TCPSrcPort" -> TCPSrcPort
    | "TCPDstPort" -> TCPDstPort
    | _ -> assert false

  let to_string = function
    | Switch -> "Switch"
    | Vlan -> "Vlan"
    | VlanPcp -> "VlanPcp"
    | VSwitch -> "VSwitch"
    | VPort -> "VPort"
    | EthType -> "EthType"
    | IPProto -> "IPProto"
    | EthSrc -> "EthSrc"
    | EthDst -> "EthDst"
    | IP4Src -> "IP4Src"
    | IP4Dst -> "IP4Dst"
    | TCPSrcPort -> "TCPSrcPort"
    | TCPDstPort -> "TCPDstPort"
    | Location -> "Location"
    | VFabric -> "VFabric"

  let is_valid_order (lst : t list) : bool =
    Set.Poly.(equal (of_list lst) (of_list all))

  (* Initial order is the order in which fields appear in this file. *)
  let order = Array.init num_fields ~f:ident

  let set_order (lst : t list) : unit =
    assert (is_valid_order lst);
    List.iteri lst ~f:(fun i fld -> order.(to_enum fld) <- i)

  let get_order () =
    Array.to_list order
    |> List.filter_map ~f:of_enum

  (* compare depends on current order! *)
  let compare (x : t) (y : t) : int =
    (* using Obj.magic instead of to_enum for bettter performance *)
    Int.compare order.(Obj.magic x) order.(Obj.magic y)

  let field_of_header_val hv = match hv with
    | Frenetic_NetKAT.Switch _ -> Switch
    | Frenetic_NetKAT.Location _ -> Location
    | Frenetic_NetKAT.EthSrc _ -> EthSrc
    | Frenetic_NetKAT.EthDst _ -> EthDst
    | Frenetic_NetKAT.Vlan _ -> Vlan
    | Frenetic_NetKAT.VlanPcp _ -> VlanPcp
    | Frenetic_NetKAT.VSwitch _ -> VSwitch
    | Frenetic_NetKAT.VPort _ -> VPort
    | Frenetic_NetKAT.EthType _ -> EthType
    | Frenetic_NetKAT.IPProto _ -> IPProto
    | Frenetic_NetKAT.IP4Src _ -> IP4Src
    | Frenetic_NetKAT.IP4Dst _ -> IP4Dst
    | Frenetic_NetKAT.TCPSrcPort _ -> TCPSrcPort
    | Frenetic_NetKAT.TCPDstPort _ -> TCPDstPort
    | Frenetic_NetKAT.VFabric _ -> VFabric

  (* Heuristic to pick a variable order that operates by scoring the fields
     in a policy. A field receives a high score if, when a test field=X
     is false, the policy can be shrunk substantially.

     NOTE(arjun): This could be done better, but it seems to work quite well
     on FatTrees and the SDX benchmarks. Some ideas for improvement:

     - Easy: also account for setting tests field=X suceeded
     - Harder, but possibly much better: properly calculate the size of the
       pol for different field assignments. Don't traverse the policy
       repeatedly. Instead, write a size function that returns map from
       field assignments to sizes. *)
  let auto_order (pol : Frenetic_NetKAT.policy) : unit =
    let open Frenetic_NetKAT in
    let count_tbl =
      match Hashtbl.Poly.of_alist (List.map all ~f:(fun f -> (f, 0))) with
      | `Ok tbl -> tbl
      | `Duplicate_key _ -> assert false in
    let rec f_pred size in_product pred = match pred with
      | True -> ()
      | False -> ()
      | Test hv ->
        if in_product then
          let fld = field_of_header_val hv in
          let n = Hashtbl.Poly.find_exn count_tbl fld in
          Hashtbl.Poly.set count_tbl ~key:fld ~data:(n + size)
      | Or (a, b) -> f_pred size false a; f_pred size false b
      | And (a, b) -> f_pred size true a; f_pred size true b
      | Neg a -> f_pred size in_product a in
    let rec f_seq' pol lst = match pol with
      | Mod _ -> (1, lst)
      | Filter a -> (1, a :: lst)
      | Seq (p, q) ->
        let (m, lst) = f_seq' p lst in
        let (n, lst) = f_seq' q lst in
        (m * n, lst)
      | Union _ -> (f_union pol, lst)
      | Star _ | Link _ | VLink _ -> (1, lst) (* bad, but it works *)
    and f_seq pol =
      let (size, preds) = f_seq' pol [] in
      List.iter preds ~f:(f_pred size true);
      size
    and f_union' pol k = match pol with
      | Mod _ -> k 1
      | Filter _ -> k 1
      | Union (p, q) ->
        f_union' p (fun m -> f_union' q (fun n -> k (m + n)))
      | Seq _ -> k (f_seq pol)
      | Star _ | Link _ | VLink _ -> k 1 (* bad, but it works *)
    and f_union pol = f_union' pol (fun n -> n) in
    let _ = f_seq pol in
    Hashtbl.Poly.to_alist count_tbl
    |> List.sort ~cmp:(fun (_, x) (_, y) -> Int.compare y x)
    |> List.map ~f:fst
    |> set_order

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
    (* TODO(grouptable): HACK, should only be able to fast fail on ports.
     * Put this somewhere else *)
    | FastFail of Int32.t list
    [@@deriving sexp]

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
        else
          Int64.shift_right_logical a (64-n) = Int64.shift_right_logical b (64-n)
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
    | _          , Query _
    | FastFail _ , _
    | _          , FastFail _ -> false
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
        None
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
    | _          , Query _
    | FastFail _ , _
    | _          , FastFail _ -> None
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
    | _          , Query _
    | FastFail _ , _
    | _          , FastFail _ -> None
    | Mask(a, m) , Mask(b, n) -> join_mask a m  b n
    | Const a, Mask(b, n)     -> join_mask a 64 b n

  let hash = Hashtbl.hash

  let compare x y = match (x, y) with
    | Const a, Mask (b, 64)
    | Mask (a, 64), Const b
    | Const a, Const b -> Int64.compare a b
    | Query s1, Query s2
    | Pipe s1, Pipe s2 -> String.compare s1 s2
    | FastFail l1, FastFail l2 -> List.compare Int32.compare l1 l2
    | Mask(a, m) , Mask(b, n) ->
      let shift = 64 - min m n in
      (match Int64.(compare (shift_right a shift) (shift_right b shift)) with
       | 0 -> Int.compare n m
       | c -> c)
    | Const _ , _ -> -1
    | _, Const _ -> 1
    | Mask _, _ -> -1
    | _, Mask _ -> 1
    | Query _, _ -> -1
    | _, Query _ -> 1
    | Pipe _, _ -> -1
    | _, Pipe _ -> 1

  let equal x y = compare x y = 0

  let to_string = function
    | Const(a)   -> Printf.sprintf "%Lu" a
    | Mask(a, m) -> Printf.sprintf "%Lu/%d" a m
    | Pipe(p) -> Printf.sprintf "Pipe(%s)" p
    | Query(p) -> Printf.sprintf "Query(%s)" p
    | FastFail(p_lst) -> Printf.sprintf "FastFail(%s)" (Frenetic_NetKAT.string_of_fastfail p_lst)

  let of_int   t = Const (Int64.of_int   t)
  let of_int32 t = Const (Int64.of_int32 t)
  let of_int64 t = Const t
  let to_int_exn = function
    | Const k -> Int64.to_int_exn k
    | _ -> assert false
end

exception FieldValue_mismatch of Field.t * Value.t


(* Packet patterns.

   This module contains operations related to the deicsion variables of the
   diagram used by the compiler, including functions to convert to and from the
   [header_value], building up flow tables. *)
module Pattern = struct
  type t = Field.t * Value.t
  [@@deriving compare]

  let to_string (f, v) =
    Printf.sprintf "%s = %s" (Field.to_string f) (Value.to_string v)

  let equal a b =
    compare a b = 0

  let to_int = Int64.to_int_exn
  let to_int32 = Int64.to_int32_exn

  let of_hv hv =
    let open Frenetic_NetKAT in
    match hv with
    | Switch sw_id -> (Field.Switch, Value.(Const sw_id))
    | Location(Physical p) -> (Field.Location, Value.of_int32 p)
    (* TODO(grouptable): value hack *)
    | Location(FastFail p_lst) -> (Field.Location, Value.(FastFail p_lst))
    | Location(Pipe p)  -> (Field.Location, Value.(Pipe p))
    | Location(Query p) -> (Field.Location, Value.(Query p))
    | EthSrc(dlAddr) -> (Field.EthSrc, Value.(Const dlAddr))
    | EthDst(dlAddr) -> (Field.EthDst, Value.(Const dlAddr))
    | Vlan(vlan) -> (Field.Vlan, Value.of_int vlan)
    | VlanPcp(vlanPcp) -> (Field.VlanPcp, Value.of_int vlanPcp)
    | VSwitch(vsw_id) -> (Field.VSwitch, Value.(Const vsw_id))
    | VPort(vpt) ->  (Field.VPort, Value.(Const vpt))
    | EthType(dlTyp) -> (Field.EthType, Value.of_int dlTyp)
    | IPProto(nwProto) -> (Field.IPProto, Value.of_int nwProto)
    | IP4Src(nwAddr, mask) ->
      (Field.IP4Src, Value.(Mask(Int64.of_int32 nwAddr, 32 + (Int32.to_int_exn mask))))
    | IP4Dst(nwAddr, mask) ->
      (Field.IP4Dst, Value.(Mask(Int64.of_int32 nwAddr, 32 + (Int32.to_int_exn mask))))
    | TCPSrcPort(tpPort) -> (Field.TCPSrcPort, Value.of_int tpPort)
    | TCPDstPort(tpPort) -> (Field.TCPDstPort, Value.of_int tpPort)
    | VFabric(vfab) -> (Field.VFabric, Value.(Const vfab))

  let to_hv (f, v) =
    let open Field in
    let open Value in
    let module NetKAT = Frenetic_NetKAT in
    match f, v with
    | (Switch  , Const sw) -> NetKAT.Switch sw
    | (Location, Const p) -> NetKAT.(Location (Physical (to_int32 p)))
    | (Location, Pipe  p) -> NetKAT.(Location (Pipe p))
    | (Location, Query q) -> NetKAT.(Location (Query q))
    | (EthSrc  , Const dlAddr) -> NetKAT.(EthSrc dlAddr)
    | (EthDst  , Const dlAddr) -> NetKAT.(EthDst dlAddr)
    | (Vlan    , Const vlan) -> NetKAT.(Vlan(to_int vlan))
    | (VlanPcp , Const vlanPcp) -> NetKAT.(VlanPcp (to_int vlanPcp))
    | (VSwitch  , Const vsw) -> NetKAT.VSwitch vsw
    | (VPort  , Const vpt) -> NetKAT.VPort vpt
    | (EthType , Const dlTyp) -> NetKAT.(EthType (to_int dlTyp))
    | (IPProto , Const nwProto) -> NetKAT.(IPProto (to_int nwProto))
    | (IP4Src  , Mask(nwAddr, mask)) -> NetKAT.(IP4Src(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Src  , Const nwAddr) -> NetKAT.(IP4Src(to_int32 nwAddr, 32l))
    | (IP4Dst  , Mask(nwAddr, mask)) -> NetKAT.(IP4Dst(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Dst  , Const nwAddr) -> NetKAT.(IP4Dst(to_int32 nwAddr, 32l))
    | (TCPSrcPort, Const tpPort) -> NetKAT.(TCPSrcPort(to_int tpPort))
    | (TCPDstPort, Const tpPort) -> NetKAT.(TCPDstPort(to_int tpPort))
    | (VFabric, Const vfab) -> NetKAT.VFabric vfab
    | _, _ -> raise (FieldValue_mismatch(f, v))

  let to_pred (f, v) =
    Frenetic_NetKAT.Test (to_hv (f, v))

  let to_sdn (f, v) : SDN.Pattern.t -> SDN.Pattern.t =
    (* Converts a [Pattern.t] into a function that will modify a [SDN.Pattern.t]
       to check the condition represented by the [Pattern.t]. *)
    let open Field in
    let open Value in
    match f, v with
    | (Switch, Const _) | (VSwitch, Const _) | (VPort, Const _)  -> assert false
    | (VFabric, Const _) -> assert false
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
   of maps from fields to values/continuations. The inner maps represent a sequential
   composition of field modifications. The outer set represents a parallel
   composition of sequential compositions. *)
module Action = struct

  type field_or_cont =
    | F of Field.t
    | K
  [@@deriving sexp, compare]

  module Seq = struct
    include Map.Make(struct
      type t = field_or_cont [@@deriving sexp, compare]
    end)

    let compare = compare_direct Value.compare

    let fold_fields seq ~init ~f =
      fold seq ~init ~f:(fun ~key ~data acc -> match key with
        | F key -> f ~key ~data acc
        | _ -> acc)

    let equal_mod_k s1 s2 =
      equal (Value.equal) (remove s1 K) (remove s2 K)

    let compare_mod_k s1 s2 =
      compare (remove s1 K) (remove s2 K)

    let to_hvs seq =
      seq |> to_alist |> List.filter_map ~f:(function (F f,v) -> Some (f,v) | _ -> None)

    let to_string (t : Value.t t) : string =
      let s = to_alist t
        |> List.map ~f:(fun (f,v) ->
            let f = match f with
              | K -> "state"
              | F f -> Field.to_string f
            in
            sprintf "%s := %s" f (Value.to_string v))
        |> String.concat ~sep:", "
      in "[" ^ s ^ "]"
  end

  module Par = struct
    include Set.Make(struct
    type t = Value.t Seq.t [@@deriving sexp]
    let compare = Seq.compare
    end)

    let to_hvs par =
      fold par ~init:[] ~f:(fun acc seq -> Seq.to_hvs seq @ acc)

    let to_string t : string =
      let s = to_list t
        |> List.map ~f:Seq.to_string
        |> String.concat ~sep:"; "
      in "{" ^ s ^ "}"

    let mod_k = map ~f:(fun seq -> Seq.remove seq K)

    let compare_mod_k p1 p2 =
      compare (mod_k p1) (mod_k p2)

    let equal_mod_k p1 p2 =
      equal (mod_k p1) (mod_k p2)
  end

  type t = Par.t [@@deriving sexp]

  let one = Par.singleton Seq.empty
  let zero = Par.empty
  let is_one = Par.equal one
  let is_zero = Par.is_empty

  let sum (a:t) (b:t) : t =
    (* This implements parallel composition specifically for NetKAT
       modifications. *)
    if is_zero a then b            (* 0 + p = p *)
    else if is_zero b then a       (* p + 0 = p *)
    else Par.union a b

  let prod (a:t) (b:t) : t =
    (* This implements sequential composition specifically for NetKAT
       modifications and makes use of NetKAT laws to simplify results.*)
    if is_zero a then zero       (* 0; p == 0 *)
    else if is_zero b then zero  (* p; 0 == 0 *)
    else if is_one a then b      (* 1; p == p *)
    else if is_one b then a      (* p; 1 == p *)
    else
      Par.fold a ~init:zero ~f:(fun acc seq1 ->
        (* cannot implement sequential composition of this kind here *)
        let _ = assert (match Seq.find seq1 K with None -> true | _ -> false) in
        let r = Par.map b ~f:(fun seq2 ->
          (* Favor modifications to the right *)
          Seq.merge seq1 seq2 ~f:(fun ~key m ->
            match m with | `Both(_, v) | `Left v | `Right v -> Some(v)))
        in
        Par.union acc r)

  let negate t : t =
    (* This implements negation for the [zero] and [one] actions. Any
       non-[zero] action will be mapped to [zero] by this function. *)
    if is_zero t then one else zero

  let get_queries (t : t) : string list =
    Par.fold t ~init:[] ~f:(fun queries seq ->
      match Seq.find seq (F Location) with
      | Some (Query str) -> str :: queries
      | _ -> queries)

  let to_sdn ?group_tbl (in_port : int64 option) (t:t) : SDN.par =
    (* Convert a NetKAT action to an SDN action. At the moment this function
       assumes that fields are assigned to proper bitwidth integers, and does
       no validation along those lines. If the input is derived from a NetKAT
       surface syntax program, then this assumption likely holds. *)
    let to_int = Int64.to_int_exn in
    let to_int32 = Int64.to_int32_exn in
    let t = Par.filter_map t ~f:(fun seq ->
      (* Queries are equivalent to drop, so remove any [Seq.t]'s from the
       * [Par.t] that set the location to a query.
       *
       * Pipe locations are no longer relevant to compilation, so rewrite all
       * all of them to the empty string. This will allow multiple singleton
       * [Seq.t]'s of port location assignments in a [Par.t] to be collapsed
       * into into one. *)
      match Seq.find seq (F Field.Location) with
      | Some(Value.Query _) -> None
      | Some(Value.Pipe  _) -> Some(Seq.add seq (F Field.Location) (Value.Pipe ""))
      | _                   -> Some(seq))
    in
    let to_port p = match in_port with
      | Some(p') when p = p' -> SDN.InPort
      | _                    -> SDN.(Physical(to_int32 p))
    in
    Par.fold t ~init:[] ~f:(fun acc seq ->
      let open Field in
      let open Value in
      let init =
        match Seq.find seq (F Location) with
        | None           -> [SDN.(Output(InPort))]
        | Some (Const p) -> [SDN.(Output(to_port p))]
        | Some (Pipe  _) -> [SDN.(Output(Controller 128))]
        | Some (Query _) -> assert false
        | Some (FastFail p_lst) ->
           (match group_tbl with
            | Some tbl ->
              let gid = Frenetic_GroupTable0x04.add_fastfail_group tbl p_lst
              in [SDN.(FastFail gid)]
            | None -> failwith "fast failover present, but no group table provided!")
        | Some mask      -> raise (FieldValue_mismatch(Location, mask))
      in
      Seq.fold (Seq.remove seq (F Location)) ~init ~f:(fun ~key ~data acc ->
        match key, data with
        | F Switch  , Const switch -> raise Frenetic_NetKAT.Non_local
        | F Switch  , _ -> raise (FieldValue_mismatch(Switch, data))
        | F Location, _ -> assert false
        | F EthSrc  , Const dlAddr  -> SDN.(Modify(SetEthSrc dlAddr)) :: acc
        | F EthDst  , Const dlAddr  -> SDN.(Modify(SetEthDst dlAddr)) :: acc
        | F Vlan    , Const vlan    -> SDN.(Modify(SetVlan(Some(to_int vlan)))) :: acc
        | F VlanPcp , Const vlanPcp -> SDN.(Modify(SetVlanPcp (to_int vlanPcp))) :: acc
        | F VSwitch, Const _ | F VPort, Const _ | F VFabric, Const _ -> assert false (* JNF: danger, danger *)
        | F EthType , Const dlTyp   -> SDN.(Modify(SetEthTyp (to_int dlTyp))) :: acc
        | F IPProto , Const nwProto -> SDN.(Modify(SetIPProto (to_int nwProto))) :: acc
        | F IP4Src  , Mask (nwAddr, 64)
        | F IP4Src  , Const nwAddr   -> SDN.(Modify(SetIP4Src(to_int32 nwAddr))) :: acc
        | F IP4Dst  , Mask (nwAddr, 64)
        | F IP4Dst  , Const nwAddr   -> SDN.(Modify(SetIP4Dst(to_int32 nwAddr))) :: acc
        | F TCPSrcPort, Const tpPort -> SDN.(Modify(SetTCPSrcPort(to_int tpPort))) :: acc
        | F TCPDstPort, Const tpPort -> SDN.(Modify(SetTCPDstPort(to_int tpPort))) :: acc
        | F f, _ -> raise (FieldValue_mismatch(f, data))
        | K, _ -> assert false
      ) :: acc)

  let demod (f, v) t =
    Par.fold t ~init:zero ~f:(fun acc seq ->
      let seq' = match Seq.find seq (F f) with
        | Some(v')
            when Value.compare v v' = 0 -> Seq.remove seq (F f)
        | _                             -> seq
      in
      sum acc (Par.singleton seq'))

  let to_policy t =
    let open Frenetic_NetKAT in
    Par.fold t ~init:drop ~f:(fun acc seq ->
      let seq' = Seq.fold_fields seq ~init:id ~f:(fun ~key ~data acc ->
        let hv = match Pattern.to_hv (key, data) with
          | IP4Src(nwAddr, 32l) -> IP4Src(nwAddr, 32l)
          | IP4Dst(nwAddr, 32l) -> IP4Dst(nwAddr, 32l)
          | IP4Src _ | IP4Dst _ -> raise (FieldValue_mismatch(key, data))
          | hv -> hv
        in
        Frenetic_NetKAT_Optimize.mk_seq (Mod(hv)) acc)
      in
      Frenetic_NetKAT_Optimize.mk_union seq' acc)

  let fold_fv t ~(init : 'a) ~(f : 'a -> field:Field.t -> value:Value.t -> 'a) : 'a =
    Par.fold t ~init ~f:(fun acc seq ->
      Seq.fold seq ~init:acc ~f:(fun ~key ~data acc -> match key with
        | F key -> f acc ~field:key ~value:data
        | _ -> acc))

  let pipes t =
    fold_fv t ~init:String.Set.empty ~f:(fun acc ~field ~value ->
      match field, value with
      | Field.Location, Value.Pipe q -> Set.add acc q
      | _, _ -> acc)

  let queries t =
    fold_fv t ~init:String.Set.empty ~f:(fun acc ~field ~value ->
      match field, value with
      | Field.Location, Value.Query q -> Set.add acc q
      | _, _ -> acc)
    |> Set.to_list

  let hash t =
    (* XXX(seliopou): Hashtbl.hash does not work because the same set can have
     * multiple representations. Pick a better hash function. *)
    Hashtbl.hash (List.map (Par.to_list t) ~f:(fun seq -> Seq.to_alist seq))

  let compare =
    Par.compare

  let size =
    Par.fold ~init:0 ~f:(fun acc seq -> acc + (Seq.length seq))

  let to_string = Par.to_string
    (* let par = to_sdn ~group_tbl:(Frenetic_GroupTable0x04.create ()) None t in
    Printf.sprintf "[%s]" (SDN.string_of_par par) *)

end

module FDK = struct

  include Frenetic_Vlr.Make(Field)(Value)(Action)

  let mk_cont k = mk_leaf Action.(Par.singleton (Seq.singleton K (Value.of_int k)))

  let conts fdk =
    fold
      (fun par ->
        Action.Par.fold par ~init:Int.Set.empty ~f:(fun acc seq ->
          match Action.(Seq.find seq K) with
          | None -> acc
          | Some k -> Value.to_int_exn k |> Int.Set.add acc))
      (fun _ t f -> Set.union t f)
      fdk

  let map_conts fdk ~(f: int -> int) =
    let open Action in
    let f par = Par.map par ~f:(fun seq -> Seq.change seq K (function
      | None -> failwith "continuation expected, but none found"
      | Some k -> Some (k |> Value.to_int_exn |> f |> Value.of_int)))
    in
    map_r f fdk

end
