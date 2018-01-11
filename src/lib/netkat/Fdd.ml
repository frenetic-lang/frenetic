open Core

module SDN = Frenetic_kernel.OpenFlow


module Field = struct

  (** The order of the constructors defines the default variable ordering and has a massive
      performance impact. Do not change unless you know what you are doing. *)
  type t
    = Switch
    | Location
    | From
    | AbstractLoc
    | VSwitch
    | VPort
    | Vlan
    | VlanPcp
    (* SJS: for simplicity, support only up to 5 meta fields for now *)
    | Meta0
    | Meta1
    | Meta2
    | Meta3
    | Meta4
    | EthType
    | IPProto
    | EthSrc
    | EthDst
    | IP4Src
    | IP4Dst
    | TCPSrcPort
    | TCPDstPort
    | VFabric
    [@@deriving sexp, enumerate, enum]
  type field = t

  let num_fields = max + 1

  let hash = Hashtbl.hash

  let of_string s =
    Sexp.of_string s |> t_of_sexp

  let to_string t =
    sexp_of_t t |> Sexp.to_string

  let is_valid_order (lst : t list) : bool =
    Set.Poly.(equal (of_list lst) (of_list all))

  let order = Array.init num_fields ~f:ident

  let set_order (lst : t list) : unit =
    assert (is_valid_order lst);
    List.iteri lst ~f:(fun i fld -> order.(to_enum fld) <- i)

  (* Not a clean way to invert a permutation, but fast *)
  let invert arr =
    let inverted = Array.init num_fields ~f:ident in
    Array.iteri arr ~f:(fun i elt -> inverted.(elt) <- i );
    inverted

  let get_order () =
    Array.to_list (invert order)
    |> List.filter_map ~f:of_enum

  (* compare depends on current order! *)
  let compare (x : t) (y : t) : int =
    (* using Obj.magic instead of to_enum for bettter performance *)
    Int.compare order.(Obj.magic x) order.(Obj.magic y)

  let equal x y = x = y

  module type ENV = sig
    type t
    val empty : t
    exception Full
    val add : t -> string -> Syntax.meta_init -> bool -> t (* may raise Full *)
    val lookup : t -> string -> field * (Syntax.meta_init * bool) (* may raise Not_found *)
  end

  module Env : ENV = struct

    type t = {
      alist : (string * (field * (Syntax.meta_init * bool))) list;
      depth : int
    }

    let empty = { alist = []; depth = 0 }

    exception Full

    let add env name init mut =
      let field =
        match env.depth with
        | 0 -> Meta0
        | 1 -> Meta1
        | 2 -> Meta2
        | 3 -> Meta3
        | 4 -> Meta4
        | _ -> raise Full
      in
      { alist = List.Assoc.add ~equal:(=) env.alist name (field, (init, mut));
        depth = env.depth + 1}

    let lookup env name =
      List.Assoc.find_exn ~equal:(=) env.alist name
  end

  let of_hv ?(env=Env.empty) hv = match hv with
    | Syntax.Switch _ -> Switch
    | Syntax.Location _ -> Location
    | Syntax.From _ -> From
    | Syntax.AbstractLoc _ -> AbstractLoc
    | Syntax.EthSrc _ -> EthSrc
    | Syntax.EthDst _ -> EthDst
    | Syntax.Vlan _ -> Vlan
    | Syntax.VlanPcp _ -> VlanPcp
    | Syntax.VSwitch _ -> VSwitch
    | Syntax.VPort _ -> VPort
    | Syntax.EthType _ -> EthType
    | Syntax.IPProto _ -> IPProto
    | Syntax.IP4Src _ -> IP4Src
    | Syntax.IP4Dst _ -> IP4Dst
    | Syntax.TCPSrcPort _ -> TCPSrcPort
    | Syntax.TCPDstPort _ -> TCPDstPort
    | Syntax.VFabric _ -> VFabric
    | Syntax.Meta (id,_) -> fst (Env.lookup env id)

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
  let auto_order (pol : Syntax.policy) : unit =
    let open Syntax in
    (* Construct array of scores, where score starts at 0 for every field *)
    let count_arr = Array.init num_fields ~f:(fun _ -> 0) in
    let rec f_pred size (env, pred) = match pred with
      | True -> ()
      | False -> ()
      | Test (Syntax.Meta (id,_)) ->
        begin match Env.lookup env id with
        | (f, (Alias hv, false)) ->
          let f = to_enum f in
          let f' = to_enum (of_hv hv) in
          count_arr.(f) <- count_arr.(f) + size;
          count_arr.(f') <- count_arr.(f') + size
        | (f,_) ->
          let f = to_enum f in
          count_arr.(f) <- count_arr.(f) + size
        end
      | Test hv ->
        let f = to_enum (of_hv hv) in
        count_arr.(f) <- count_arr.(f) + size
      | Or (a, b) -> f_pred size (env, a); f_pred size (env, b)
      | And (a, b) -> f_pred size (env, a); f_pred size (env, b)
      | Neg a -> f_pred size (env, a) in
    let rec f_seq' pol lst env k = match pol with
      | Mod _ -> k (1, lst)
      | Filter a -> k (1, (env, a) :: lst)
      | Seq (p, q) ->
        f_seq' p lst env (fun (m, lst) ->
          f_seq' q lst env (fun (n, lst) ->
            k (m * n, lst)))
      | Union _ -> k (f_union pol env, lst)
      | Let { id; init; mut; body=p } ->
        let env = Env.add env id init mut in
        f_seq' p lst env k
      | Star p -> k (f_union p env, lst)
      | Link (sw,pt,_,_) -> k (1, (env, Test (Switch sw)) :: (env, Test (Location (Physical pt))) :: lst)
      | VLink (sw,pt,_,_) -> k (1, (env, Test (VSwitch sw)) :: (env, Test (VPort pt)) :: lst)
      | Dup -> k (1, lst)
    and f_seq pol env : int =
      let (size, preds) = f_seq' pol [] env (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    and f_union' pol lst env k = match pol with
      | Mod _ -> (1, lst)
      | Filter a -> (1, (env, a) :: lst)
      | Union (p, q) ->
        f_union' p lst env (fun (m, lst) ->
          f_union' q lst env (fun (n, lst) ->
            k (m + n, lst)))
      | Seq _ -> k (f_seq pol env, lst)
      | Let { id; init; mut; body=p } ->
        let env = Env.add env id init mut in
        k (f_seq p env, lst)
      | Star p -> f_union' p lst env k
      | Link (sw,pt,_,_) -> k (1, (env, Test (Switch sw)) :: (env, Test (Location (Physical pt))) :: lst)
      | VLink (sw,pt,_,_) -> k (1, (env, Test (VSwitch sw)) :: (env, Test (VPort pt)) :: lst)
      | Dup -> k (1, lst)
    and f_union pol env : int =
      let (size, preds) = f_union' pol [] env (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    in
    let _ = f_seq pol Env.empty in
    Array.foldi count_arr ~init:[] ~f:(fun i acc n -> ((Obj.magic i, n) :: acc))
    |> List.stable_sort ~cmp:(fun (_, x) (_, y) -> Int.compare x y)
    |> List.rev (* SJS: do NOT remove & reverse order! Want stable sort *)
    |> List.map ~f:fst
    |> set_order

end

module Value = struct

  type t
    = Const of Int64.t
    | Mask of Int64.t * int
    | AbstractLocation of Syntax.abstract_location
    | Pipe of string
    | Query of string
    (* TODO(grouptable): HACK, should only be able to fast fail on ports.
     * Put this somewhere else *)
    | FastFail of Int32.t list
    [@@deriving sexp]

  (* subseq_eq, meet and join are defined to make this fit interface of Vlr.Lattice *)
  let subset_eq a b =
    (* Note that Mask checking is a lot like OpenFlow.Pattern.Ip, but the int's are different sizes *)
    let subset_eq_mask a m b n =
      if m < n
        then false
        else
          Int64.shift_right_logical a (64-n) = Int64.shift_right_logical b (64-n)
    in
    match a, b with
    | Const  a           , Const b
    (* Note that comparing a mask to a constant requires the mask to be all 64 bits, otherwise they fail the lesser mask test *)
    | Mask(a, 64)        , Const b -> a = b
    | AbstractLocation a , AbstractLocation b -> a = b
    | Pipe   a           , Pipe  b
    | Query  a           , Query b -> a = b
    | Mask             _ , Const            _
    | AbstractLocation _ ,                  _
    | Pipe             _ ,                  _
    | Query            _ ,                  _
    | FastFail         _ ,                  _
    | _                  , AbstractLocation _
    | _                  , Pipe             _
    | _                  , Query            _
    | _                  , FastFail         _ -> false
    | Mask(a, m)         , Mask(b, n) -> subset_eq_mask a m  b n
    | Const a            , Mask(b, n) -> subset_eq_mask a 64 b n

  let meet ?(tight=false) a b =
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
    | AbstractLocation a , AbstractLocation b ->
      if Syntax.equal_abstract_location a b then Some(AbstractLocation a) else None
    | AbstractLocation _ , _
    | _ , AbstractLocation _ -> None
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
    (* The intent here looks a lot like OpenFlow.Pattern.Ip.join, but the notion of "tightness" might not
       not apply.  Look at perhaps sharing the logic between the two, abstracting out bit length since this deals with
       64 bit ints *)
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
    | AbstractLocation a , AbstractLocation b ->
      if Syntax.equal_abstract_location a b then Some(AbstractLocation a) else None
    | AbstractLocation _ , _
    | _ , AbstractLocation _ -> None
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

  (* Value compare is used in Pattern below, but is not public *)
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
    | AbstractLocation a , AbstractLocation b ->
      Syntax.compare_abstract_location a b
    | Const _ , _ -> -1
    | _, Const _ -> 1
    | Mask _, _ -> -1
    | _, Mask _ -> 1
    | AbstractLocation _ , _ -> -1
    | _ , AbstractLocation _ -> 1
    | Query _, _ -> -1
    | _, Query _ -> 1
    | Pipe _, _ -> -1
    | _, Pipe _ -> 1

  let equal x y = compare x y = 0

  let to_string = function
    | Const(a)   -> Printf.sprintf "%Lu" a
    | Mask(a, m) -> Printf.sprintf "%Lu/%d" a m
    | AbstractLocation(s) -> Printf.sprintf "%s" s
    | Pipe(p) -> Printf.sprintf "Pipe(%s)" p
    | Query(p) -> Printf.sprintf "Query(%s)" p
    | FastFail(p_lst) -> Printf.sprintf "FastFail(%s)" (Syntax.string_of_fastfail p_lst)

  let of_int   t = Const (Int64.of_int   t)
  (* Private to this file only *)
  let of_int32 t = Const (Int64.of_int32 t)
  let of_int64 t = Const t
  let to_int64_exn = function
    | Const k -> k
    | _ -> assert false
end

exception FieldValue_mismatch of Field.t * Value.t

module Pattern = struct
  type t = Field.t * Value.t
  [@@deriving compare]

  let to_string (f, v) =
    Printf.sprintf "%s = %s" (Field.to_string f) (Value.to_string v)

  let equal a b =
    compare a b = 0

  let to_int = Int64.to_int_exn
  let to_int32 = Int64.to_int32_exn

  let of_hv ?(env=Field.Env.empty) hv =
    let open Syntax in
    match hv with
    | Switch sw_id -> (Field.Switch, Value.(Const sw_id))
    | Location(Physical p) -> (Field.Location, Value.of_int32 p)
    | From loc -> (Field.From, Value.AbstractLocation loc)
    | AbstractLoc loc -> (Field.AbstractLoc, Value.AbstractLocation loc)
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
    | Meta(name,v) -> (fst (Field.Env.lookup env name), Value.(Const v))

  let to_hv (f, v) =
    let open Field in
    let open Value in
    match f, v with
    | (Switch  , Const sw) -> Syntax.Switch sw
    | (Location, Const p) -> Syntax.(Location (Physical (to_int32 p)))
    | (Location, Pipe  p) -> Syntax.(Location (Pipe p))
    | (Location, Query q) -> Syntax.(Location (Query q))
    | (From, AbstractLocation l) -> Syntax.From l
    | (AbstractLoc, AbstractLocation l) -> Syntax.AbstractLoc l
    | (EthSrc  , Const dlAddr) -> Syntax.(EthSrc dlAddr)
    | (EthDst  , Const dlAddr) -> Syntax.(EthDst dlAddr)
    | (Vlan    , Const vlan) -> Syntax.(Vlan(to_int vlan))
    | (VlanPcp , Const vlanPcp) -> Syntax.(VlanPcp (to_int vlanPcp))
    | (VSwitch  , Const vsw) -> Syntax.VSwitch vsw
    | (VPort  , Const vpt) -> Syntax.VPort vpt
    | (EthType , Const dlTyp) -> Syntax.(EthType (to_int dlTyp))
    | (IPProto , Const nwProto) -> Syntax.(IPProto (to_int nwProto))
    | (IP4Src  , Mask(nwAddr, mask)) -> Syntax.(IP4Src(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Src  , Const nwAddr) -> Syntax.(IP4Src(to_int32 nwAddr, 32l))
    | (IP4Dst  , Mask(nwAddr, mask)) -> Syntax.(IP4Dst(to_int32 nwAddr, Int32.of_int_exn (mask - 32)))
    | (IP4Dst  , Const nwAddr) -> Syntax.(IP4Dst(to_int32 nwAddr, 32l))
    | (TCPSrcPort, Const tpPort) -> Syntax.(TCPSrcPort(to_int tpPort))
    | (TCPDstPort, Const tpPort) -> Syntax.(TCPDstPort(to_int tpPort))
    | (VFabric, Const vfab) -> Syntax.VFabric vfab
    | _, _ -> raise (FieldValue_mismatch(f, v))

  let to_pred (f, v) =
    Syntax.Test (to_hv (f, v))

  let to_sdn (f, v) : SDN.Pattern.t -> SDN.Pattern.t =
    let open Field in
    let open Value in
    match f, v with
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
      { pat with SDN.Pattern.nwSrc = Some(to_int32 nwAddr, 32l) }
    | (IP4Dst  , Mask(nwAddr, mask)) -> fun pat ->
      { pat with SDN.Pattern.nwDst =
          Some(to_int32 nwAddr, Int32.of_int_exn (mask - 32)) }
    | (IP4Dst  , Const nwAddr) -> fun pat ->
      { pat with SDN.Pattern.nwDst = Some(to_int32 nwAddr, 32l) }
    | (TCPSrcPort, Const tpPort) -> fun pat ->
      { pat with SDN.Pattern.tpSrc = Some(to_int tpPort) }
    | (TCPDstPort, Const tpPort) -> fun pat ->
      { pat with SDN.Pattern.tpDst = Some(to_int tpPort) }
    (* Should never happen because these pseudo-fields should have been removed by the time to_sdn is used *)
    | (Switch, Const _)
    | (From, AbstractLocation _)
    | (AbstractLoc, AbstractLocation _)
    | (VSwitch, Const _)
    | (VPort, Const _)
    | (VFabric, Const _)
    | (Meta0, Const _)
    | (Meta1, Const _)
    | (Meta2, Const _)
    | (Meta3, Const _)
    | (Meta4, Const _) -> assert false
    | _, _ -> raise (FieldValue_mismatch(f, v))

end


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
      | Some(Value.Pipe  _) -> Some(Seq.set seq (F Field.Location) (Value.Pipe ""))
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
              let gid = Frenetic_kernel.GroupTable0x04.add_fastfail_group tbl p_lst
              in [SDN.(FastFail gid)]
            | None -> failwith "fast failover present, but no group table provided!")
        | Some mask      -> raise (FieldValue_mismatch(Location, mask))
      in
      Seq.fold (Seq.remove seq (F Location)) ~init ~f:(fun ~key ~data acc ->
        match key, data with
        | F From       , AbstractLocation loc -> raise Syntax.Non_local
        | F AbstractLoc, AbstractLocation loc -> raise Syntax.Non_local
        | F Switch  , Const switch -> raise Syntax.Non_local
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
    let open Syntax in
    Par.fold t ~init:drop ~f:(fun acc seq ->
      let seq' = Seq.fold_fields seq ~init:id ~f:(fun ~key ~data acc ->
        let hv = match Pattern.to_hv (key, data) with
          | IP4Src(nwAddr, 32l) -> IP4Src(nwAddr, 32l)
          | IP4Dst(nwAddr, 32l) -> IP4Dst(nwAddr, 32l)
          | IP4Src _ | IP4Dst _ -> raise (FieldValue_mismatch(key, data))
          | hv -> hv
        in
        Optimize.mk_seq (Mod(hv)) acc)
      in
      Optimize.mk_union seq' acc)

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
    (* let par = to_sdn ~group_tbl:(Frenetic_kernel.GroupTable0x04.create ()) None t in
    Printf.sprintf "[%s]" (SDN.string_of_par par) *)

end

module FDD = struct

  include Vlr.Make(Field)(Value)(Action)

  let mk_cont k = const Action.(Par.singleton (Seq.singleton K (Value.of_int64 k)))

  let conts fdd =
    fold fdd
      ~f:(fun par ->
        Action.Par.fold par ~init:Int64.Set.empty ~f:(fun acc seq ->
          match Action.(Seq.find seq K) with
          | None -> acc
          | Some k -> Value.to_int64_exn k |> Int64.Set.add acc))
      ~g:(fun _ t f -> Set.union t f)

  let map_conts fdd ~(f: int64 -> int64) =
    let open Action in
    let f par = Par.map par ~f:(fun seq -> Seq.change seq K (function
      | None -> failwith "continuation expected, but none found"
      | Some k -> Some (k |> Value.to_int64_exn |> f |> Value.of_int64)))
    in
    map_r f fdd

end
