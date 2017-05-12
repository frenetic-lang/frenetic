open Core.Std

module Field = struct

  (** The order of the constructors defines the default variable ordering and has a massive
      performance impact. Do not change unless you know what you are doing. *)
  type t
    = AbstractLoc
    | From
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

  let of_header header = match header with
    | Frenetic_NetKAT_Portless.AbstractLoc -> AbstractLoc
    | Frenetic_NetKAT_Portless.From -> From
    | Frenetic_NetKAT_Portless.EthSrc -> EthSrc
    | Frenetic_NetKAT_Portless.EthDst -> EthDst
    | Frenetic_NetKAT_Portless.Vlan -> Vlan
    | Frenetic_NetKAT_Portless.VlanPcp -> VlanPcp
    | Frenetic_NetKAT_Portless.EthType -> EthType
    | Frenetic_NetKAT_Portless.IPProto -> IPProto
    | Frenetic_NetKAT_Portless.IP4Src -> IP4Src
    | Frenetic_NetKAT_Portless.IP4Dst -> IP4Dst
    | Frenetic_NetKAT_Portless.TCPSrcPort -> TCPSrcPort
    | Frenetic_NetKAT_Portless.TCPDstPort -> TCPDstPort

  let to_header header = match header with
    | AbstractLoc -> Frenetic_NetKAT_Portless.AbstractLoc
    | From -> Frenetic_NetKAT_Portless.From
    | EthSrc -> Frenetic_NetKAT_Portless.EthSrc
    | EthDst -> Frenetic_NetKAT_Portless.EthDst
    | Vlan -> Frenetic_NetKAT_Portless.Vlan
    | VlanPcp -> Frenetic_NetKAT_Portless.VlanPcp
    | EthType -> Frenetic_NetKAT_Portless.EthType
    | IPProto -> Frenetic_NetKAT_Portless.IPProto
    | IP4Src -> Frenetic_NetKAT_Portless.IP4Src
    | IP4Dst -> Frenetic_NetKAT_Portless.IP4Dst
    | TCPSrcPort -> Frenetic_NetKAT_Portless.TCPSrcPort
    | TCPDstPort -> Frenetic_NetKAT_Portless.TCPDstPort

  (* Heuristic to pick a variable order that operates by scoring the fields
     in a policy. A field receives a high score if, when a test field=X
     is false, the policy can be shrunk substantially. *)
  let auto_order (pol : Frenetic_NetKAT_Portless.policy) : unit =
    let open Frenetic_NetKAT_Portless in
    (* Construct array of scores, where score starts at 0 for every field *)
    let count_arr = Array.init num_fields ~f:(fun _ -> 0) in
    let rec f_pred size pred = match pred with
      | True -> ()
      | False -> ()
      | Test (header,_,_) ->
        let f = to_enum (of_header header) in
        count_arr.(f) <- count_arr.(f) + size
      | Or (a, b) -> f_pred size a; f_pred size b
      | And (a, b) -> f_pred size a; f_pred size b
      | Neg a -> f_pred size a in
    let rec f_seq' pol lst  k = match pol with
      | Mod _ -> k (1, lst)
      | Filter a -> k (1, a :: lst)
      | Seq (p, q) ->
        f_seq' p lst  (fun (m, lst) ->
          f_seq' q lst  (fun (n, lst) ->
            k (m * n, lst)))
      | Union _ -> k (f_union pol , lst)
      | Star p -> k (f_union p , lst)
    and f_seq pol  : int =
      let (size, preds) = f_seq' pol [] (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    and f_union' pol lst  k = match pol with
      | Mod _ -> (1, lst)
      | Filter a -> (1, a :: lst)
      | Union (p, q) ->
        f_union' p lst  (fun (m, lst) ->
          f_union' q lst  (fun (n, lst) ->
            k (m + n, lst)))
      | Seq _ -> k (f_seq pol , lst)
      | Star p -> f_union' p lst  k
    and f_union pol  : int =
      let (size, preds) = f_union' pol []  (fun x -> x) in
      List.iter preds ~f:(f_pred size);
      size
    in
    let _ = f_seq pol in
    Array.foldi count_arr ~init:[] ~f:(fun i acc n -> ((Obj.magic i, n) :: acc))
    |> List.stable_sort ~cmp:(fun (_, x) (_, y) -> Int.compare x y)
    |> List.rev (* SJS: do NOT remove & reverse order! Want stable sort *)
    |> List.map ~f:fst
    |> set_order

end

module Value = struct

  type t = Int64.t * int [@@deriving sexp]

  (* subseq_eq, meet and join are defined to make this fit interface of Frenetic_Vlr.Lattice *)
  (* Note that Mask checking is a lot like Frenetic_OpenFlow.Pattern.Ip, but the int's are different sizes *)
  let subset_eq (a, m) (b, n) =
    if m < n
      then false
    else
      Int64.shift_right_logical a (64-n) = Int64.shift_right_logical b (64-n)

  let meet ?(tight=false) (a, m) (b, n) =
    let lt = subset_eq (a, m) (b, n) in
    let gt = subset_eq (b, n) (a, m) in
    if lt && gt then
      Some((a, m))
    else if lt then
      if (not tight) || (m = (n + 1)) then Some((a, m)) else None
    else if gt then
      if (not tight) || (n = (m + 1)) then Some((b, n)) else None
    else
      None

  (* The intent here looks a lot like Frenetic_OpenFlow.Pattern.Ip.join, but the notion of "tightness" might not
     not apply.  Look at perhaps sharing the logic between the two, abstracting out bit length since this deals with
     64 bit ints *)
  let join ?(tight=false) (a, m) (b, n) =
    let lt = subset_eq ((a, m)) ((b, n)) in
    let gt = subset_eq ((b, n)) ((a, m)) in
    if lt && gt then
      Some((a, m))
    else if lt then
      if (not tight) || (n = (m - 1)) then Some((b, n)) else None
    else if gt then
      if (not tight) || (m = (n - 1)) then Some((a, m)) else None
    else
      if (not tight) || m = n then
        let x, y = ((a, m - 1), (b, n - 1)) in
        if subset_eq x y && subset_eq y x then Some(x) else None
      else
        None (* XXX(seliopou): complete definition *)

  let hash = Hashtbl.hash

  (* Value compare is used in Pattern below, but is not public *)
  let compare (a, m) (b, n) =
    let shift = 64 - min m n in
    (match Int64.(compare (shift_right a shift) (shift_right b shift)) with
     | 0 -> Int.compare n m
     | c -> c)

  let equal x y = compare x y = 0

  let to_string (a, m) = Printf.sprintf "%Lu/%d" a m

end

exception FieldValue_mismatch of Field.t * Value.t

module Pattern = struct
  type t = Field.t * Value.t
  [@@deriving compare]

  let to_string (f, v) =
    Printf.sprintf "%s = %s" (Field.to_string f) (Value.to_string v)

  let equal a b =
    compare a b = 0

  let of_header_value_mask header value mask = (Field.of_header header, (value, mask))

  let to_header_value_mask (f, (v, m)) = (f, v, m)

  let to_pred (f, (v, m)) = Frenetic_NetKAT_Portless.Test (Field.to_header f, v, m)

end


module Action = struct

  type field_or_cont = Field.t [@@deriving sexp, compare]

  module Seq = struct
    include Map.Make(struct
      type t = field_or_cont [@@deriving sexp, compare]
    end)

    let compare = compare_direct Value.compare

    let fold_fields seq ~init ~f =
      fold seq ~init ~f:(fun ~key ~data acc -> f ~key ~data acc)

    let to_hvs seq =
      seq |> to_alist |> List.filter_map ~f:(function (f,v) -> Some (f,v))

    let to_string (t : Value.t t) : string =
      let s = to_alist t
        |> List.map ~f:(fun (f,v) ->
            let f = Field.to_string f in
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

  let to_policy t =
    let open Frenetic_NetKAT_Portless in
    Par.fold t ~init:(Filter False) ~f:(fun acc seq ->
        let seq' = Seq.fold_fields seq ~init:(Filter True) ~f:(fun ~key ~data acc ->
        let (h, v, m) = Pattern.to_header_value_mask (key, data) in
        Frenetic_NetKAT_Portless_Optimize.mk_seq (Mod(Field.to_header h, v, m)) acc) in
        Frenetic_NetKAT_Portless_Optimize.mk_union seq' acc)

  let hash t =
    (* XXX(seliopou): Hashtbl.hash does not work because the same set can have
     * multiple representations. Pick a better hash function. *)
    Hashtbl.hash (List.map (Par.to_list t) ~f:(fun seq -> Seq.to_alist seq))

  let compare =
    Par.compare

  let size =
    Par.fold ~init:0 ~f:(fun acc seq -> acc + (Seq.length seq))

  let to_string = Par.to_string

end

module Portless_FDD = struct
  include Frenetic_Vlr.Make(Field)(Value)(Action)
end
