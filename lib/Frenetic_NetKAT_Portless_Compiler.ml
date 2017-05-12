open Core.Std
open Frenetic_Portless_Fdd
open Frenetic_NetKAT_Portless

module Field = Frenetic_Portless_Fdd.Field

type order
  = [ `Default
    | `Static of Field.t list
    | `Heuristic ]

module Action = Frenetic_Portless_Fdd.Action
module Value = Frenetic_Portless_Fdd.Value
module Par = Action.Par
module Seq = Action.Seq

module Local_Portless_FDD = struct

  include Portless_FDD

  let of_test header value mask =
    atom (Pattern.of_header_value_mask header value mask) Action.one Action.zero

  let of_mod header value mask =
    let k, v = Pattern.of_header_value_mask header value mask in
    const Action.(Par.singleton (Seq.singleton k v))

  let rec of_pred (pred: pred) =
    match pred with
    | True           -> id
    | False          -> drop
    | Test(h, v, m)  -> of_test h v m
    | And(p, q)      -> prod (of_pred p) (of_pred q)
    | Or (p, q)      -> sum (of_pred p) (of_pred q)
    | Neg(q)         -> map_r Action.negate (of_pred q)

  let seq_tbl = BinTbl.create ~size:1000 ()

  let clear_cache ~preserve = begin
    BinTbl.clear seq_tbl;
    clear_cache preserve;
  end

  let seq t u =
    match unget u with
    | Leaf _ -> prod t u (* This is an optimization. If [u] is an
                            [Action.Par.t], then it will compose with [t]
                            regardless of however [t] modifies packets. None
                            of the decision variables in [u] need to be
                            removed because there are none. *)
    | Branch _ ->
      dp_map t
        ~f:(fun par ->
          Action.Par.fold par ~init:drop ~f:(fun acc seq ->
            let u' = restrict (Action.Seq.to_hvs seq) u in
            (sum (prod (const Action.Par.(singleton seq)) u') acc)))
        ~g:(fun v t f -> cond v t f)
        ~find_or_add:(fun t -> BinTbl.find_or_add seq_tbl (t,u))

  let union t u = sum t u

  let big_union fdds = List.fold ~init:drop ~f:union fdds

  let star' lhs t =
    let rec loop acc power =
      let power' = seq power t in
      let acc' = union acc power' in
      if equal acc acc'
        then acc
        else loop acc' power'
    in
    loop id lhs

  let star t = star' id t

  let rec of_local_pol_k p k =
    match p with
    | Filter p        -> k (of_pred p)
    | Mod   (h, v, m) -> k (of_mod  h v m)
    | Union (p, q)    ->
      of_local_pol_k p (fun p' ->
          of_local_pol_k q (fun q' ->
              k (union p' q')))
    | Seq   (p, q)    ->
      of_local_pol_k p (fun p' ->
          if Portless_FDD.equal p' Portless_FDD.drop then
            k Portless_FDD.drop
          else
            of_local_pol_k q (fun q' ->
                k (seq p' q')))
    | Star p -> of_local_pol_k p (fun p' -> k (star p'))

  let rec of_local_pol p = of_local_pol_k p ident

  let to_local_pol t =
    fold t ~f:Action.to_policy ~g:(fun v t f ->
      let p = Pattern.to_pred v in
      match t, f with
      | Filter t, Filter f ->
        Frenetic_NetKAT_Portless_Optimize.(mk_filter (mk_or (mk_and p t)
                                                        (mk_and (mk_not p) f)))
      | _       , _        ->
        Frenetic_NetKAT_Portless_Optimize.(mk_union (mk_seq (mk_filter p) t)
                                             (mk_seq (mk_filter (mk_not p)) f)))
end

include Local_Portless_FDD

type cache
  = [ `Keep
    | `Empty
    | `Preserve of t ]

type compiler_options = {
    cache_prepare: cache;
    field_order: order;
    remove_tail_drops: bool;
    dedup_flows: bool;
    optimize: bool;
}

let default_compiler_options = {
  cache_prepare = `Empty;
  field_order = `Heuristic;
  remove_tail_drops = false;
  dedup_flows = true;
  optimize = true;
}

let prepare_compilation ~options (pol: policy) = begin
  (match options.cache_prepare with
   | `Keep -> ()
   | `Empty -> Local_Portless_FDD.clear_cache ~preserve:Int.Set.empty
   | `Preserve fdd -> Local_Portless_FDD.clear_cache ~preserve:(Local_Portless_FDD.refs fdd));
  (match options.field_order with
   | `Heuristic -> Field.auto_order pol
   | `Default -> Field.set_order Field.all
   | `Static flds -> Field.set_order flds)
end

let compile_local ?(options=default_compiler_options) pol =
  prepare_compilation ~options pol; of_local_pol pol


let abs_loc_to_host id = assert (is_loc_host id); (Int64.shift_right_logical id 1)
let abs_loc_to_switch id = assert (is_loc_switch id); (Int64.shift_right_logical id 1)

let src_to_dst_list ~src_loc topo =
  List.fold topo ~init:[]
    ~f:(fun acc (src, dst) ->
        match dst with
        | Switch (dst_id, dst_pt) ->
          if link_to_abs_loc src = src_loc then
            (dst_id, dst_pt) :: acc
          else
            acc
        | _ -> acc)

let dst_to_src_list ~dst_loc topo =
  List.fold topo ~init:[]
    ~f:(fun acc (src, dst) ->
        match src with
        | Switch (src_id, src_pt) ->
          if link_to_abs_loc dst = dst_loc then
            (src_id, src_pt) :: acc
          else
            acc
        | _ -> acc)

module Portful = Frenetic_NetKAT

let rec portify_pred pred topo =
  match pred with
  | True -> Portful.True
  | False -> Portful.False
  | And (pred1, pred2) -> Portful.And (portify_pred pred1 topo, portify_pred pred2 topo)
  | Or (pred1, pred2) -> Portful.Or (portify_pred pred1 topo, portify_pred pred2 topo)
  | Neg (pred) -> Portful.Neg (portify_pred pred topo)
  | Test (header, value, mask) -> match header with
    | EthSrc -> Portful.Test (Portful.EthSrc value)
    | EthDst -> Portful.Test (Portful.EthDst value)
    | Vlan -> Portful.Test (Portful.Vlan (Int64.to_int_exn value))
    | VlanPcp -> Portful.Test (Portful.VlanPcp (Int64.to_int_exn value))
    | EthType -> Portful.Test (Portful.EthType (Int64.to_int_exn value))
    | IPProto -> Portful.Test (Portful.IPProto (Int64.to_int_exn value))
    | IP4Src -> Portful.Test (Portful.IP4Src (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | IP4Dst -> Portful.Test (Portful.IP4Dst (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | TCPSrcPort -> Portful.Test (Portful.TCPSrcPort (Int64.to_int_exn value))
    | TCPDstPort -> Portful.Test (Portful.TCPDstPort (Int64.to_int_exn value))
    | AbstractLoc -> Portful.Test (Portful.Switch (abs_loc_to_switch value))
    | From ->
      let from_list = src_to_dst_list ~src_loc:value topo in
      List.fold from_list
        ~init:(Portful.False)
        ~f:(fun acc (sw, pt) ->
            Portful.Or (acc, Portful.And (
                (Portful.Test (Portful.Switch sw)),
                (Portful.Test (Portful.Location (Portful.Physical (Int32.of_int64_exn pt)))))))

let rec portify (portless_pol: policy) topo: Portful.policy =
  match portless_pol with
  | Union (pol1, pol2) -> Portful.Union (portify pol1 topo, portify pol2 topo)
  | Seq (pol1, pol2) -> Portful.Seq (portify pol1 topo, portify pol2 topo)
  | Star pol -> Portful.Star (portify pol topo)
  | Filter pred -> Portful.Filter (portify_pred pred topo)
  | Mod (header, value, mask) -> match header with
    | EthSrc -> Portful.Mod (Portful.EthSrc value)
    | EthDst -> Portful.Mod (Portful.EthDst value)
    | Vlan -> Portful.Mod (Portful.Vlan (Int64.to_int_exn value))
    | VlanPcp -> Portful.Mod (Portful.VlanPcp (Int64.to_int_exn value))
    | EthType -> Portful.Mod (Portful.EthType (Int64.to_int_exn value))
    | IPProto -> Portful.Mod (Portful.IPProto (Int64.to_int_exn value))
    | IP4Src ->  Portful.Mod (Portful.IP4Src (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | IP4Dst ->  Portful.Mod (Portful.IP4Dst (Int64.to_int32_exn value, Int.to_int32_exn mask))
    | TCPSrcPort -> Portful.Mod (Portful.TCPSrcPort (Int64.to_int_exn value))
    | TCPDstPort -> Portful.Mod (Portful.TCPDstPort (Int64.to_int_exn value))
    | AbstractLoc ->
      let sw_port_list = dst_to_src_list ~dst_loc:value topo in
      List.fold sw_port_list
        ~init:(if is_loc_host value then Portful.drop else Portful.Filter (Portful.Test (Portful.Switch (abs_loc_to_switch value))))
        ~f:(fun acc (sw, mod_pt) ->
            let portful_test = Portful.Test (Portful.Switch sw) in
            let portful_mod = Portful.Mod (Portful.Location (Portful.Physical (Int32.of_int64_exn mod_pt))) in
            Portful.Union (acc, Portful.Seq (Portful.Filter portful_test, portful_mod)))
    | From -> Portful.id

let compile portless_pol topo =
  let compiled = compile_local portless_pol in
  let portless_pol = to_local_pol compiled in
  portify portless_pol topo
