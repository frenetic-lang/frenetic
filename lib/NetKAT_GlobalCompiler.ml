open NetKAT_Types
open Optimize

let set_pc pc = Mod (Vlan pc)
let unset_pc = Mod (Vlan 0xffff)
let match_pc pc = mk_filter (Test (Vlan pc))

let match_location sw pt =
  let t1 = Test (Switch sw) in
  let t2 = Test (Location (Physical pt)) in
  mk_filter (mk_and t1 t2)

(** Note that 0xffff = null. Using null as the inital global pc is a convenient hack with the result
    that the vlan field is only used if it is actually needed, i.e. if there are several global
    program counters *)
let initial_global_pc = 0xffff


let rec tag_links (pol : policy) : policy =
  let module T = Core.Std.Hashtbl.Poly in
  let pc_tbl = T.create () in
  let next_pc sw pt =
    let pc = T.find pc_tbl (sw, pt) |> Core.Std.Option.value ~default:initial_global_pc in
    T.replace pc_tbl ~key:(sw, pt) ~data:(pc-1); pc
  in
  let rec tag p =
    match p with
    | Filter _ | Mod _ -> p
    | Union (p,q) -> mk_union (tag p) (tag q)
    | Seq (p,q) -> mk_seq (tag p) (tag q)
    | Star p -> mk_star (tag p)
    | Link (_,_,sw2,pt2) ->
      let pc = next_pc sw2 pt2 in
      mk_big_seq [set_pc pc; p; match_pc pc]
  in
  tag pol

(* splits policy into E, D, and K policy *)
let rec split_pol (pol: policy) : policy * policy * policy =
  match pol with
  | Filter _ | Mod _ -> (pol, drop, drop)
  | Union (p,q) ->
    let (e_p, d_p, k_p) = split_pol p in
    let (e_q, d_q, k_q) = split_pol q in
    (mk_union e_p e_q, mk_union d_p d_q, mk_union k_p k_q)
  | Seq (p,q) ->
    let (e_p, d_p, k_p) = split_pol p in
    let (e_q, d_q, k_q) = split_pol q in
    let e = mk_seq e_p e_q in
    (* SJS: loss of precision!!! Sound but not optimal *)
    let e_p_ind = if e_p = drop then drop else id in
    let d = mk_union d_p (mk_seq e_p d_q) in
    let k = mk_union (mk_seq e_p_ind k_q) (mk_seq k_p q) in
    (* inline fdds into policies to avoid duplication *)
    (e, d, k)
  | Star p ->
    let (e_p, d_p, k_p) = split_pol p in
    let e = mk_star e_p in
    let d = mk_seq e d_p in
    let k = mk_seq k_p (mk_union e d) in
    (e, d, k)
  | Link (sw1,pt1,sw2,pt2) -> (drop, match_location sw1 pt1, match_location sw2 pt2)

let compile (ingress : pred) (egress : pred) (p : policy) =
  let ingress = mk_filter ingress in
  let egress = mk_filter egress in
  let p = tag_links p in
  let rec loop e_acc d_acc p =
    let (e, d, k) = split_pol p in
    let e_acc = mk_union e_acc e in
    let d_acc = mk_union d_acc d in
    if k = drop then
      mk_union d_acc (mk_seq e_acc egress)
    else
      loop e_acc d_acc k
  in
  loop drop drop (mk_seq ingress p)

let compile_pol ~(pol : policy) : policy = match pol with
  | Seq (Filter ingress, Seq (p, Filter egress)) -> compile ingress egress p
  | _ -> assert false
