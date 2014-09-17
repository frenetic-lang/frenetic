open NetKAT_Types
open Optimize

let union (ps : policy list) : policy = List.fold_left mk_union drop ps

let seq (ps : policy list) : policy = List.fold_left mk_seq id ps

let or2 (ps : pred list) : pred = List.fold_left mk_or False ps

let and2 (ps : pred list) : pred = List.fold_left mk_and True ps

let plus (p : policy) = mk_seq p (mk_star p)

let final_local_pc = 0
let initial_local_pc = final_local_pc + 1
let initial_global_pc = 0

let match_pc pc = Filter (Test (Vlan pc))

let set_pc pc = Mod (Vlan pc)

let unset_pc = Mod (Vlan 0xffff)

let match_location (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))

let match_entrance sw pt pc =
  let t1 = Test (Vlan pc) in
  let t2 = Test (Switch sw) in
  let t3 = Test (Location(Physical(pt))) in
  Filter (and2 [t1; t2; t3])

let partition_jump_table t =
  let file_table_row (enter, local, exit) row =
    match row with
    | `Enter q -> (q::enter, local, exit)
    | `Local q -> (enter, q::local, exit)
    | `Exit q -> (enter, local, q::exit) in
  List.fold_left file_table_row ([],[],[]) t

let cps (ingress : (switchId * portId) list) (egress : (switchId * portId) list) (p : policy) =
  let module M = Map.Make (struct type t = switchId * portId let compare = compare end) in
  let local_pc_ref = ref final_local_pc in
  let global_pc_ref = ref M.empty in
  let next_local_pc () =
    (local_pc_ref := (!local_pc_ref + 1); !local_pc_ref) in
  let next_global_pc sw pt =
    let m = !global_pc_ref in
    let pc = try M.find (sw, pt) m with Not_found -> initial_global_pc in
    (global_pc_ref := M.add (sw, pt) (pc+1) m; pc) in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
        [`Local (seq [match_pc pc; p; set_pc k])]
    | Union (q,r) ->
       let pc_q = next_local_pc () in
       let pc_r = next_local_pc () in
       `Local (seq [match_pc pc ; union [set_pc pc_q; set_pc pc_r]]) ::
       (cps' q pc_q k) @ (cps' r pc_r k)
    | Seq (q,r) ->
       let pc' = next_local_pc () in
       (cps' q pc pc') @ (cps' r pc' k)
    | Star q ->
       let pc_q = next_local_pc () in
       `Local (seq [match_pc pc ; union [set_pc pc_q; set_pc k]]) :: (cps' q pc_q pc)
    | Link (sw1,pt1,sw2,pt2) -> 
       let gpc = next_global_pc sw2 pt2 in 
       [`Exit (seq [match_pc pc; match_location (sw1,pt1); set_pc gpc]);
        `Enter (seq [match_entrance sw2 pt2 gpc; set_pc k]) ] in
  let match_ingress = union (List.map match_location ingress) in
  let match_egress = union (List.map match_location egress) in
  let pre = seq [match_ingress; set_pc initial_local_pc] in
  let post = seq [match_pc final_local_pc; match_egress; unset_pc] in
  let jump_table = cps' p (next_local_pc ()) final_local_pc in
  let (enter, local, exit) = partition_jump_table jump_table in
  seq [union (pre::enter); mk_star (union local); union (post::exit)]

let switches (p:policy) =
  let rec collect p =
    match p with 
    | Filter _ | Mod _ -> 
       []
    | Union(q,r) | Seq (q,r) -> 
       collect q @ collect r
    | Star q -> 
       collect q
    | Link(sw1,_,sw2,_) -> 
       [sw1;sw2] in
  collect p |> Core.Core_list.of_list |> Core.Core_list.dedup |> Core.Core_list.to_list

