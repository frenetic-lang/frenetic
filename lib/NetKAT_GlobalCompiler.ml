open NetKAT_Types
open Optimize

let union (ps : policy list) : policy = List.fold_left mk_union drop ps

let seq (ps : policy list) : policy = List.fold_left mk_seq id ps

let or2 (ps : pred list) : pred = List.fold_left mk_or False ps

let plus (p : policy) = mk_seq p (mk_star p)

let final_pc = 0
let initial_pc = final_pc + 1

let match_pc pc = Filter (Test (Vlan pc))

let set_pc pc = Mod (Vlan pc)

let partition_jump_table t =
  let file_table_row (enter, local, exit) row =
    match row with
    | `Enter q -> (q::enter, local, exit)
    | `Local q -> (enter, q::local, exit)
    | `Exit q -> (enter, local, q::exit) in
  List.fold_left file_table_row ([],[],[]) t

let cps (p : policy) = 
  let local_pc_ref = ref final_pc in
  let global_pc_ref = ref final_pc in
  let next_pc (pc_type : [`Local|`Global]) = 
    match pc_type with
    | `Local -> (local_pc_ref := (!local_pc_ref + 1); !local_pc_ref)
    | `Global -> (global_pc_ref := (!global_pc_ref + 1); !global_pc_ref) in
  let filter (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt))))) in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
        [`Local (seq [match_pc pc; p; set_pc k])]
    | Union (q,r) ->
       let pc_q = next_pc `Local in
       let pc_r = next_pc `Local in
       `Local (seq [match_pc pc ; union [set_pc pc_q; set_pc pc_r]]) ::
       (cps' q pc_q k) @ (cps' r pc_r k)
    | Seq (q,r) ->
       (* TODO: is this correct to inline pc |-> pc of q? *)
       let pc' = next_pc `Local in
       (cps' q pc pc') @ (cps' r pc' k)
    | Star q ->
       let pc_q = next_pc `Local in
       `Local (seq [match_pc pc ; union [set_pc pc_q; set_pc k]]) ::
       (cps' q pc_q pc)
    | Link (sw1,pt1,sw2,pt2) -> 
       let gpc = next_pc `Global in 
       [`Exit (seq [match_pc pc; filter (sw1,pt1); set_pc gpc]);
        `Enter (seq [match_pc gpc; filter (sw2,pt2); set_pc k]) ] in
  (*TODO: hard coded ingress & egress for experimentation; turn into parameters *)
  let ingress = [(Int64.of_int 1, Int32.of_int 1); (Int64.of_int 2, Int32.of_int 2)] in
  let egress = [(Int64.of_int 5, Int32.of_int 100); (Int64.of_int 6, Int32.of_int 100)] in
  let match_ingress = union (List.map filter ingress) in
  let match_egress = union (List.map filter egress) in
  let pre = seq [match_ingress; set_pc initial_pc] in
  (* TODO: instead of setting the final global pc, the pc should be removed *)
  let post = seq [match_pc final_pc; match_egress; set_pc final_pc] in
  let jump_table = cps' p (next_pc `Local) final_pc in
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

