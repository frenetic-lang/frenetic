open NetKAT_Types
open Optimize

let union (ps : policy list) : policy = List.fold_left mk_union drop ps

let seq (ps : policy list) : policy = List.fold_left mk_seq id ps

let or2 (ps : pred list) : pred = List.fold_left mk_or False ps

let plus (p : policy) = seq [p; Star p]

let match_pc pc = Filter (Test (VlanPcp pc))

let set_pc pc = Mod (VlanPcp pc)

let final_pc = 0
let initial_pc = final_pc + 1

let cps (p : policy) = 
  let pc_ref = ref final_pc in
  let next_pc = (fun () -> (pc_ref := (!pc_ref + 1); !pc_ref)) in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
        [seq [match_pc pc; p; set_pc k]]
    | Union (q,r) ->
       let pc_q = next_pc () in
       let pc_r = next_pc () in
       (seq [match_pc pc ; union [set_pc pc_q; set_pc pc_r]]) :: (cps' q pc_q k) @ (cps' r pc_r k)
    | Seq (q,r) ->
       (* TODO: is this correct to inline pc |-> pc of q? *)
       let pc' = next_pc () in
       (cps' q pc pc') @ (cps' r pc' k)
    | Star q ->
       let pc_q = next_pc () in
       (seq [match_pc pc ; union [set_pc pc_q; set_pc k]]) :: (cps' q pc_q pc)
    | Link (sw1,pt1,sw2,pt2) -> 
       let pc' = next_pc () in 
       let filter (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt))))) in
       [seq [match_pc pc; filter (sw1,pt1); set_pc pc'];
        seq [match_pc pc'; filter (sw2,pt2); set_pc k] ] in 
  union (cps' p (next_pc ()) final_pc)

let rec switches (p:policy) = 
  match p with 
  | Filter _ | Mod _ -> 
     []
  | Union(q,r) | Seq (q,r) -> 
     switches q @ switches r
  | Star q -> 
     switches q
  | Link(sw1,_,sw2,_) -> 
     [sw1;sw2]
     
                
(**************************************************************************************************)

let match_pc (pc_type : [`Local|`Global]) pc =
  let t1 = Test (Vlan pc) in
  let t2 = Test (VlanPcp (if pc_type=`Global then 0 else 1)) in
  Filter (And (t1, t2))

let set_pc (pc_type : [`Local|`Global]) pc =
  let m1 = Mod (Vlan pc) in
  let m2 = Mod (VlanPcp (if pc_type=`Global then 0 else 1)) in
  Seq (m1, m2)

let cps (p : policy) = 
  let local_pc_ref = ref final_pc in
  let global_pc_ref = ref final_pc in
  let next_pc (pc_type : [`Local|`Global]) = 
    match pc_type with
    | `Local -> (local_pc_ref := (!local_pc_ref + 1); !local_pc_ref)
    | `Global -> (global_pc_ref := (!global_pc_ref + 1); !global_pc_ref) in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
        [seq [match_pc `Local pc; p; set_pc `Local k]]
    | Union (q,r) ->
       let pc_q = next_pc `Local in
       let pc_r = next_pc `Local in
       (seq [match_pc `Local pc ; union [set_pc `Local pc_q; set_pc `Local pc_r]]) ::
       (cps' q pc_q k) @ (cps' r pc_r k)
    | Seq (q,r) ->
       (* TODO: is this correct to inline pc |-> pc of q? *)
       let pc' = next_pc `Local in
       (cps' q pc pc') @ (cps' r pc' k)
    | Star q ->
       let pc_q = next_pc `Local in
       (seq [match_pc `Local pc ; union [set_pc `Local pc_q; set_pc `Local k]]) :: (cps' q pc_q pc)
    | Link (sw1,pt1,sw2,pt2) -> 
       let gpc = next_pc `Global in 
       let filter (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt))))) in
       [seq [match_pc `Local pc; filter (sw1,pt1); set_pc `Global gpc];
        seq [match_pc `Global gpc; filter (sw2,pt2); set_pc `Local k] ] in
  union (cps' p (next_pc `Local) final_pc)

let wrapped_cps (p : policy) =
  (*TODO: hard coded ingress & egress for experimentation; remove *)
  let ingress = [(Int64.of_int 1, Int32.of_int 1)] in
  let egress = [(Int64.of_int 2, Int32.of_int 2)] in
  let filter (sw,pt) = And (Test(Switch sw), Test(Location(Physical(pt)))) in
  let ing_p = or2 (List.map filter ingress) in
  let eg_p = or2 (List.map filter egress) in
  let init_pc = seq [match_pc `Global initial_pc ; set_pc `Local initial_pc] in
  let finalize_pc = seq [match_pc `Local final_pc ; Filter eg_p ; set_pc `Global final_pc] in
  let pre = union [seq [Filter ing_p ; set_pc `Global initial_pc];
                   seq [Filter (Neg ing_p) ; Filter (Test (VlanPcp 0))]] in
  let post = Filter (Test (VlanPcp 0)) in
  let jump_table = union [init_pc; cps p; finalize_pc] in
  seq [pre; plus jump_table; post]

