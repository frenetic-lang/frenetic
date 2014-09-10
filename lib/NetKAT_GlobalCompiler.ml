open NetKAT_Types
open Optimize

let union (ps : policy list) : policy =
  List.fold_left (fun p q -> mk_union p q) drop ps

let seq (ps : policy list) : policy =
  List.fold_left (fun p q -> mk_seq p q) id ps

let match_pc pc = Filter (Test (VlanPcp pc))

let set_pc pc = Mod (VlanPcp pc)

let final_pc = 0

let cps (p : policy) = 
  let pc_ref = ref final_pc in
  let next_pc = (fun () -> (pc_ref := (!pc_ref + 1); !pc_ref)) in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
        [seq [match_pc pc; p; set_pc k]]
    | Union (q,r) ->
        (let pc_q = next_pc () in
         let pc_r = next_pc () in
         (seq [match_pc pc ; union [set_pc pc_q; set_pc pc_r]]) :: (cps' q pc_q k) @ (cps' r pc_r k))
    | Seq (q,r) ->
        (let pc_q = next_pc () in
         let pc_r = next_pc () in
         (seq [match_pc pc ; set_pc pc_q] :: (cps' q pc_q pc_r)) @ (cps' r pc_r k))
    | Star q ->
        (let pc_q = next_pc () in
         (seq [match_pc pc ; union [set_pc pc_q; set_pc k]]) :: (cps' q pc_q pc))
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
     
                
