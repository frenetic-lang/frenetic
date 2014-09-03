open NetKAT_Types

let union (ps : policy list) : policy =
  List.fold_left (fun p q -> Union (p,q)) drop ps

let seq (ps : policy list) : policy =
  List.fold_left (fun p q -> Seq (p,q)) id ps

let match_pc pc = Filter (Test (VlanPcp pc))

let set_pc pc = Mod (VlanPcp pc)

let cps (p : policy) (k : int) =
  let pc_ref = ref (-1) in
  let next_pc = (fun () -> (pc_ref := (!pc_ref + 1); !pc_ref)) in
  let rec cps' p pc k =
    match p with
    | (Filter _) | (Mod _) ->
        [seq [match_pc pc; p; set_pc k]]
    | (Union (q,r)) ->
        (let pc_q = next_pc () in
         let pc_r = next_pc () in
         (seq [match_pc pc ; union [set_pc pc_q; set_pc pc_r]]) :: (cps' q pc_q k) @ (cps' r pc_r k))
    | (Seq (q,r)) ->
        (let pc_q = next_pc () in
         let pc_r = next_pc () in
         (seq [match_pc pc ; set_pc pc_q] :: (cps' q pc_q pc_r)) @ (cps' r pc_r k))
    | (Star q) ->
        (let pc_q = next_pc () in
         (seq [match_pc pc ; union [set_pc pc_q; set_pc k]]) :: (cps' q pc_q pc))
    (* SJS: Figure this out *)
    | (Link _) -> [seq [match_pc pc; p; match_pc pc; set_pc k]]
  in union (cps' p (next_pc ()) k)
