open NetKAT_Types
open Optimize

let union (ps : policy list) : policy = List.fold_left mk_union drop ps

let seq (ps : policy list) : policy = List.fold_left mk_seq id ps

let and2 (ps : pred list) : pred = List.fold_left mk_and True ps

let final_local_pc = 0
let initial_local_pc = final_local_pc + 1
let initial_global_pc = 0xffff

let match_pc pc = Filter (Test (Vlan pc))

let set_pc pc = Mod (Vlan pc)

let unset_pc = Mod (Vlan 0xffff)

let match_location (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))

let match_link_end sw pt pc =
  let t1 = Test (Vlan pc) in
  let t2 = Test (Switch sw) in
  let t3 = Test (Location(Physical(pt))) in
  Filter (and2 [t1; t2; t3])

type cps_policy =
| Entrance of policy
| Local of policy
| Exit of policy

let cps (p : policy) =
  let module M = Map.Make (struct type t = switchId * portId let compare = compare end) in
  let local_pc_ref = ref final_local_pc in
  let global_pc_ref = ref M.empty in
  let next_local_pc () =
    (local_pc_ref := (!local_pc_ref + 1); !local_pc_ref) in
  let next_global_pc sw pt =
    let m = !global_pc_ref in
    let pc = try M.find (sw, pt) m with Not_found -> initial_global_pc in
    (global_pc_ref := M.add (sw, pt) (pc-1) m; pc) in
  let rec cps' p pc k =
    match p with
    | Filter _ | Mod _ ->
       [Local (seq [match_pc pc; p; set_pc k])]
    | Union (q,r) ->
       let pc_q = next_local_pc () in
       let pc_r = next_local_pc () in
       Local (seq [match_pc pc ; union [set_pc pc_q; set_pc pc_r]]) ::
       (cps' q pc_q k) @ (cps' r pc_r k)
    | Seq (q,r) ->
       let pc' = next_local_pc () in
       (cps' q pc pc') @ (cps' r pc' k)
    | Star q ->
       let pc_q = next_local_pc () in
       Local (seq [match_pc pc ; union [set_pc pc_q; set_pc k]]) :: (cps' q pc_q pc)
    | Link (sw1,pt1,sw2,pt2) ->
       let gpc = next_global_pc sw2 pt2 in
       [Exit (seq [match_link_end sw1 pt1 pc; set_pc gpc]);
        Entrance (seq [match_link_end sw2 pt2 gpc; set_pc k])] in
  cps' p (next_local_pc ()) final_local_pc

let split_cps (cps : cps_policy list) =
  let clasify_cps_policy (entrance, local, exit) = function
    | Entrance p -> (p::entrance, local, exit)
    | Local p -> (entrance, p::local, exit)
    | Exit p -> (entrance, local, p::exit) in
  List.fold_left clasify_cps_policy ([],[],[]) cps

let compile (ingress : pred) (egress : pred) (p : policy) =
  let (entrance, local, exit) = split_cps (cps p) in
  let pre = seq [Filter ingress; set_pc initial_local_pc] in
  let post = seq [match_pc final_local_pc; Filter egress; unset_pc] in
  seq [union (pre::entrance); mk_star (union local); union (post::exit)]

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
