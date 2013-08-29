module SDN = SDN_types
module NetKAT = NetKAT_Types

let pred_to_pattern (sw : SDN.fieldVal) (pred : ONF.pred) : SDN.pattern option =
	let f (h : NetKAT.hdr) (v : NetKAT.hdrVal) (pat : SDN.pattern) =
	  match h with
	    | NetKAT.Switch -> pat (* already tested for this *)
      | NetKAT.Hdr h' -> SDN.FieldMap.add h' v pat in
	if NetKAT.HdrMap.mem NetKAT.Switch pred &&
	   NetKAT.HdrMap.find NetKAT.Switch pred = sw then
	   None
  else 
    Some (NetKAT.HdrMap.fold f pred SDN.FieldMap.empty)

(* Take care to emit the packet last. All other updates can happen in
   any order. Signals an exception if the switch is being updated. No
   error if an un-updatable field is updated. That error occurs later, while
   translating to specific versions of OpenFlow. *)
let seq_to_action (seq : ONF.seq) : SDN.action =
  if not (NetKAT.HdrMap.mem (NetKAT.Hdr SDN.InPort) seq) then
    SDN.EmptyAction
  else
    let port = NetKAT.HdrMap.find (NetKAT.Hdr SDN.InPort) seq in
    let mods = NetKAT.HdrMap.remove (NetKAT.Hdr SDN.InPort) seq in
    let mk_mod (h : NetKAT.hdr) (v : NetKAT.hdrVal) (action : SDN.action) =
      match h with
      | NetKAT.Switch -> raise (Invalid_argument "seq_to_action got switch update")
      | NetKAT.Hdr h' ->  SDN.Seq (SDN.SetField (h', v), action) in
    NetKAT.HdrMap.fold mk_mod mods (SDN.OutputPort port)

let sum_to_action (sum : ONF.sum) : SDN.action =
  let f (seq : ONF.seq) (action : SDN.action) =
    SDN.Par (seq_to_action seq, action) in
  ONF.HdrValSet.fold f sum SDN.EmptyAction

let simpl_flow (p : SDN.pattern) (a : SDN.action) : SDN.flow = {
  SDN.pattern = p;
  SDN.action = a;
  SDN.cookie = 0L;
  SDN.idle_timeout = SDN.Permanent;
  SDN.hard_timeout = SDN.Permanent
}

(* Prunes out rules that apply to other switches. *)
let rec local_to_tbl (sw : SDN.fieldVal) (local : ONF.local) : SDN.flowTable =
  match local with
  | ONF.Action sum -> [simpl_flow SDN.FieldMap.empty (sum_to_action sum)]
  | ONF.ITE (if_, then_, else_) ->
    (match pred_to_pattern sw if_ with
     | None -> local_to_tbl sw else_
     | Some pat ->
        (simpl_flow pat (sum_to_action then_)) :: (local_to_tbl sw else_))