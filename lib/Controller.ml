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
(* let seq_to_action (seq : ONF.seq) : SDN.action = *)
