module Platform = SDN
module SDN = SDN_Types
module NetKAT = NetKAT_Types
module Stream = NetCore_Stream

let pred_to_pattern (sw : SDN.fieldVal) (pred : ONF.pat) : SDN.pattern option =
	let f (h : NetKAT.header) (v : NetKAT.header_val) (pat : SDN.pattern) =
	  match h with
	    | NetKAT.Switch -> pat (* already tested for this *)
      | NetKAT.Header h' -> SDN.FieldMap.add h' v pat in
	if NetKAT.HeaderMap.mem NetKAT.Switch pred &&
	   NetKAT.HeaderMap.find NetKAT.Switch pred <> sw then
	   None
  else 
    Some (NetKAT.HeaderMap.fold f pred SDN.FieldMap.empty)

(* Take care to emit the packet last. All other updates can happen in
   any order. Signals an exception if the switch is being updated. No
   error if an un-updatable field is updated. That error occurs later, while
   translating to specific versions of OpenFlow. *)
let act_to_action (seq : ONF.act) : SDN.action =
  if not (NetKAT.HeaderMap.mem (NetKAT.Header SDN.InPort) seq) then
    SDN.EmptyAction
  else
    let port = NetKAT.HeaderMap.find (NetKAT.Header SDN.InPort) seq in
    let mods = NetKAT.HeaderMap.remove (NetKAT.Header SDN.InPort) seq in
    let mk_mod (h : NetKAT.header) (v : NetKAT.header_val) (action : SDN.action) =
      match h with
      | NetKAT.Switch -> raise (Invalid_argument "seq_to_action got switch update")
      | NetKAT.Header h' ->  SDN.Seq (SDN.SetField (h', v), action) in
    NetKAT.HeaderMap.fold mk_mod mods (SDN.OutputPort port)

let acts_to_action (sum : ONF.acts) : SDN.action =
  let f (seq : ONF.act) (action : SDN.action) =
    SDN.Par (act_to_action seq, action) in
  ONF.ActSet.fold f sum SDN.EmptyAction

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
  | [] -> 
    [] (* TODO (jnf, arjun): is it okay to produce an empty table? *)
  | (if_, then_)::else_ ->
    (match pred_to_pattern sw if_ with
     | None -> local_to_tbl sw else_
     | Some pat ->
        (simpl_flow pat (acts_to_action then_)) :: (local_to_tbl sw else_))

(* Keeps the switch configured with the latest policy on onf_stream. *)
let switch_thread 
  (onf_stream : ONF.local Stream.t)
  (feats : SDN.switchFeatures) : unit Lwt.t =
  let sw_id = feats.SDN.switch_id in
  let config_switch onf = 
    Platform.setup_flow_table sw_id (local_to_tbl sw_id onf) in
  lwt () = config_switch (Stream.now onf_stream) in
  Lwt_stream.iter_s config_switch (Stream.to_stream onf_stream)

let rec start ~port ~pols =
  let onf_stream = Stream.map ONF.compile pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
  Lwt_stream.iter_p (switch_thread onf_stream) new_switches
