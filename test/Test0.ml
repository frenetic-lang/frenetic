open SDN_Types

(*
val accept_switches : int -> 
  (unit Lwt.u * SDN_Types.switchFeatures Lwt_stream.t) Lwt.t

val setup_flow_table : SDN_Types.switchId -> SDN_Types.flowTable -> unit Lwt.t
*)

let handle_switch (features : switchFeatures) : unit Lwt.t =
  let act = Action (SeqP (Act OutputAllPorts ) ) in
  SDN.setup_flow_table features.switch_id
    [{ pattern = FieldMap.empty;
       action = act;
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
      }]

let main =
	lwt (stop_accepting, new_switches) = SDN.accept_switches 6633 in
	Lwt_stream.iter_p handle_switch new_switches

let () = Lwt_main.run main