open SDN_Types
open VInt

let handle_switch (features : switchFeatures) : unit Lwt.t =
  lwt () = SDN.setup_flow_table features.switch_id
    [{ pattern = FieldMap.singleton InPort (Int32 1l);
       action = [ [[OutputPort (Int32 2l)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     };
     { pattern = FieldMap.singleton InPort (Int32 2l);
       action = [ [[OutputPort (Int32 1l)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     }
    ] in
   Lwt.return (Printf.printf "Installed switch %s\n%!" (get_string features.switch_id))

let main =
	lwt (stop_accepting, new_switches) = SDN.accept_switches 6633 in
	Lwt_stream.iter_p handle_switch new_switches

let () = Lwt_main.run main
