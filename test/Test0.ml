open SDN_Types
open VInt

let handle_switch (features : switchFeatures) : unit Lwt.t =
  SDN.setup_flow_table features.switch_id
    [{ pattern = FieldMap.singleton InPort (Int32 1l);
       action = [ [[OutputPort (Int32 2l)]];
                  [[OutputPort (Int32 3l)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     };
     { pattern = FieldMap.singleton InPort (Int32 2l);
       action = [ [[OutputPort (Int32 1l)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     };
     { pattern = FieldMap.singleton InPort (Int32 3l);
       action = [ [[OutputPort (Int32 1l)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     }
    ]

let main =
	lwt (stop_accepting, new_switches) = SDN.accept_switches 6633 in
	Lwt_stream.iter_p handle_switch new_switches

let () = Lwt_main.run main