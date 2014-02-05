open Core.Std
open Async.Std

open SDN_Types
open VInt
open Async_OpenFlow

let handle_switch platform (features : switchFeatures) : unit Deferred.t =
  Highlevel.setup_flow_table platform features.switch_id
    [{ pattern = FieldMap.singleton InPort (Int16 1);
       action = [ [[OutputPort (Int16 2)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     };
     { pattern = FieldMap.singleton InPort (Int16 2);
       action = [ [[OutputPort (Int16 1)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     }
    ]
  >>= fun () ->
  (Printf.printf "Installed switch %Ld\n%!" features.switch_id;
   return ())

let () =
  let _ = Highlevel.create ~port:6633 ()
          >>= fun platform ->
          Pipe.iter (Highlevel.accept_switches platform) (handle_switch platform) 
  in never_returns (Scheduler.go ())
