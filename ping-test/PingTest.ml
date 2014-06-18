open Core.Std
open Async.Std

open SDN_Types
open VInt
open Async_OpenFlow

let handle_switch platform (features : switchFeatures) : unit Deferred.t =
  let open Pattern in
  Highlevel.setup_flow_table platform features.switch_id
    [{ pattern = { match_all with inPort = Some(1l) };
       action = [ [[Output (Physical 2l)]] ];
       cookie = 0L;
       idle_timeout = Permanent;
       hard_timeout = Permanent
     };
     { pattern = { match_all with inPort = Some(2l) };
       action = [ [[Output (Physical 1l)]] ];
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
