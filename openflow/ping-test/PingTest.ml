open Core.Std
open Async.Std

open SDN_Types
open VInt
open Async_OpenFlow

let handle_switch platform (features : switchFeatures) : unit Deferred.t =
  let open Pattern in
  SDN.install_flows platform features.switch_id
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
  >>| function
    | Result.Ok () -> Printf.printf "Installed switch %Ld\n%!" features.switch_id
    | Result.Error exn ->
      Printf.eprintf  "Error: %s\n%!" (Exn.to_string exn)

let main () =
  SDN.create ~port:6633 ()
  >>> fun ctl ->
    Deferred.don't_wait_for (Pipe.iter (SDN.listen ctl) ~f:(function
      | `Connect(sw_id, features) -> handle_switch ctl features
      | `Disconnect _
      | _ ->
        Printf.printf "Ignoring non-connect event\n%!"; return ()))

let () = never_returns (Scheduler.go_main ~main ())
