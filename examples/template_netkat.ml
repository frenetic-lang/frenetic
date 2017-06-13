open Core
open Async.Std
open Frenetic_NetKAT

let initial_policy : policy = Mod(Location(Pipe("controller")))

let rec handle_events (module Controller : Frenetic_NetKAT_Controller.CONTROLLER) =
  let open Controller in
  event () >>=
  fun evt ->
    let response = Frenetic_NetKAT_Json.event_to_json_string evt in
    printf "%s\n%!" response;
    handle_events (module Controller)

let _ =
  let module Controller = Frenetic_NetKAT_Controller.Make in
  Controller.start 6633;
  Controller.update_policy initial_policy;
  don't_wait_for(handle_events (module Controller));
  never_returns (Scheduler.go ());
