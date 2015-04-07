open Core.Std
open Async.Std


module Log = Async_OpenFlow.Log
let tags = [("netkat", "updates-test")]

module SwitchMap = Map.Make(Int64)

let create () =
  let open Async_NetKAT in
  let open NetKAT_Types in

  let pol = Async_NetKAT.(default (Policy.create_from_file "examples/tree-2-2.kat")) in

  let handler t w () e = match e with
    | SwitchUp(switch_id, ps) ->
      return (Some pol)
    | SwitchDown(switch_id) ->
      return (Some pol)
    | _ -> return (Some pol) in
      
  Policy.create ~pipes:(PipeSet.singleton "updates-test") pol handler

let _ = 
  let main () =
    let app = create () in
    Async_NetKAT_Controller.start app ~update:`PerPacketConsistent ()
    >>> fun t -> ()
  in
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())
