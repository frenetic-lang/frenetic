open Core.Std
open Async.Std


module Log = Async_OpenFlow.Log
let tags = [("netkat", "updates-test")]

module SwitchMap = Map.Make(Int64)

let create () =
  let open Async_NetKAT in
  let open NetKAT_Types in

  let default = Mod(Location(Pipe "updates")) in

  (* let pol  = *)
  (*   Seq(Filter(Test(Switch 1L)), *)
  (*       Union(Seq(Filter(Test(Location(Physical 1l))), Mod(Location(Physical 2l))), *)
  (*             Seq(Filter(Test(Location(Physical 2l))), Mod(Location(Physical 1l))))) *)
  (* in *)
  let pol = Async_NetKAT.default (create_from_file "examples/tree-2-2.kat") in

  let handler t w () e = match e with
    | SwitchUp(switch_id) ->
      return (Some pol)
    | SwitchDown(switch_id) ->
      return (Some pol)
    | _ -> return (Some pol) in
      
  create ~pipes:(PipeSet.singleton "updates-test") pol handler

let _ = 
  let main () =
    let app = create () in
    Async_NetKAT_Controller.start app () in
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())


