open Core.Std
open Async.Std

let main () : unit =
  don't_wait_for 
    (Http_app.create NetKAT_Types.drop "localhost" 5000 ~path:"/netkat/app" () >>= fun pol -> 
     Async_NetKAT_Controller.start pol () >>= fun ctrl -> 
     return ())

let () = 
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())



