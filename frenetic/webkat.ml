open Core.Std
open Async.Std

let main () =
  don't_wait_for 
    (Http_app.create NetKAT_Types.drop "localhost" 5000 ~path:"/netkat/app/" >>=
    (fun app -> return (Async_NetKAT_Controller.start app ())))

let () = 
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())



