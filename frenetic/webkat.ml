open Core.Std
open Async.Std

let main () : unit =
  let http_app = Http_app.listen ~port:9000 in
  don't_wait_for
    (Async_NetKAT_Controller.start http_app () >>= fun ctrl ->
     Async_NetKAT_Controller.disable_discovery ctrl)

let () =
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())



