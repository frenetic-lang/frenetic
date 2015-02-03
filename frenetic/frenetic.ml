open Core.Std
open Async.Std


let main () : unit = match Sys.argv |> Array.to_list with
  | "compile-server" :: args -> Compile_Server.main args
  | "http-controller" :: args -> Http_Controller.main args
  | _ ->
    printf "Invalid arguments.\n";
    Shutdown.shutdown 0

let () =
  never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())
