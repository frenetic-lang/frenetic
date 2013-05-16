open Lwt
open Printf
open Unix
open NetCore

module Controller = NetCore.Make(OpenFlow0x01.Platform)

(* configuration state *)
let controller = ref "learning"

(* command-line arguments *)
let arg_specs = 
  [ ("-c", 
     Arg.Set_string controller, 
     "<controller> run a specific controller, one of:\n\
      learning")
  ]
 
let arg_rest rest = ()

let usage = "desmoines [options]"

let () = Arg.parse arg_specs arg_rest usage

let main () = 
  if !controller = "learning" then 
    begin
      OpenFlow0x01.Platform.init_with_port 6633; 
      Controller.start_controller Learning.Routing.policy
    end
  else
    failwith (Printf.sprintf "Unknown controller: %s" !controller)
      
let _ =
  Sys.catch_break true;
  try 
    Lwt_main.run (main ())
  with exn -> 
    Misc.Log.printf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) 
      (Printexc.get_backtrace ());
    exit 1
