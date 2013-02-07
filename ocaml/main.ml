open Printf
open Openflow1_0
open Platform 
open Unix
open MessagesDef

module Controller = Maclearning.Make (OpenFlowPlatform)

let main () = 
  Sys.catch_break true;
  try 
    OpenFlowPlatform.init_with_port 6633;
    Lwt_main.run (Controller.start ())
  with exn -> 
    Printf.eprintf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) (Printexc.get_backtrace ());
    OpenFlowPlatform.shutdown ();
    exit 1
      
let _ = main ()
