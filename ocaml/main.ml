open Printf
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef

module Controller = Maclearning.Make (OpenFlowPlatform)

let main () = 
  Sys.catch_break true;
  try 
    begin try
      OpenFlowPlatform.init_with_port 6633;
      Controller.start ()
    with exn -> 
      Printf.eprintf "[main] exception\n%!";
      OpenFlowPlatform.shutdown ();
      raise exn
    end
  with Sys.Break -> 
    Printf.eprintf "[main] got break, exiting\n%!";
    exit 1
      
let _ = main ()
