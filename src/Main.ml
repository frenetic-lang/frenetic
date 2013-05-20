open Lwt
open Printf
open Unix
open Syntax.External

module Controller = Controller.Make(OpenFlow0x01.Platform)

(* configuration state *)
let controller = ref ""

(* command-line arguments *)
let arg_specs = 
  [ ("-c", 
     Arg.Set_string controller, 
     "<controller> run a specific controller")
  ]
 
let arg_rest rest = ()

let usage = "desmoines [options]"

let () = Arg.parse arg_specs arg_rest usage

let main () = 
  let stream, push = Lwt_stream.create() in  
  (* JNF: kind of a hack that we have to call this function :-( *)
  push (Some (Act ToAll));
  OpenFlow0x01.Platform.init_with_port 6633; 
  Controller.start_controller stream  
      
let _ =
  Sys.catch_break true;
  try 
    Lwt_main.run (main ())
  with exn -> 
    Misc.Log.printf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) 
      (Printexc.get_backtrace ());
    exit 1
