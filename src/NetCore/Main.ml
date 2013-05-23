open Lwt
open Printf
open Unix
open NetCore_Types.External

module M = NetCore_MacLearning

module Controller = NetCore_Controller.Make(OpenFlow0x01_Platform)

let policy = ref (NetCore_Stream.constant Empty)

let () =
  Arg.parse
    [ ]
    (fun filename -> policy := NetCore_Compiler.parse_from_chan (open_in filename) filename)
    "Usage: netcore FILENAME"

let main () = 
  (* JNF: kind of a hack that we have to call this function :-( *)
  OpenFlow0x01_Platform.init_with_port 6633 >>
  Controller.start_controller !policy
      
let _ =
  Sys.catch_break true;
  try 
    Lwt_main.run (main ())
  with exn -> 
    Printf.eprintf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) 
      (Printexc.get_backtrace ());
    exit 1
