open Lwt
open Printf
open Unix

module M = NetCore_MacLearning

module Controller = NetCore_Controller.Make(OpenFlow0x01_Platform)

let drop_all = NetCore_Types.PoAction (NetCore_Action.Output.drop)
let policy = ref (Lwt.return (), NetCore_Stream.constant drop_all)

let () =
  Arg.parse
    [ ]
    (fun filename -> 
      policy := NetCore_Parsing.parse_from_chan (open_in filename) filename)
    "usage: netcore filename"

let main () = 
  OpenFlow0x01_Platform.init_with_port 6633 >>
  let (gen_stream, stream) = !policy in
  Lwt.pick [gen_stream; Controller.start_controller stream]

(* TODO(arjun): I do not 100% approve of this code. *)
let _ =
  Sys.catch_break true;
  try 
    Lwt_main.run (main ())
  with exn -> 
    Printf.eprintf "[main] exception: %s\n%s\n%!" 
      (Printexc.to_string exn) 
      (Printexc.get_backtrace ());
    exit 1
