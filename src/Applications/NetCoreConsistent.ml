open Lwt
open Printf
open Unix

module M = NetCore_MacLearning

module Controller = NetCore_Controller.MakeConsistent(OpenFlow0x01_Platform)

let drop_all = NetCore_Types.Action (NetCore_Action.Output.drop)
let policy = ref (Lwt.return (), NetCore_Stream.constant drop_all)

let parse_by_extension filename =
  if String.length filename < 3 then
    failwith "missing file extension"
  else if Str.last_chars filename 3 = ".md" then
    NetCore_Parsing.parse_literate_from_chan (open_in filename) filename
  else if Str.last_chars filename 3 = ".nc" then
    NetCore_Parsing.parse_from_chan (open_in filename) filename
  else 
    failwith "unknown file extension"

let () =
  Arg.parse
    [ ]
    (fun filename ->  policy := parse_by_extension filename)
    "usage: netcore filename"

let main () = 
  OpenFlow0x01_Platform.init_with_port 6633 >>
  let (gen_stream, stream) = !policy in
  let (pkt_stream, push_pkt) = Lwt_stream.create () in
  Lwt.pick [gen_stream; Controller.start_controller pkt_stream stream]

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
