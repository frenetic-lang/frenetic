module Controller = NetCore_Controller.Make(OpenFlow0x01_Platform)

type modeType =
  | ControllerMode (* start as a controller *)
  | ParserMode (* only parse (for testing) *)

let mode = ref ControllerMode

let policy_filename = ref ""

let listenPort = ref 6633

let arg_spec =
  [ ("-parse-only", 
     Arg.Unit (fun () -> mode := ParserMode),
     "parse the file, but do not start the controller") ;
     
    ("-version", 
     Arg.Unit (fun () -> Printf.printf "\n1.0.2\n\n"),
     "show version") ;
     
    ("-port", 
     Arg.Int (fun port -> listenPort := port),
     "set listenPort := port") ;
  ]

let usage =
    "Usage: frenetic [OPTION]... FILE\n \
     Starts the Frenetic controller, running the policy in FILE."

let () =
  Arg.parse
    arg_spec
    (fun fname -> policy_filename := fname)
    usage

let policy = match !policy_filename with
  | "" -> Arg.usage arg_spec usage; exit 1
  | fname -> NetCore_Parsing.compile_program (NetCore_Parsing.parse_by_extension fname)

let () = match !mode with
  | ParserMode -> Printf.printf "Parsed OK\n"
  | ControllerMode ->
    let main () = 
      OpenFlow0x01_Platform.init_with_port !listenPort >>
        let (gen_stream, stream) = policy in
        let (pkt_stream, push_pkt) = Lwt_stream.create () in
        Lwt.pick [gen_stream; Controller.start_controller pkt_stream stream] in
    Sys.catch_break true;
    try 
      Lwt_main.run (main ())
    with exn -> (* TODO(arjun): fairly certain this is not needed *)
      Printf.eprintf "unhandled exception: %s\n%s\n%!" 
        (Printexc.to_string exn) 
        (Printexc.get_backtrace ());
      exit 1
