let listenPort = ref 6633

let arg_spec =
  [ ("-port", 
     Arg.Int (fun port -> listenPort := port),
     "set listenPort := port")  
  ]

let usage =
    "Usage: maclearning [OPTION]... FILE\n \
     Starts the Frenetic MAC learning controller, running the policy in FILE."

let () =
  Arg.parse
    arg_spec
    (fun _ -> ())
    usage

let () = 
  let main () = 
    OpenFlow0x01_Platform.init_with_port !listenPort >>
      let (init,pol) = NetCore_MacLearning.make () in 
      let (gen_stream, stream) = NetCore_Stream.from_stream init pol in 
      let (pkt_stream, push_pkt) = Lwt_stream.create () in
      Lwt.pick [gen_stream; NetCore_Controller.start_controller pkt_stream stream] in
  Sys.catch_break true;
  Lwt_main.run (main ())
    
