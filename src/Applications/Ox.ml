open Lwt
open Unix
open Ox_Controller
open OpenFlow0x01

module Log = Frenetic_Log

module Repeater (OxPlatform:OXPLATFORM) = 
struct
  let switchConnected sw = Lwt.return ()

  let switchDisconnected sw = Lwt.return ()

  let packetIn sw pktIn = 
    match pktIn.packetInBufferId with 
      | None -> 
	Lwt.return ()
      | Some bufId -> 
	let pktOut = { pktOutBufOrBytes = Buffer bufId;
		       pktOutPortId = Some pktIn.packetInPort;
		       pktOutActions = [Action.Output PseudoPort.Flood] } in 
	OxPlatform.packetOut sw pktOut;
	Lwt.return ()

  let statsReply sw stats = Lwt.return ()
end

module OxPlatform = MakeOxPlatform(OpenFlow0x01_Platform)

module Controller = Ox_Controller.Make(OpenFlow0x01_Platform)(Repeater(OxPlatform))

let main () = 
  OpenFlow0x01_Platform.init_with_port 6633 >>
  Controller.start_controller ()
      
let _ =
  Log.printf "Main" "--- Welcome to Ox ---\n%!";
  Sys.catch_break true;
  try 
    Lwt_main.run (main ())
  with exn -> 
    Log.printf "Main" "Unexpected exception: %s\n%s\n%!" 
      (Printexc.to_string exn) 
      (Printexc.get_backtrace ());
    exit 1
