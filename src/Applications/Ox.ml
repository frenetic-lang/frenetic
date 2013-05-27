(* *)
open Lwt
open Unix
open Ox_Controller
open OpenFlow0x01

module Log = Frenetic_Log

module Repeater (OxPlatform:OXPLATFORM) = 
struct
  let switchConnected sw = ()

  let switchDisconnected sw = ()

  let statsReply xid sw stats = ()

  let packetIn xid sw pktIn = match pktIn.packetInBufferId with 
    | None -> 
      ()
    | Some bufId -> 
      let pktOut = { 
	pktOutBufOrBytes = Buffer bufId;
	pktOutPortId = Some pktIn.packetInPort;
	pktOutActions = [Action.Output PseudoPort.Flood] 
      } in 
      OxPlatform.packetOut xid sw pktOut
end

module Controller = Ox_Controller.Make(OpenFlow0x01_Platform)(Repeater)

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
