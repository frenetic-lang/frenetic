open Lwt
open Unix
open Ox_Controller

module Log = Frenetic_Log

module Make (OxPlatform:OXPLATFORM) = 
struct
  let switchConnected sw = 
    Log.printf "Main" "SwitchConnected"; 
    Lwt.return ()

  let switchDisconnected sw = 
    Log.printf "Main" "SwitchDisconnected"; 
    Lwt.return ()

  let packetIn sw pktIn = 
    Log.printf "Main" "PacketIn"; 
    Lwt.return ()

  let statsReply sw stats =
    Log.printf "Main" "StatsReply";
    Lwt.return ()
end

module OxPlatform = MakeOxPlatform(OpenFlow0x01_Platform)

module Handlers = Make(OxPlatform)

module Controller = Ox_Controller.Make(OpenFlow0x01_Platform)(Handlers)

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
