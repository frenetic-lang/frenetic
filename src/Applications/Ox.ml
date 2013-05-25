open Lwt
open Unix
open Frenetic_Log

module Controller = Ox_Controller.Make(OpenFlow0x01_Platform)

let handlers = 
  { Ox_Controller.handleSwitchConnected = 
      (fun sw -> 
        Log.printf "Main" "SwitchConnected"; 
        Lwt.return ());
    Ox_Controller.handleSwitchDisconnected = 
      (fun sw -> 
        Log.printf "Main" "SwitchDisconnected"; 
        Lwt.return ());
    Ox_Controller.handlePacketIn = 
      (fun pktIn -> 
        Log.printf "Main" "PacketIn"; 
        Lwt.return ()) }

let main () = 
  OpenFlow0x01_Platform.init_with_port 6633 >>
  Controller.start_controller handlers
      
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
