module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let switch_connected sw = ()

  let switch_disconnected sw = ()
    
  let barrier_reply sw xid = ()
    
  let stats_reply sw xid stats = ()
    
  let packet_in sw xid pktIn = 
    send_packet_out sw 0l
      { PacketOut.payload = pktIn.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = [Action.Output PseudoPort.AllPorts]
      }

  let port_status sw xid msg = ()

end

module Controller = Ox_Controller.Make (MyApplication)

let _ =
  Printf.printf "--- Welcome to Ox ---\n%!";
  Sys.catch_break true;
  try
    Lwt_main.run (Controller.start_controller ())
  with exn ->
    Printf.printf "[Ox] unexpected exception: %s\n%s\n%!"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ());
    exit 1
