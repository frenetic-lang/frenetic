module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let switch_connected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld disconnected.\n%!" sw
      
  let packet_in (sw : switchId) (xid : xid) (pk : PacketIn.t) : unit =
    send_packet_out sw 0l
      { PacketOut.payload = pk.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = []
      }

  let barrier_reply (sw : switchId) (xid : xid) : unit =
    ()

  let stats_reply (sw : switchId) (xid : xid) (stats : StatsReply.t) : unit =
    ()

  let port_status (sw : switchId) (xid : xid) (port : PortStatus.t) : unit =
    ()

end

module Controller = Ox_Controller.Make (MyApplication)

let _ =
  Printf.printf "[Main] Controller launching...\n%!";
  Sys.catch_break true;
  try
    Lwt_main.run (Controller.start_controller ())
  with exn ->
    Printf.printf "[Main] Unexpected exception: %s\n%s\n%!"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ());
    exit 1
