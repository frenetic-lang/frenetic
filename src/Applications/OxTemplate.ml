module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw
      
  let packet_in (sw : switchId) (xid : Message.xid) (pk : PacketIn.t) : unit =
    Printf.printf "Received a PacketIn message from switch %Ld:\n%s\n%!"
      sw (PacketIn.to_string pk);
    (* TODO(arjun): seriously ... common payload type between both!!!! *)
    match pk.PacketIn.buffer_id with
      | Some buf ->
        send_packet_out sw 0l ({ PacketOut.buf_or_bytes = PacketOut.Payload.Buffer buf;
                           PacketOut.port_id = None;
                           PacketOut.actions = [] })
      | None -> 
        send_packet_out sw 0l
          { PacketOut.buf_or_bytes = PacketOut.Payload.Packet pk.PacketIn.packet;
            PacketOut.port_id = None;
            PacketOut.actions = [] }

  let barrier_reply (sw : switchId) (xid : Message.xid) : unit =
    Printf.printf "Received a barrier reply %ld.\n%!" xid

  let stats_reply (sw : switchId) (xid : Message.xid) (stats : StatsReply.t) : unit =
    Printf.printf "Received a StatsReply from switch %Ld:\n%s\n%!"
      sw (StatsReply.to_string stats)

  let port_status (sw : switchId) (xid : Message.xid) (port_stat : PortStatus.t) : unit =
    Printf.printf "Received a PortStatus from switch %Ld:\n%s\n%!"
      sw (PortStatus.to_string port_stat)

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
