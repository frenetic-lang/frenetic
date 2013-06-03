module MyApplication : Ox_Controller.OXMODULE = struct
  open OpenFlow0x01

  let switchConnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switchConnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw
      
  (* TODO(arjun): switch before xid in all handlers. *)
  let packetIn (xid : Message.xid) (sw : switchId) (pk : PacketIn.t) : unit =
    Printf.printf "Received a PacketIn message from switch %Ld:\n%s\n%!"
      sw (PacketIn.to_string pk);
    (* TODO(arjun): seriously ... common payload type between both!!!! *)
    match PacketIn.buffer_id with
      | Some buf ->
        packetOut 0L sw ({ PacketOut.buf_or_bytes = PacketOut.Payload.Buffer buf;
                           PacketOut.port_id = None;
                           PacketOut.actions = [] })
      | None -> 
        (packetOut 0L sw
          { PacketOut.buf_or_bytes = PacketOut.Payload.Bytes PacketIn.packet;
            PacketOut.port_id = None;
            PacketOut.actions = [] })

(* TODO(arjun): Where is the switch ID? *)
  let barrierReply (xid : Message.xid) : unit =
    Printf.printf "Received a barrier reply %Ld.\n%!" xid

  let statsReply (xid : Message.xid) (sw : switchId) (stats : StatsReply.t) : unit =
    Printf.printf "Received a StatsReply from switch %Ld:\n%s\n%!"
      sw (StatsReply.to_string stats)

  let portStatus (xid : Message.xid) (sw : switchId) (stat : PortStatus.t) : unit =
    Printf.printf "Received a PortStatus from switch %Ld:\n%s\n%!"
      sw (PortStatus.to_string stats)

end

module Controller = Ox_Controller.Make (App)

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
