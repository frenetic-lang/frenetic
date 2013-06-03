module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let switch_connected sw = ()

  let switch_disconnected sw = ()
    
  let barrier_reply sw xid = ()
    
  let stats_reply sw xid stats = ()
    
  let packet_in sw xid pktIn = match pktIn.PacketIn.buffer_id with
    | None ->
      () (* TODO(arjun): cannot count on this! *)
    | Some bufId ->
      let open PacketOut in
          let pktOut = {
            buf_or_bytes = Payload.Buffer bufId;
            port_id = Some pktIn.PacketIn.port;
            actions = [Action.Output PseudoPort.Flood]
          } in
          send_packet_out sw xid pktOut

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
