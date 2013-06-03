module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let table = ref []

  let switch_connected sw =
    table := (sw,Hashtbl.create 11)::!table

  let switch_disconnected sw =
    table := List.remove_assoc sw !table

  let barrier_reply sw xid = ()

  let stats_reply sw xid stats = ()

  let packet_in sw xid pktIn = 
    let pkt = Packet.parse pktIn.PacketIn.packet in
    match pktIn.PacketIn.buffer_id with
      | Some bufId -> 
        let inport = pktIn.PacketIn.port in 
        let src = pkt.Packet.dlSrc in 
        let dst = pkt.Packet.dlDst in 
        let sw_table = List.assoc sw !table in 
        Hashtbl.add sw_table src inport;
        let open FlowMod in
            if Hashtbl.mem sw_table dst then 
              let outport = Hashtbl.find sw_table dst in 
              let m = { Match.all with 
                Match.dlSrc = Some src;
                Match.dlDst = Some dst;
                Match.inPort = Some inport } in 
              let fm = {
                mod_cmd = Command.AddFlow;
                match_ = m;
                priority = 1;
                actions = [Action.Output (PseudoPort.PhysicalPort outport)];
                cookie = Int64.zero;
                idle_timeout = Timeout.Permanent;
                hard_timeout = Timeout.Permanent;
                notify_when_removed = false;
                apply_to_packet = None;
                out_port = None;
                check_overlap = false } in 
              send_flow_mod sw (Int32.succ xid) fm;
              send_barrier_request sw (Int32.succ (Int32.succ xid))
            else
              let open PacketOut in
                  let pktOut = { 
                    buf_or_bytes = Payload.Buffer bufId;
                    port_id = Some pktIn.PacketIn.port;
                    actions = [Action.Output PseudoPort.Flood] 
                  } in 
                  send_packet_out sw xid pktOut
      | _ -> 
        ()          

  let port_status _ _ _ = ()

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
