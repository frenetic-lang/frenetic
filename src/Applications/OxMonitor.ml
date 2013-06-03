module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let send_stats_request sw = 
    let open StatsRequest.AggregateFlowRequest in  
        callback 5.0 
          (fun () -> 
            Printf.printf "Sending stats request to %Ld\n%!" sw; 
            statsRequest 0l sw 
              (StatsRequest.AggregateFlowReq { 
                of_match = Match.all;
                table_id = 0xff;
                port = None }))

  let switchConnected sw = 
    let open FlowMod in
        let fm = {
          mod_cmd = Command.AddFlow;
          match_ = Match.all;
          priority = 1;
          actions = [Action.Output PseudoPort.Flood];
          cookie = Int64.zero;
          idle_timeout = Timeout.Permanent;
          hard_timeout = Timeout.Permanent;
          notify_when_removed = false;
          apply_to_packet = None;
          out_port = None;
          check_overlap = false } in 
        flowMod (Int32.one) sw fm;
        barrierRequest (Int32.succ Int32.one) sw;
        send_stats_request sw

  let switchDisconnected sw = ()

  let barrierReply xid = ()

  let statsReply xid sw stats = 
    Printf.printf "Stats Reply\n%!";
    let open StatsReply in
        let open AggregateFlowStats in 
            match stats with 
              | AggregateFlowRep afs -> 
                Printf.printf "Packets: %Ld\nBytes: %Ld\nFlows: %ld\n%!"
                  afs.packet_count afs.byte_count afs.flow_count;
                send_stats_request sw
              | _ -> 
                ()

  let packetIn xid sw pktIn = ()

  let portStatus xid sw msg = ()


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
