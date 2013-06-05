open OpenFlow0x01_Core
open OpenFlow0x01
open Ox
open OxPlatform

module MyApplication : OXMODULE = struct

  let my_send_stats_request sw = 
    let open StatsRequest.AggregateFlowRequest in  
        timeout 5.0  (* consider milliseconds to avoid writing 1.0 *)
          (fun () -> 
            Printf.printf "Sending stats request to %Ld\n%!" sw; 
            send_stats_request sw 0l
              (StatsRequest.AggregateFlowReq { 
                of_match = Match.all;
                table_id = 0xff;
                port = None }))

  let switch_connected sw = 
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
        send_flow_mod sw 1l fm;
        send_barrier_request sw 2l;
        my_send_stats_request sw

  let switch_disconnected sw = ()

  let barrier_reply sw xid = ()

  let stats_reply sw xid stats = 
    Printf.printf "Stats Reply\n%!";
    let open StatsReply in
        let open AggregateFlowStats in 
            match stats with 
              | AggregateFlowRep afs -> 
                Printf.printf "Packets: %Ld\nBytes: %Ld\nFlows: %ld\n%!"
                  afs.packet_count afs.byte_count afs.flow_count;
                my_send_stats_request sw
              | _ -> 
                ()

  let packet_in sw xid pktIn = ()

  let port_status sw xid msg = ()


end

module Controller = Make (MyApplication)
