open Ox
open OpenFlow0x01_Core
open OxPlatform
module Stats = OpenFlow0x01_Stats

module MyApplication : OXMODULE = struct
  

  let my_send_stats_request sw = 
    timeout 5.0  (* TODO(arjun):consider milliseconds to avoid writing 1.0 *)
      (fun () -> 
        Printf.printf "Sending stats request to %Ld\n%!" sw; 
        send_stats_request sw 0l
          (Stats.AggregateRequest (match_all, 0xff, None)))

  let switch_connected sw = 
    let fm = {
      command = AddFlow;
      pattern = match_all;
      priority = 1;
      actions = [Output Flood];
      cookie = Int64.zero;
      idle_timeout = Permanent;
      hard_timeout = Permanent;
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
    match stats with 
      | Stats.AggregateFlowRep afs -> 
        Printf.printf "Packets: %Ld\nBytes: %Ld\nFlows: %ld\n%!"
          afs.Stats.total_packet_count afs.Stats.total_byte_count 
          afs.Stats.flow_count;
        my_send_stats_request sw
      | _ -> 
        ()

  let packet_in sw xid pktIn = ()

  let port_status sw xid msg = ()


end

module Controller = Make (MyApplication)
