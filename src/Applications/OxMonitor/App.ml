open Ox_Controller.OxPlatform
open OpenFlow0x01
open Packet

let send_stats_request sw = 
  Printf.printf "Callback\n%!";
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
  send_stats_request sw

let switchDisconnected sw = ()

let barrierReply xid = ()

let statsReply xid sw stats = 
  Printf.printf "Stats Reply\n%!";
  let open StatsReply in
      let open AggregateFlowStats in 
          match stats with 
            | AggregateFlowRep afs -> 
              Printf.printf "Packets: %Ld\nBytes: %Ld\n; Flows: %d\n%!"
                afs.packet_count afs.byte_count afs.flow_count;
        (* JNF: callback at time 5 *)
              send_stats_request sw
            | _ -> 
              ()

let packetIn xid sw pktIn = match pktIn.PacketIn.buffer_id with
  | None ->
    ()
  | Some bufId ->
    let open PacketIn in
        let open PacketOut in
            let pktOut = {
              buf_or_bytes = Payload.Buffer bufId;
              port_id = Some pktIn.port;
              actions = [Action.Output PseudoPort.Flood]
            } in
            packetOut xid sw pktOut

let portStatus xid sw msg = ()
