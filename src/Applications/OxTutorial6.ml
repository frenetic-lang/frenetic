open OpenFlow0x01_Core

(* Extend OxTutorial4 to also implement its packet_in handler efficiently
   in the flow table. This finishes OxTutoria6. *)
module MyApplication : Ox.OXMODULE = struct
  open Ox.OxPlatform
  open OpenFlow0x01

  let match_icmp = 
    { dlSrc = None; dlDst = None; dlTyp = Some 0x800; dlVlan = None;
      dlVlanPcp = None; nwSrc = None; nwDst = None; nwProto = Some 1;
      nwTos = None; tpSrc = None; tpDst = None; inPort = None }

  let match_http_requests = 
    { dlSrc = None; dlDst = None; dlTyp = Some 0x800; dlVlan = None;
      dlVlanPcp = None; nwSrc = None; nwDst = None; nwProto = Some 6;
      nwTos = None; tpSrc = None; tpDst = Some 80; inPort = None }

  let match_http_responses = 
    { dlSrc = None; dlDst = None; dlTyp = Some 0x800; dlVlan = None;
      dlVlanPcp = None; nwSrc = None; nwDst = None; nwProto = Some 6;
      nwTos = None; tpSrc = Some 80; tpDst = None; inPort = None }

  (* TODO(arjun): I think we provide this function here, and explain what
     it does. Note it has been modified to take xid param. *)
  let rec periodic_stats_request sw interval xid pat =
    let callback () =
      Printf.printf "Sending stats request to %Ld\n%!" sw; 
      send_stats_request sw xid
        (StatsRequest.AggregateFlowReq {
          StatsRequest.AggregateFlowRequest.of_match = pat;
          StatsRequest.AggregateFlowRequest.table_id = 0xff;
          StatsRequest.AggregateFlowRequest.port = None });
      periodic_stats_request sw interval xid pat in
    timeout interval callback

  (* FILL: pick XIDs for each type of request. *)
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    periodic_stats_request sw 5.0 500l match_http_requests;
    periodic_stats_request sw 5.0 700l match_http_responses;   
    send_flow_mod sw 0l
      (FlowMod.add_flow 200 match_icmp []);
    send_flow_mod sw 1l
      (FlowMod.add_flow 199 match_http_requests 
         [Output AllPorts]);
    send_flow_mod sw 1l
      (FlowMod.add_flow 198 match_http_responses 
         [Output AllPorts]);
    send_flow_mod sw 1l
      (FlowMod.add_flow 197 Match.all [Output AllPorts])
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let num_http_request_packets = ref 0L
  let num_http_response_packets = ref 0L

  let is_http_request_packet (pk : Packet.packet) : bool = 
    Packet.dlTyp pk = 0x800 &&
    Packet.nwProto pk = 6 &&
    Packet.tpDst pk = 80

  let is_http_response_packet (pk : Packet.packet) : bool = 
    Packet.dlTyp pk = 0x800 &&
    Packet.nwProto pk = 6 &&
    Packet.tpSrc pk = 80

  (* TODO(arjun): I think we'll provide this. *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    let payload = pktIn.input_payload in
    let pk = Payload.parse payload in
    if is_http_request_packet pk then
      num_http_request_packets := Int64.add 1L !num_http_request_packets;
    if is_http_response_packet pk then
      num_http_response_packets := Int64.add 1L !num_http_response_packets;
    if Packet.dlTyp pk = 0x800 && Packet.nwProto pk = 1 then
      send_packet_out sw 0l
        { output_payload = payload;
          port_id = None;
          apply_actions = []
        }
    else 
      send_packet_out sw 0l
        { output_payload = payload;
          port_id = None;
          apply_actions = [Output AllPorts]
        }

  let barrier_reply (sw : switchId) (xid : xid) : unit =
    ()

  (* TODO(arjun): this has to become simpler, even if we give it to
     students. *)
  let stats_reply (sw : switchId) (xid : xid) (stats : StatsReply.t) : unit =
    match stats with
      | StatsReply.AggregateFlowRep rep ->
        let k = rep.StatsReply.AggregateFlowStats.packet_count in
        (match xid with
          | 500l -> 
            num_http_request_packets := Int64.add !num_http_request_packets k
          | 700l ->
            num_http_response_packets := Int64.add !num_http_response_packets k
          | _ -> Printf.printf "Unexpected xid (%ld) in StatsReply\n%!" xid);
        Printf.printf "Seen %Ld HTTP packets.\n%!"
          (Int64.add !num_http_request_packets !num_http_response_packets)
      | _ -> 
        Printf.printf "Unexpected stats message.\n%!"


  let port_status (sw : switchId) (xid : xid) (port : PortStatus.t) : unit =
    ()

end

module Controller = Ox.Make (MyApplication)
