(* *)
open Lwt
open Unix
open Ox_Controller
open OpenFlow0x01
open Packet

module Log = Frenetic_Log

module Repeater (OxPlatform:OXPLATFORM) =
struct
  let switchConnected sw = ()

  let switchDisconnected sw = ()

  let barrierReply xid = ()

  let statsReply xid sw stats = ()

  let packetIn xid sw pktIn = match pktIn.packetInBufferId with
    | None ->
      ()
    | Some bufId ->
      let pktOut = {
        pktOutBufOrBytes = Buffer bufId;
        pktOutPortId = Some pktIn.packetInPort;
        pktOutActions = [Action.Output PseudoPort.Flood]
      } in
      OxPlatform.packetOut xid sw pktOut
end

module Monitor (OxPlatform:OXPLATFORM) =
struct
  let send_stats_request sw = 
    Printf.printf "Callback\n%!";
    let open AggregateFlowRequest in  
	OxPlatform.callback 5.0 
	  (fun () -> 
	    Printf.printf "Sending stats request to %Ld\n%!" sw; 
	    OxPlatform.statsRequest 0l sw 
	      (AggregateFlowReq { 
		of_match = Match.all;
		table_id = 0xff;
		port = None }))

  let switchConnected sw = 
    send_stats_request sw

  let switchDisconnected sw = ()

  let barrierReply xid = ()

  let statsReply xid sw stats = 
    Printf.printf "Stats Reply\n%!";
    let open AggregateFlowStats in 
    match stats with 
      | AggregateFlowRep afs -> 
	Printf.printf "Packets: %d\nBytes: %d\n; Flows: %d\n%!"
	  afs.packet_count afs.byte_count afs.flow_count;
	(* JNF: callback at time 5 *)
	send_stats_request sw
      | _ -> 
	()

  let packetIn xid sw pktIn = match pktIn.packetInBufferId with
    | None ->
      ()
    | Some bufId ->
      let pktOut = {
        pktOutBufOrBytes = Buffer bufId;
        pktOutPortId = Some pktIn.packetInPort;
        pktOutActions = [Action.Output PseudoPort.Flood]
      } in
      OxPlatform.packetOut xid sw pktOut
end

module Learning (OxPlatform:OXPLATFORM) =
struct
  let table = ref []

  let switchConnected sw =
    table := (sw,Hashtbl.create 11)::!table

  let switchDisconnected sw =
    table := List.remove_assoc sw !table

  let barrierReply xid = ()

  let statsReply xid sw stats = ()

  let packetIn xid sw pktIn = 
    match pktIn.packetInBufferId, Packet.parse pktIn.packetInPacket with
      | Some bufId, Some pkt -> 
	let inport = pktIn.packetInPort in 
	let src = pkt.dlSrc in 
	let dst = pkt.dlDst in 
	let sw_table = List.assoc sw !table in 
	Hashtbl.add sw_table src inport;
	if Hashtbl.mem sw_table dst then 
	  let outport = Hashtbl.find sw_table dst in 
	  let m = { Match.all with 
	            Match.dlSrc = Some src;
		    Match.dlDst = Some dst;
	            Match.inPort = Some inport } in 
	  let fm = {
	    mfModCmd = AddFlow;
	    mfMatch = m;
	    mfPriority = 1;
	    mfActions = [Action.Output (PseudoPort.PhysicalPort outport)];
	    mfCookie = Int64.zero;
	    mfIdleTimeOut = Permanent;
	    mfHardTimeOut = Permanent;
	    mfNotifyWhenRemoved = false;
	    mfApplyToPacket = None;
	    mfOutPort = None;
	    mfCheckOverlap = false } in 
	  OxPlatform.flowMod (Int32.succ xid) sw fm;
	  OxPlatform.barrierRequest (Int32.succ (Int32.succ xid)) sw
	else
	  let pktOut = { 
	    pktOutBufOrBytes = Buffer bufId;
	    pktOutPortId = Some pktIn.packetInPort;
	    pktOutActions = [Action.Output PseudoPort.Flood] 
	  } in 
	  OxPlatform.packetOut xid sw pktOut
      | _ -> 
	()	  
end

module Controller = Ox_Controller.Make(OpenFlow0x01_Platform)(Monitor)

let main () =
  OpenFlow0x01_Platform.init_with_port 6633 >>
  Controller.start_controller ()

let _ =
  Log.printf "Ox" "--- Welcome to Ox ---\n%!";
  Sys.catch_break true;
  try
    Lwt_main.run (main ())
  with exn ->
    Log.printf "Ox" "unexpected exception: %s\n%s\n%!"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ());
    exit 1
