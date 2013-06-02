open Ox_Controller.OxPlatform
open OpenFlow0x01
open Packet

let table = ref []

let switchConnected sw =
  table := (sw,Hashtbl.create 11)::!table

let switchDisconnected sw =
  table := List.remove_assoc sw !table

let barrierReply xid = ()

let statsReply xid sw stats = ()

let packetIn xid sw pktIn = 
  match pktIn.PacketIn.buffer_id, Packet.parse pktIn.PacketIn.packet with
    | Some bufId, Some pkt -> 
      let inport = pktIn.PacketIn.port in 
      let src = pkt.dlSrc in 
      let dst = pkt.dlDst in 
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
            flowMod (Int32.succ xid) sw fm;
            barrierRequest (Int32.succ (Int32.succ xid)) sw
          else
            let open PacketOut in
                let pktOut = { 
                  buf_or_bytes = Payload.Buffer bufId;
                  port_id = Some pktIn.PacketIn.port;
                  actions = [Action.Output PseudoPort.Flood] 
                } in 
                packetOut xid sw pktOut
    | _ -> 
      ()          

let portStatus _ _ _ = ()
