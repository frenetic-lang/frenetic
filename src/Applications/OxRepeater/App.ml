open Ox_Controller.OxPlatform
open OpenFlow0x01
open Packet

let switchConnected sw = ()

let switchDisconnected sw = ()
  
let barrierReply xid = ()
  
let statsReply xid sw stats = ()
  
let packetIn xid sw pktIn = match pktIn.PacketIn.buffer_id with
  | None ->
    ()
  | Some bufId ->
    let open PacketOut in
    let pktOut = {
      buf_or_bytes = Payload.Buffer bufId;
      port_id = Some pktIn.PacketIn.port;
      actions = [Action.Output PseudoPort.Flood]
    } in
    packetOut xid sw pktOut

let portStatus xid sw msg = ()
