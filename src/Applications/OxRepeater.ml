open OpenFlow0x01_Core
open Ox
open OxPlatform

module MyApplication : OXMODULE = struct

  include OxDefaults

  let switch_connected sw = ()

  let switch_disconnected sw = ()
   
  let packet_in sw xid pktIn = 
    send_packet_out sw 0l
      { PacketOut.payload = pktIn.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = [Action.Output PseudoPort.AllPorts]
      }

end

module Controller = Make (MyApplication)
