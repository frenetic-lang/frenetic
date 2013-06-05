open OpenFlow0x01_Core

module MyApplication : Ox.OXMODULE = struct
  open Ox.OxPlatform
  open OpenFlow0x01

  let switch_connected sw = ()

  let switch_disconnected sw = ()
    
  let barrier_reply sw xid = ()
    
  let stats_reply sw xid stats = ()
    
  let packet_in sw xid pktIn = 
    send_packet_out sw 0l
      { PacketOut.payload = pktIn.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = [Output AllPorts]
      }

  let port_status sw xid msg = ()

end

module Controller = Ox.Make (MyApplication)
