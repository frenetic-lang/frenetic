open OxStart
open OxPlatform
open OpenFlow0x01_Core

module MyApplication : OXMODULE = struct
    
  include DefaultTutorialHandlers
    
  let switch_connected sw = ()

  let switch_disconnected sw = ()
    
  let barrier_reply sw xid = ()
    
  let stats_reply sw xid stats = ()
    
  let packet_in sw xid pktIn = 
    send_packet_out sw 0l
      { output_payload = pktIn.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts]
      }

  let port_status sw xid msg = ()

end

module Controller = Make (MyApplication)
