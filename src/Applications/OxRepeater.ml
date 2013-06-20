open OxStart
open OxPlatform
open OpenFlow0x01_Core

module MyApplication : OXMODULE = struct
    
  include DefaultTutorialHandlers
    
  let packet_in sw xid pktIn = 
    send_packet_out sw 0l
      { output_payload = pktIn.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts]
      }
end

module Controller = Make (MyApplication)
