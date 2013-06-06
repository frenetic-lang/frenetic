open OpenFlow0x01_Core
open OxStart
open OxPlatform

module MyApplication : OXMODULE = struct

  include OxDefaults

  let switch_connected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld disconnected.\n%!" sw
      
  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    send_packet_out sw 0l
      { output_payload = pk.input_payload;
        port_id = None;
        apply_actions = []
      }

end

module Controller = Make (MyApplication)
