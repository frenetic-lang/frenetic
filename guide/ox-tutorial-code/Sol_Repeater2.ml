open OxPlatform
open OpenFlow0x01_Core

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers
  
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 0l (add_flow 10 match_all [Output AllPorts])

  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "packet_in: %s\n%!" (OpenFlow0x01.PacketIn.to_string pk);
    send_packet_out sw 0l
      { output_payload = pk.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts]
      }

end

module Controller = OxStart.Make (MyApplication)
