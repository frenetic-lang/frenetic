open OxPlatform
open OpenFlow0x01_Core

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers
  
  let switch_connected (sw : switchId) _ : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 0l (add_flow 10 match_all [Output AllPorts])

  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "%s\n%!" (packetIn_to_string pk);
    send_packet_out sw 0l
      { output_payload = pk.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts]
      }

end

module Controller = OxStart.Make (MyApplication)
