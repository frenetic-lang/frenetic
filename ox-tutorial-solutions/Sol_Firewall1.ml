open OpenFlow0x01_Core
open OxPlatform

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers

  let switch_connected (sw : switchId) _ : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let is_icmp_packet (pk : Packet.packet) =
    Packet.dlTyp pk = 0x800 && Packet.nwProto pk = 1

  let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    Printf.printf "%s\n%!" (packetIn_to_string pktIn);
    let pk = parse_payload pktIn.input_payload in
    if is_icmp_packet pk then
      send_packet_out sw 0l
        { output_payload = pktIn.input_payload;
          port_id = None;
          apply_actions = []
        }
    else 
      send_packet_out sw 0l
        { output_payload = pktIn.input_payload;
          port_id = None;
          apply_actions = [Output AllPorts]
        }

end

module Controller = OxStart.Make (MyApplication)
