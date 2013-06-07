open OpenFlow0x01_Core
open OxPlatform

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers

  let match_icmp = { match_all with
    dlTyp = Some 0x800;
    nwProto = Some 1
  }

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 0l (add_flow 200 match_icmp []);
    send_flow_mod sw 0l (add_flow 100 match_all [Output AllPorts])
      
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
