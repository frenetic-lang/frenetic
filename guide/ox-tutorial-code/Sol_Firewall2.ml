open OxStart
open OpenFlow0x01_Core
open OxPlatform

module MyApplication = struct

  include OxDefaults

  let match_icmp = { match_all with
    dlTyp = Some 0x800;
    nwProto = Some 1
  }

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 0l (add_flow 200 match_icmp []);
    send_flow_mod sw 0l (add_flow 100 match_all [Output AllPorts])
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let is_icmp_packet (pk : Packet.packet) =
    Packet.dlTyp pk = 0x800 && Packet.nwProto pk = 1

  let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    let payload = pktIn.input_payload in
    let pk = parse_payload payload in
    if is_icmp_packet pk then
      send_packet_out sw 0l
        { output_payload = payload;
          port_id = None;
          apply_actions = []
        }
    else 
      send_packet_out sw 0l
        { output_payload = payload;
          port_id = None;
          apply_actions = [Output AllPorts]
        }

end

module Controller = Make (MyApplication)
