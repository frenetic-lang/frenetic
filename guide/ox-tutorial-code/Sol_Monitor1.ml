open OxPlatform
open OpenFlow0x01_Core

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers
  
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw

  (* [FILL] copy over the packet_in function from Firewall.ml
     verbatim, including any helper functions. *)
  let is_icmp_packet (pk : Packet.packet) =
    Packet.dlTyp pk = 0x800 && Packet.nwProto pk = 1

  let firewall_packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
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

  (* [FILL]: Match HTTP packets *)
  let is_http_packet (pk : Packet.packet) =
    Packet.dlTyp pk = 0x800 &&
    Packet.nwProto pk = 6 &&
    (Packet.tpSrc pk = 80 || Packet.tpDst pk = 80)

  let num_http_packets = ref 0
   
  let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    Printf.printf "packet_in: %s\n%!" (OpenFlow0x01.PacketIn.to_string pktIn);
    firewall_packet_in sw xid pktIn;
    if is_http_packet (parse_payload pktIn.input_payload) then
      begin
        num_http_packets := !num_http_packets + 1;
        Printf.printf "Seen %d HTTP packets.\n%!" !num_http_packets
      end

end

module Controller = OxStart.Make (MyApplication)
