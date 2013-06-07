open OxPlatform
open OpenFlow0x01_Core
module Stats = OpenFlow0x01_Stats

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers
  
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw

  (* [FILL] copy over the packet_in function from Firewall.ml
     verbatim, including any helper functions. *)
  let firewall_packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    ()

  (* [FILL]: Match HTTP packets *)
  let is_http_packet (pk : Packet.packet) =
    false

  let num_http_packets = ref 0
   
  let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    Printf.printf "%s\n%!" (packetIn_to_string pktIn);
    firewall_packet_in sw xid pktIn;
    if is_http_packet (parse_payload pktIn.input_payload) then
      begin
        num_http_packets := !num_http_packets + 1;
        Printf.printf "Seen %d HTTP packets.\n%!" !num_http_packets
      end

end

module Controller = OxStart.Make (MyApplication)
