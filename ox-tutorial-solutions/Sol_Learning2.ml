open OxPlatform
open Packet
open OpenFlow0x01_Core

module MyApplication = struct

  include OxStart.DefaultTutorialHandlers

  let known_hosts : (dlAddr, portId) Hashtbl.t = Hashtbl.create 50 (* initial capacity *)

  (* [FILL] Store the location (port) of each host in the
     known_hosts hashtable. Use the code in the tutorial as a guide. *)
  let learning_packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    let pk = parse_payload pktIn.input_payload in
    Hashtbl.add known_hosts pk.Packet.dlSrc pktIn.port

  (* [FILL] Route packets to known hosts out the correct port,
     otherwise flood them. *)
  let routing_packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    let pk = parse_payload pktIn.input_payload in
    let pkt_dst = pk.Packet.dlDst in
    let pkt_src = pk.Packet.dlSrc in
    try 
      let out_port = Hashtbl.find known_hosts pkt_dst in
      let src_port = pktIn.port in
      let src_dst_match = {match_all with dlDst = Some pkt_dst; dlSrc = Some pkt_src} in
      let dst_src_match = {match_all with dlDst = Some pkt_src; dlSrc = Some pkt_dst} in
      Printf.printf "Installing rule for host %Ld to %Ld.\n" pkt_src pkt_dst;
      send_flow_mod sw 0l (add_flow 10 src_dst_match [Output (PhysicalPort out_port)]);
      Printf.printf "Installing rule for host %Ld to %Ld.\n" pkt_dst pkt_src;
      send_flow_mod sw 0l (add_flow 10 dst_src_match [Output (PhysicalPort src_port)]);
      send_packet_out sw 0l {
        output_payload = pktIn.input_payload;
        port_id = None;
        apply_actions = [Output (PhysicalPort out_port)]
      } 
    with Not_found ->
      (Printf.printf "Flooding to %Ld.\n" pkt_dst;
       send_packet_out sw 0l {
         output_payload = pktIn.input_payload;
         port_id = None;
         apply_actions = [Output AllPorts]
       })

  (* [FILL] Modify this packet_in function to run both
     learning_packet_in and routing_packet_in. *)
  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    Printf.printf "%s\n%!" (packetIn_to_string pk);
    learning_packet_in sw xid pk;
    routing_packet_in sw xid pk

end

module Controller = OxStart.Make (MyApplication)
