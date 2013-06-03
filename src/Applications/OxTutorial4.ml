(* Write a packet_in function that:
  
   1. Blocks all ICMP traffic
   2. Counts the number of HTTP packets (requests and responses to port 80)
   3. Repeats all non-ICMP traffic

   *Do not* write a flow table.
 *)
module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let switch_connected (sw : switchId) : unit = 
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let num_http_packets = ref 0

  (* [FILL IN HERE]: write this predicate *)
  let is_http_packet (pk : Packet.packet) : bool = 
    pk.Packet.dlTyp = 0x800 && (* TODO(arjun): GAAAAAAAAAAAH! *)
    Packet.nwProto pk = 6 &&
    (Packet.tpSrc pk = 80 || Packet.tpDst pk = 80)

  (* [FILL IN HERE] You can use the packet_in function from OxTutorial2.
     However, 
     00:00:00:00:00:02 in the network and use policy from
     OxTutorial1 *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    if is_http_packet (Payload.parse pktIn.PacketIn.payload) then
      begin
        num_http_packets := !num_http_packets + 1;
        Printf.printf "Seen %d HTTP packets.\n%!" !num_http_packets
      end;
    (* [FILL IN HERE] Use the packet_in function from OxTutorial2 here. *)
    let payload = pktIn.PacketIn.payload in
    let pk = Payload.parse payload in
    (* TODO(arjun): AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGGGGGH! *)
    if pk.Packet.dlTyp = 0x800 && Packet.nwProto pk = 1 then
      send_packet_out sw 0l
        { PacketOut.payload = payload;
          PacketOut.port_id = None;
          PacketOut.actions = []
        }
    else 
      send_packet_out sw 0l
        { PacketOut.payload = payload;
          PacketOut.port_id = None;
          PacketOut.actions = [Action.Output PseudoPort.AllPorts]
        }

  let barrier_reply (sw : switchId) (xid : xid) : unit =
    ()

  let stats_reply (sw : switchId) (xid : xid) (stats : StatsReply.t) : unit =
    ()

  let port_status (sw : switchId) (xid : xid) (port : PortStatus.t) : unit =
    ()

end

module Controller = Ox_Controller.Make (MyApplication)
