(* Write a packet_in function that drops ICMP traffic and acts as a repeater
   on all other traffic. Do not try to add rules to the flow table until the
   function works correctly. *)
module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw
      
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    Printf.printf "Received a PacketIn message from switch %Ld:\n%s\n%!"
      sw (PacketIn.to_string pktIn);
    (* FILL: Send the packet out of all ports, but block ICMP *)
    let payload = pktIn.PacketIn.payload in
    let pk = Payload.parse payload in
    if (Packet.dlTyp pk) = 0x806 then
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
    Printf.printf "Received a barrier reply %ld.\n%!" xid

  let stats_reply (sw : switchId) (xid : xid) (stats : StatsReply.t) : unit =
    Printf.printf "Received a StatsReply from switch %Ld:\n%s\n%!"
      sw (StatsReply.to_string stats)

  let port_status (sw : switchId) (xid : xid) (port : PortStatus.t) : unit =
    Printf.printf "Received a PortStatus from switch %Ld:\n%s\n%!"
      sw (PortStatus.to_string port)

end

module Controller = Ox_Controller.Make (MyApplication)
