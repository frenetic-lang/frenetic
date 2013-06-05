(* Write a packet_in function that drops ICMP traffic and acts as a repeater
   on all other traffic. Do not try to add rules to the flow table until the
   function works correctly. *)
module MyApplication : Ox.OXMODULE = struct
  open Ox.OxPlatform
  open OpenFlow0x01

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  (* [FILL IN HERE]: Send the packet out of all ports, but block ICMP *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    let payload = pktIn.PacketIn.payload in
    let pk = Payload.parse payload in
    if Packet.dlTyp pk = 0x800 && Packet.nwProto pk = 1 then
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

module Controller = Ox.Make (MyApplication)
