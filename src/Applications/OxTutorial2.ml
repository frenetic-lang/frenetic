open Ox
open OpenFlow0x01_Core
open OxPlatform

(* Write a packet_in function that drops ICMP traffic and acts as a repeater
   on all other traffic. Do not try to add rules to the flow table until the
   function works correctly. *)
module MyApplication : OXMODULE = struct

  include OxDefaults

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  (* [FILL IN HERE]: Send the packet out of all ports, but block ICMP *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
    let payload = pktIn.input_payload in
    let pk = parse_payload payload in
    if Packet.dlTyp pk = 0x800 && Packet.nwProto pk = 1 then
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
