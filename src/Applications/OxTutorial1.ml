open Ox
open OxPlatform
open OpenFlow0x01

(* Student has seen repeater that emits packet-out messages. Now, emit
   a flow table to implement a repeater efficiently. *)
module MyApplication : OXMODULE = struct

  include OxDefaults

  (* [FILL HERE] When a switch connects, add a rule to its flow table
     to make it behave as a repeater. *)
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 1l
      (FlowMod.add_flow 200 Match.all [Action.Output PseudoPort.AllPorts])

  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let packet_in (sw : switchId) (xid : xid) (pk : PacketIn.t) : unit =
    send_packet_out sw 0l
      { PacketOut.payload = pk.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = [Action.Output PseudoPort.AllPorts]
      }

end

module Controller = Make (MyApplication)
