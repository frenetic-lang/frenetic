open OxStart
open OpenFlow0x01_Core
open OxPlatform

(* Student has seen repeater that emits packet-out messages. Now, emit
   a flow table to implement a repeater efficiently. *)
module MyApplication : OXMODULE = struct

  include DefaultTutorialHandlers

  (* [FILL HERE] When a switch connects, add a rule to its flow table
     to make it behave as a repeater. *)
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 1l (add_flow 200 match_all [Output AllPorts])

  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    send_packet_out sw 0l
      { output_payload = pk.input_payload;
        port_id = None;
        apply_actions = [Output AllPorts]
      }

end

module Controller = Make (MyApplication)
