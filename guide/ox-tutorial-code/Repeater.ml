open OxPlatform
open OpenFlow0x01_Core

module MyApplication = struct

  include OxStart.OxDefaults

  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  (* [FILL] This packet_in function sends all packets out of port 1.
     Modify it to behave like a repeater: send the packet out of all
     ports, except its input port. *)
  let packet_in (sw : switchId) (xid : xid) (pk : packetIn) : unit =
    send_packet_out sw 0l
      { output_payload = pk.input_payload;
        port_id = None;
        apply_actions = [Output (PhysicalPort 1)]
      }

end

module Controller = OxStart.Make (MyApplication)
