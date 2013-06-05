open OpenFlow0x01_Core
open Ox
open OxPlatform

module MyApplication : OXMODULE = struct

  include OxDefaults

  let switch_connected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld disconnected.\n%!" sw
      
  let packet_in (sw : switchId) (xid : xid) (pk : PacketIn.t) : unit =
    send_packet_out sw 0l
      { PacketOut.payload = pk.PacketIn.payload;
        PacketOut.port_id = None;
        PacketOut.actions = []
      }
end

module Controller = Make (MyApplication)
