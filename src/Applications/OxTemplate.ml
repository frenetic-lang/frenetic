open OpenFlow0x01
open OpenFlow0x01_Core
open Ox
open OxPlatform

module MyApplication : OXMODULE = struct

  let switch_connected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "[Main] Switch %Ld disconnected.\n%!" sw
      
  let packet_in (sw : switchId) (xid : xid) (pk : PacketIn.t) : unit =
    send_packet_out sw 0l
      { PacketOut.payload = pk.input_payload;
        PacketOut.port_id = None;
        PacketOut.actions = []
      }

  let barrier_reply (sw : switchId) (xid : xid) : unit =
    ()

  let stats_reply (sw : switchId) (xid : xid) (stats : StatsReply.t) : unit =
    ()

  let port_status (sw : switchId) (xid : xid) (port : PortStatus.t) : unit =
    ()

end

module Controller = Make (MyApplication)
