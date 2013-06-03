module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01
  type xid = Message.xid

  let switch_connected (sw : switchId) : unit = 
    Printf.printf "Switch %Ld connected.\n%!" sw
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  let monitored_host_mac = 0x000000000002L

  let num_packets = ref 0

  (* FILL: write this predicate *)
  let is_monitored_packet (pk : Packet.packet) : bool = 
    pk.Packet.dlSrc = monitored_host_mac ||
    pk.Packet.dlDst = monitored_host_mac

  (* FILL count the number of packets sent to and from host
     00:00:00:00:00:02 in the network and use policy from
     OxTutorial1 *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    Printf.printf "Received a PacketIn message from switch %Ld:\n%s\n%!"
      sw (PacketIn.to_string pktIn);
    (* TODO(arjun): might give them this and just have them write the
       is_http predicate. *)
    (if is_monitored_packet (Payload.parse pktIn.PacketIn.payload) then
        (num_packets := !num_packets + 1;
         Printf.printf "Seen %d packets to host 2.\n%!" !num_packets));
    let payload = pktIn.PacketIn.payload in
    let pk = Payload.parse payload in
    if pk.Packet.dlTyp = 0x806 then
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

let _ =
  Printf.printf "--- Welcome to Ox ---\n%!";
  Sys.catch_break true;
  try
    Lwt_main.run (Controller.start_controller ())
  with exn ->
    Printf.printf "[Ox] unexpected exception: %s\n%s\n%!"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ());
    exit 1
