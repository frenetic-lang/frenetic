(* Extend OxTutorial2 to implement the packet_in function efficiently on
   a switch. *)
module MyApplication : Ox_Controller.OXMODULE = struct
  open Ox_Controller.OxPlatform
  open OpenFlow0x01

  (* [FILL IN HERE]: write a match rule to match ICMP traffic. *)
  let match_icmp = 
    let open Match in
    { dlSrc = None; dlDst = None; dlTyp = Some 0x800; dlVlan = None;
      dlVlanPcp = None; nwSrc = None; nwDst = None; nwProto = Some 1;
      nwTos = None; tpSrc = None; tpDst = None; inPort = None }

  (* [FILL IN HERE] Use match_icmp to configure the flow table. *)
  let switch_connected (sw : switchId) : unit =
    Printf.printf "Switch %Ld connected.\n%!" sw;
    send_flow_mod sw 0l
      (FlowMod.add_flow 200 match_icmp []);
    send_flow_mod sw 1l
      (FlowMod.add_flow 199 Match.all [Action.Output PseudoPort.AllPorts])
      
  let switch_disconnected (sw : switchId) : unit =
    Printf.printf "Switch %Ld disconnected.\n%!" sw

  (* [FILL IN HERE]: Use exactly the same packet_in function you wrote from
     OxTutorial2.  *)
  let packet_in (sw : switchId) (xid : xid) (pktIn : PacketIn.t) : unit =
    let payload = pktIn.PacketIn.payload in
    let pk = Payload.parse payload in
    if Packet.dlTyp pk = 0x806 && Packet.nwProto pk = 1 then
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
