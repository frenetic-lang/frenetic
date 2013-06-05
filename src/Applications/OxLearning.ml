open OpenFlow0x01
open OpenFlow0x01_Core
open Ox

module MyApplication : OXMODULE = struct
  open OxPlatform

  let table = ref []

  let switch_connected sw =
    table := (sw,Hashtbl.create 11)::!table

  let switch_disconnected sw =
    table := List.remove_assoc sw !table

  let barrier_reply sw xid = ()

  let stats_reply sw xid stats = ()

  let packet_in sw xid pktIn =
    let pkt = Payload.parse pktIn.input_payload in
    let inport = pktIn.port in 
    let src = pkt.Packet.dlSrc in 
    let dst = pkt.Packet.dlDst in 
    let sw_table = List.assoc sw !table in 
    Hashtbl.add sw_table src inport;
    if Hashtbl.mem sw_table dst then 
      let outport = Hashtbl.find sw_table dst in 
      let m = { Match.all with 
        dlSrc = Some src;
        dlDst = Some dst;
        inPort = Some inport } in 
      let fm = {
        command = AddFlow;
        pattern = m;
        priority = 1;
        actions = [Output (PhysicalPort outport)];
        cookie = Int64.zero;
        idle_timeout = Permanent;
        hard_timeout = Permanent;
        notify_when_removed = false;
        apply_to_packet = None; (* TODO(arjun): BUG!! *)
        out_port = None;
        check_overlap = false } in 
      send_flow_mod sw (Int32.succ xid) fm;
      send_barrier_request sw (Int32.succ (Int32.succ xid))
    else
      let open PacketOut in
          let pktOut = { 
            payload = pktIn.input_payload;
            port_id = Some pktIn.port;
            actions = [Output Flood] 
          } in 
          send_packet_out sw xid pktOut
            
  let port_status _ _ _ = ()

end

module Controller = Make (MyApplication)
