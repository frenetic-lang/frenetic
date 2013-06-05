
open Ox
open OxPlatform
open OpenFlow0x01_Core

module MyApplication : OXMODULE = struct

  include OxDefaults

  let table = ref []

  let switch_connected sw =
    table := (sw,Hashtbl.create 11)::!table

  let switch_disconnected sw =
    table := List.remove_assoc sw !table

  let packet_in sw xid pktIn =
    let pkt = parse_payload pktIn.input_payload in
    let inport = pktIn.port in 
    let src = pkt.Packet.dlSrc in 
    let dst = pkt.Packet.dlDst in 
    let sw_table = List.assoc sw !table in 
    Hashtbl.add sw_table src inport;
    if Hashtbl.mem sw_table dst then 
      let outport = Hashtbl.find sw_table dst in 
      let m = { match_all with 
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
      let pktOut = { 
        output_payload = pktIn.input_payload;
        port_id = Some pktIn.port;
        apply_actions = [Output Flood] 
      } in 
      send_packet_out sw xid pktOut

end

module Controller = Make (MyApplication)
