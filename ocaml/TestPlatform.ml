open Lwt
open OpenFlow0x01Types

module Lwt_channel = Misc.Lwt_channel

exception SwitchDisconnected of switchId

type status = Connecting | Connected | Disconnected

type switch = {
  mutable status : status;
  to_controller : (xid * message) Lwt_channel.t;
  to_switch : (xid * message) Lwt_channel.t;    
}

type state = {
  switches : (switchId, switch) Hashtbl.t;
  pending_switches : (switchId * switch) Lwt_channel.t
}

let current_state = ref {
  switches = Hashtbl.create 100;
  pending_switches = Lwt_channel.create ()
}

let tear_down () = 
  current_state := {
    switches = Hashtbl.create 100;
    pending_switches = Lwt_channel.create ()
  }

let connect_switch sw_id = 
  let sw = { status = Connecting; 
             to_controller = Lwt_channel.create (); 
             to_switch = Lwt_channel.create () } in
  if Hashtbl.mem !current_state.switches sw_id then
    Lwt.fail (Failure "already connected")
  else
    Lwt_channel.send (sw_id, sw) !current_state.pending_switches

let disconnect_switch sw_id = 
  let sw = Hashtbl.find !current_state.switches sw_id in
  sw.status <- Disconnected

let accept_switch () = 
  lwt (sw_id, sw) = Lwt_channel.recv !current_state.pending_switches in
sw.status <- Connected;
Hashtbl.add !current_state.switches sw_id sw;
return { 
  switch_id = sw_id;
  num_buffers = 100l;
  num_tables = 1;
      (* This is an amazing switch! *)
  supported_capabilities = 
    { flow_stats = true;
      table_stats = true;
      port_stats = true; 
      stp = true;
      ip_reasm = true;
      queue_stats = true; 
      arp_match_ip = true };
  supported_actions =
    { output = true;
      set_vlan_id = true; 
      set_vlan_pcp = true;
      strip_vlan = true; 
      set_dl_src = true;
      set_dl_dst = true;
      set_nw_src = true; 
      set_nw_dst = true; 
      set_nw_tos = true;
      set_tp_src = true; 
      set_tp_dst = true;
      enqueue = true;
      vendor = false } }

let exn_if_disconnected sw_id sw = 
  if sw.status = Disconnected then
    begin
      Hashtbl.remove !current_state.switches sw_id;
      Lwt.fail (SwitchDisconnected sw_id)
    end
  else 
    return ()
      
      
let send_to_switch sw_id xid msg = 
  let sw = Hashtbl.find !current_state.switches sw_id in
  exn_if_disconnected sw_id sw >>
    Lwt_channel.send (xid, msg) sw.to_switch

let recv_from_switch sw_id = 
  let sw = Hashtbl.find !current_state.switches sw_id in
  exn_if_disconnected sw_id sw >>
      (* The controller is receiving messages from the switch. *)
    Lwt_channel.recv sw.to_controller

let fail_if_disconnected sw_id sw = 
  if sw.status = Disconnected then
    begin
      Hashtbl.remove !current_state.switches sw_id;
      Lwt.fail (Failure ("the switch is sending after it disconnected ..."))
    end
  else
    return ()

let send_to_controller sw_id xid msg = 
  let sw = Hashtbl.find !current_state.switches sw_id in
  fail_if_disconnected sw_id sw >>
    Lwt_channel.send (xid, msg) sw.to_controller
    
let recv_from_controller sw_id = 
  let sw = Hashtbl.find !current_state.switches sw_id in
  exn_if_disconnected sw_id sw >>
    Lwt_channel.recv sw.to_switch
