module Switch = OpenFlow0x04_Switch
module TxSwitch = OpenFlow0x04_TxSwitch
module AL = SDN_Types
module Core = OpenFlow0x04_Core
module Msg = OpenFlow0x04.Message
module Fields = AL.FieldMap

open SDN_OpenFlow0x04

type t = {
  switch : Switch.t;
  tx_switch : TxSwitch.t;
  packet_ins : AL.pktIn Lwt_stream.t;
  group_table : GroupTable0x04.t;
}

let features (sw : t) : AL.switchFeatures =
  let feats = Switch.features sw.switch in
  let ports = Switch.ports sw.switch in
  let open OpenFlow0x04.SwitchFeatures in
  let from_portDesc desc =
    VInt.Int32 desc.Core.port_no in
  { AL.switch_id = feats.datapath_id;
    AL.switch_ports = List.map from_portDesc ports }

let from_handle (switch : Switch.t) : t =
  let tx_switch = TxSwitch.from_switch switch in
  let group_table = GroupTable0x04.create () in
  let (packet_ins, send_pktIn) = Lwt_stream.create () in
  let switch_thread () =
    let recv_msg msg = match msg with
      | Msg.PacketInMsg pktIn -> send_pktIn (Some (to_packetIn pktIn))
      | msg -> (* TODO(arjun): log ignored messages *) () in
    Lwt_stream.iter recv_msg (TxSwitch.recv_stream tx_switch) in
  Lwt.async (fun () -> 
    Lwt.pick [ switch_thread (); 
               Switch.wait_disconnect switch ]);
  { switch; tx_switch; packet_ins; group_table }
    
let disconnect (t : t) : unit Lwt.t = 
  Switch.disconnect t.switch

let setup_flow_table (sw : t) (tbl : AL.flowTable) : unit Lwt.t =
  let tbl = fix_vlan_in_table tbl in
  let priority = ref 65535 in
  let mk_flow_mod (flow : AL.flow) =
    let flow_mod = from_flow sw.group_table !priority flow in
    decr priority; (* TODO(arjun): range check *)
    Msg.FlowModMsg flow_mod in
  GroupTable0x04.clear_groups sw.group_table;
  let flow_mods = List.map mk_flow_mod tbl in
  let group_mods = GroupTable0x04.commit sw.group_table in
  TxSwitch.send sw.tx_switch (Msg.FlowModMsg Core.delete_all_flows) >>
  Lwt_list.iter_s (TxSwitch.send sw.tx_switch) group_mods >>
  Lwt_list.iter_s (TxSwitch.send sw.tx_switch) flow_mods
      
let packet_in (sw : t) =
  sw.packet_ins
    
let packet_out (sw : t) (pay : AL.payload) (act : AL.par) : unit Lwt.t =
  lwt pay = Lwt.wrap1 from_payload pay in
  lwt actions = Lwt.wrap1 (Common.flatten_par None) act in
  let pktOut = {
    Core.po_payload = pay;
    (* I believe port_id affects the semantics of action of the (Output InPort)
       action. Since the abstract actions cannot state InPort, by applying 
       [from_action None], we never generate an InPort action. *)
    Core.po_in_port = Core.Controller 0;
    Core.po_actions = actions } in
  TxSwitch.send sw.tx_switch (Msg.PacketOutMsg pktOut)
    
let flow_stats_request (sw : t) (pat : AL.pattern) : AL.flowStats list Lwt.t =
  raise_lwt (Failure "flow_stats_request NYI")

