module Switch = OpenFlow0x01_Switch
module TxSwitch = OpenFlow0x01_TxSwitch
module AL = SDN_Types
module Core = OpenFlow0x01_Core
module Msg = OpenFlow0x01.Message

open SDN_OpenFlow0x01
	    
type t =
    { switch : Switch.t;
      tx_switch : TxSwitch.t;
      packet_ins : AL.pktIn Lwt_stream.t }

let features (sw : t) : AL.switchFeatures =
  let feats = Switch.features sw.switch in
  let open OpenFlow0x01.SwitchFeatures in
  let from_portDesc desc =
    VInt.Int16 desc.OpenFlow0x01.PortDescription.port_no in
  { AL.switch_id = VInt.Int64 feats.switch_id;
    AL.switch_ports = List.map from_portDesc feats.ports }

let from_handle (switch : Switch.t) : t =
  let tx_switch = TxSwitch.from_switch switch in
  let (packet_ins, send_pktIn) = Lwt_stream.create () in
  let switch_thread () =
    let recv_msg msg = match msg with
      | Msg.PacketInMsg pktIn -> send_pktIn (Some (to_packetIn pktIn))
      | msg -> (* TODO(arjun): log ignored messages *) () in
    Lwt_stream.iter recv_msg (TxSwitch.recv_stream tx_switch) in
  Lwt.async (fun () -> 
    Lwt.pick [ switch_thread (); 
               Switch.wait_disconnect switch ]);
  { switch; 
    tx_switch; 
    packet_ins }
    
let disconnect (t : t) : unit Lwt.t = 
  Switch.disconnect t.switch
    
let setup_flow_table (sw : t) (tbl : AL.flowTable) : unit Lwt.t =
  let priority = ref 65535 in
  let send_flow_mod (flow : AL.flow) =
    lwt flow_mod = Lwt.wrap2 from_flow !priority flow in
    lwt _ = TxSwitch.send sw.tx_switch (Msg.FlowModMsg flow_mod) in
    decr priority; (* TODO(arjun): range check *)
    Lwt.return () in
  lwt _ = TxSwitch.send sw.tx_switch (Msg.FlowModMsg Core.delete_all_flows) in
  Lwt_list.iter_s send_flow_mod tbl
    
let packet_in (sw : t) =
  sw.packet_ins
    
let packet_out (sw : t) (pay : AL.payload) (act : AL.par) : unit Lwt.t =
  lwt pay = Lwt.wrap1 from_payload pay in
  lwt actions = Lwt.wrap1 (Common.flatten_par None) act in
  let pktOut = {
    Core.output_payload = pay;
    (* I believe port_id affects the semantics of action of the (Output InPort)
       action. Since the abstract actions cannot state InPort, by applying 
       [from_action None], we never generate an InPort action. *)
    Core.port_id = None;
    Core.apply_actions = actions } in
  TxSwitch.send sw.tx_switch (Msg.PacketOutMsg pktOut)
    
let flow_stats_request (sw : t) (pat : AL.pattern) : AL.flowStats list Lwt.t =
  raise_lwt (Failure "flow_stats_request NYI")

