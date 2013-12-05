module Switch = OpenFlow0x01_Switch
module TxSwitch = OpenFlow0x01_TxSwitch
module AL = SDN_Types
module Core = OpenFlow0x01_Core
module Msg = OpenFlow0x01.Message
module Fields = AL.FieldMap

let from_buffer_id (bufId : AL.bufferId) : int32 =
  let open SDN_Types in
  match bufId with
    | OF10BufferId n -> n
    | OF13BufferId _ ->
      raise (Invalid_argument "expected OpenFlow 1.0 buffer ID")
	
let to_payload (pay : Core.payload) : AL.payload =
  let open Core in
  match pay with
    | Buffered (buf_id, ct) ->
      AL.Buffered (AL.OF10BufferId buf_id, ct)
    | NotBuffered ct ->
      AL.NotBuffered ct
	
let from_payload (pay : AL.payload) : Core.payload =
  let open SDN_Types in
  match pay with
    | Buffered (buf_id, bytes) ->
      Core.Buffered (from_buffer_id buf_id, bytes)
    | NotBuffered bytes -> Core.NotBuffered bytes
      
let from_port (port : AL.port) : Core.pseudoPort =
  let open SDN_Types in
  match port with
    | AllPorts -> Core.AllPorts
    | Controller n -> Core.Controller n
    | PhysicalPort (VInt.Int16 n) -> Core.PhysicalPort n
    | PhysicalPort _ -> raise (Invalid_argument "expected OpenFlow 1.0 port ID")

let to_reason (reason : Core.packetInReason) : AL.packetInReason =
  let open Core in
  match reason with
    | ExplicitSend -> AL.ExplicitSend
    | NoMatch -> AL.NoMatch
      
let to_packetIn (pktIn : Core.packetIn) : AL.pktIn =
  let open Core in
  match pktIn with
    | { input_payload; total_len; port; reason } ->
      (to_payload input_payload, total_len, VInt.Int16 port, to_reason reason)

let from_pattern (pat : AL.pattern) : Core.pattern = 
  let lookup conv field =
    try Some (conv (Fields.find field pat))
    with Not_found -> None in
  { Core.dlSrc = lookup VInt.get_int48 AL.EthSrc;
    Core.dlDst = lookup VInt.get_int48 AL.EthDst;
    Core.dlTyp = lookup VInt.get_int16 AL.EthType;
    Core.dlVlan = 
      (try match VInt.get_int16 (Fields.find AL.Vlan pat) with
	| 0xFFFF -> Some None
	| x -> Some (Some x)
       with Not_found -> None);
    Core.dlVlanPcp = lookup VInt.get_int4 AL.VlanPcp;
    Core.nwSrc = lookup VInt.get_int32 AL.IP4Src;
    Core.nwDst = lookup VInt.get_int32 AL.IP4Dst;
    Core.nwProto = lookup VInt.get_int8 AL.IPProto;
    Core.nwTos = None; (* Forgot to define it at the abstraction layer *)
    Core.tpSrc = lookup VInt.get_int16 AL.TCPSrcPort;
    Core.tpDst = lookup VInt.get_int16 AL.TCPDstPort;
    Core.inPort = lookup VInt.get_int16 AL.InPort }

module Common = HighLevelSwitch_common.Make (struct
  type of_action = Core.action
  type of_portId = Core.portId

  module Mod = ModComposition
  
  let from_action (inPort : Core.portId option) (act : AL.action) 
    : Mod.t * Core.action  =
    let open OpenFlow0x01_Core in
    match act with
      | AL.OutputAllPorts -> 
        (Mod.none, Output AllPorts)
      | AL.OutputPort n ->
	let n = VInt.get_int16 n in 
        if Some n = inPort then
          (Mod.none, Output InPort)
        else
          (Mod.none, Output (PhysicalPort n))
      | AL.Enqueue (m,n) -> 
        let m = VInt.get_int16 m in 
        let n = VInt.get_int32 n in 
        if Some m = inPort then 
          (Mod.none, Enqueue(InPort, n))
        else 
          (Mod.none, Enqueue (PhysicalPort m, n))
      | AL.SetField (AL.InPort, _) -> raise (Invalid_argument "cannot set input port")
      | AL.SetField (AL.EthType, _) -> raise (Invalid_argument "cannot set frame type")
      | AL.SetField (AL.EthSrc, n) -> (Mod.dlSrc, SetDlSrc (VInt.get_int48 n))
      | AL.SetField (AL.EthDst, n) -> (Mod.dlDst , SetDlDst (VInt.get_int48 n))
      | AL.SetField (AL.Vlan, n) -> 
	begin match VInt.get_int16 n with 
	  | 0xFFFF -> (Mod.dlVlan, SetDlVlan None)
	  | n -> (Mod.dlVlan, SetDlVlan (Some n))
	end
      | AL.SetField (AL.VlanPcp, n) -> (Mod.dlVlanPcp, SetDlVlanPcp (VInt.get_int4 n))
      | AL.SetField (AL.IPProto, _) -> raise (Invalid_argument "cannot set IP protocol")
      | AL.SetField (AL.IP4Src, n) -> (Mod.nwSrc, SetNwSrc (VInt.get_int32 n))
      | AL.SetField (AL.IP4Dst, n) -> (Mod.nwDst, SetNwDst (VInt.get_int32 n))
      | AL.SetField (AL.TCPSrcPort, n) -> (Mod.tpSrc, SetTpSrc (VInt.get_int16 n))
      | AL.SetField (AL.TCPDstPort, n) -> (Mod.tpDst, SetTpDst (VInt.get_int16 n))
end)

let from_group (inPort : Core.portId option) (group : AL.group) : Core.action list =
  match group with
  | [] -> []
  | [par] -> Common.flatten_par inPort par
  | _ -> raise (SDN_Types.Unsupported "OpenFlow 1.0 does not support fast-failover")
      
let from_timeout (timeout : AL.timeout) : Core.timeout =
  match timeout with
    | AL.Permanent -> Core.Permanent
    | AL.ExpiresAfter n -> Core.ExpiresAfter n
      
let from_flow (priority : int) (flow : AL.flow) : Core.flowMod = 
  let open AL in
  match flow with
	| { pattern; action; cookie; idle_timeout; hard_timeout } ->
    let pat = from_pattern pattern in
	  let open Core in 
	  { command = AddFlow;
	    pattern = pat;
	    priority = priority;
	    actions = from_group pat.inPort action;
	    cookie = cookie;
	    idle_timeout = from_timeout idle_timeout;
	    hard_timeout = from_timeout hard_timeout;
	    notify_when_removed = false;
	    apply_to_packet = None;
	    out_port = None;
	    check_overlap = false }
	    
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

