module Switch = OpenFlow0x01_Switch
module TxSwitch = OpenFlow0x01_TxSwitch
module AL = SDN_Types
module Core = OpenFlow0x01_Core
module Mod = ModComposition
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
      AL.Buffered (AL.OF10BufferId buf_id, Cstruct.to_string ct)
    | NotBuffered ct ->
      AL.NotBuffered (Cstruct.to_string ct)
	
let from_payload (pay : AL.payload) : Core.payload =
  let open SDN_Types in
  match pay with
    | Buffered (buf_id, bytes) ->
      Core.Buffered (from_buffer_id buf_id, Cstruct.of_string bytes)
    | NotBuffered bytes -> Core.NotBuffered (Cstruct.of_string bytes)
      
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
    
(* Converts an abstract action into an OpenFlow 1.0 action. The operation may
   fail if the action in unrealizable. *)
let rec from_action (inPort : Core.portId option) (act : AL.action) 
  : Mod.t * Core.action list =
  let open SDN_Types in
  let open OpenFlow0x01_Core in
  match act with
    | OutputAllPorts -> (Mod.none, [Output AllPorts])
    | OutputPort (VInt.Int16 n) ->
      if Some n = inPort then
	(Mod.none, [Output InPort])
      else
	(Mod.none, [Output (PhysicalPort n)])
    | OutputPort _ -> raise (Invalid_argument "expected OpenFlow 1.0 port number")
    | SetField (AL.InPort, _) -> raise (Invalid_argument "cannot set input port")
    | SetField (EthType, _) -> raise (Invalid_argument "cannot set frame type")
    | SetField (EthSrc, VInt.Int48 n) -> (Mod.dlSrc, [SetDlSrc n])
    | SetField (EthDst, VInt.Int48 n) -> (Mod.dlDst , [SetDlDst n])
    | SetField (Vlan, VInt.Int16 0xFFFF) -> (Mod.dlVlan, [SetDlVlan None])
    | SetField (Vlan, VInt.Int16 n) -> (Mod.dlVlan, [SetDlVlan (Some n)])
    | SetField (VlanPcp, VInt.Int4 n) -> (Mod.dlVlanPcp, [SetDlVlanPcp n])
    | SetField (IPProto, _) -> raise (Invalid_argument "cannot set IP protocol")
    | SetField (IP4Src, VInt.Int32 n) -> (Mod.nwSrc, [SetNwSrc n])
    | SetField (IP4Dst, VInt.Int32 n) -> (Mod.nwDst, [SetNwDst n])
    | SetField (TCPSrcPort, VInt.Int16 n) -> (Mod.tpSrc, [SetTpSrc n])
    | SetField (TCPDstPort, VInt.Int16 n) -> (Mod.tpDst, [SetTpDst n])
    | SetField _ -> raise (Invalid_argument "invalid SetField combination")
    | Seq (a1, a2) -> 
      let (mods1, seq1) = from_action inPort a1 in
      let (mods2, seq2) = from_action inPort a2 in
      (Mod.seq mods1 mods2, seq1 @ seq2)
    | Par (a1, a2) ->
      let (mods1, seq1) = from_action inPort a1 in
      let (mods2, seq2) = from_action inPort a2 in
      (Mod.par mods1 mods2, seq1 @ seq2)
    | Failover _ -> raise (Invalid_argument "cannot implement fast failover")
    | EmptyAction -> (Mod.none, [])
      
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
	    actions = (let (_, act) = from_action pat.inPort action in act);
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
    
let packet_out (sw : t) (pay : AL.payload) (act : AL.action) : unit Lwt.t =
  lwt pay = Lwt.wrap1 from_payload pay in
  lwt (_, actions) = Lwt.wrap2 from_action None act in
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

