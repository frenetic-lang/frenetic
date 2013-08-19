module Switch = OpenFlow0x01_Switch
module AL = SDN_types
module Core = OpenFlow0x01_Core
module Mod = ModComposition
module Msg = OpenFlow0x01.Message

let from_buffer_id (bufId : AL.bufferId) : int32 =
	let open SDN_types in
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
	let open SDN_types in
	match pay with
	| Buffered (buf_id, bytes) ->
	  Core.Buffered (from_buffer_id buf_id, Cstruct.of_string bytes)
	| NotBuffered bytes -> Core.NotBuffered (Cstruct.of_string bytes)

let from_port (port : AL.port) : Core.pseudoPort =
  let open SDN_types in
	match port with
	| AllPorts -> Core.AllPorts
	| Controller n -> Core.Controller n
	| PhysicalPort (OF10PortId n) -> Core.PhysicalPort n
  | PhysicalPort _ -> raise (Invalid_argument "expected OpenFlow 1.0 port ID")


let to_portId (portId : int) : AL.portId =
	AL.OF10PortId portId

let to_reason (reason : Core.packetInReason) : AL.packetInReason =
	let open Core in
	match reason with
	| ExplicitSend -> AL.ExplicitSend
	| NoMatch -> AL.NoMatch

let to_packetIn (pktIn : Core.packetIn) : AL.pktIn =
	let open Core in
	match pktIn with
	| { input_payload; total_len; port; reason } ->
	  (to_payload input_payload, total_len, to_portId port, to_reason reason)

let from_pattern (pat : AL.pattern) : Core.pattern =
	let open SDN_types in
	match pat with
	|  { inPort; ethType; ethSrc; ethDst; vlan; vlanPcp; ipProto; ip4Src; ip4Dst;
	     tcpSrcPort; tcpDstPort } ->
  	let open OpenFlow0x01_Core in
  	{ dlSrc = ethSrc;
  		dlDst = ethDst;
  		dlTyp = ethType;
  		dlVlan = (match vlan with
  		  | Some 0xFFFF -> Some None
  		  | Some x -> Some (Some x)
  		  | None -> None);
  		dlVlanPcp = vlanPcp;
  		nwSrc = ip4Src;
  		nwDst = ip4Dst;
  		nwProto = ipProto;
  		nwTos = None;
  		tpSrc = tcpSrcPort;
  		tpDst = tcpDstPort;
  		inPort = (match inPort with
      	| Some (OF10PortId n) -> Some n
      	| Some _ -> raise (Invalid_argument "expected OpenFlow 1.0 port ID")
        | None -> None)
  	}

(* Converts an abstract action into an OpenFlow 1.0 action. The operation may
   fail if the action in unrealizable. *)
let rec from_action (inPort : Core.portId option) (act : AL.action) 
  : Mod.t * Core.action list =
	let open SDN_types in
	let open OpenFlow0x01_Core in
	match act with
  | OutputAllPorts -> (Mod.none, [Output AllPorts])
  | OutputPort (OF10PortId n) ->
    if Some n = inPort then
	    (Mod.none, [Output InPort])
  	else
		  (Mod.none, [Output (PhysicalPort n)])
	| OutputPort _ -> raise (Invalid_argument "expected OpenFlow 1.0 port number")
	| SetField (AL.InPort _) -> raise (Invalid_argument "cannot set input port")
	| SetField (EthType _) -> raise (Invalid_argument "cannot set frame type")
	| SetField (EthSrc n) -> (Mod.dlSrc, [SetDlSrc n])
	| SetField (EthDst n) -> (Mod.dlDst , [SetDlDst n])
	| SetField (Vlan n) -> (Mod.dlVlan, [SetDlVlan (Some n)]) (* TODO(arjun): we should strip away our magic None *)
	| SetField (VlanPcp n) -> (Mod.dlVlanPcp, [SetDlVlanPcp n])
	| SetField (IPProto n) -> raise (Invalid_argument "cannot set IP protocol")
	| SetField (IP4Src n) -> (Mod.nwSrc, [SetNwSrc n])
	| SetField (IP4Dst n) -> (Mod.nwDst, [SetNwDst n])
	| SetField (TCPSrcPort n) -> (Mod.tpSrc, [SetTpSrc n])
	| SetField (TCPDstPort n) -> (Mod.tpDst, [SetTpDst n])
	| Seq (a1, a2) -> 
		let (mods1, seq1) = from_action inPort a1 in
		let (mods2, seq2) = from_action inPort a2 in
		(Mod.seq mods1 mods2, seq1 @ seq2)
	| Par (a1, a2) ->
		let (mods1, seq1) = from_action inPort a1 in
		let (mods2, seq2) = from_action inPort a2 in
		(Mod.par mods1 mods2, seq1 @ seq2)
	| Failover _ -> raise (Invalid_argument "cannot implement fast failover")

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
	  {
	    command = AddFlow;
  	  pattern = pat;
      priority = priority;
      actions = (let (_, act) = from_action pat.inPort action in act);
      cookie = cookie;
      idle_timeout = from_timeout idle_timeout;
      hard_timeout = from_timeout hard_timeout;
      notify_when_removed = false;
      apply_to_packet = None;
      out_port = None;
      check_overlap = false
      }

type t = {
	handle : Switch.t;
	packet_ins : AL.pktIn Lwt_stream.t;
	terminator: unit Lwt.u
}

let features (sw : t) : AL.switchFeatures =
	let feats = Switch.features sw.handle in
	let open OpenFlow0x01.SwitchFeatures in
	let from_portDesc desc =
	  AL.OF10PortId desc.OpenFlow0x01.PortDescription.port_no in
	{ AL.switch_id = AL.OF10SwitchId feats.switch_id;
    AL.switch_ports = List.map from_portDesc feats.ports
	}

let from_handle (handle : Switch.t) : t =
	let (packet_ins, send_pktIn) = Lwt_stream.create () in
	let (termination_thread, terminator) = Lwt.wait () in
	let termination_thread =
	  lwt _ = termination_thread in
	  send_pktIn None;
	  Lwt.return () in
	let rec switch_thread () =
	  match_lwt Switch.recv handle with
	  | (_, Msg.PacketInMsg pktIn) ->
	    send_pktIn (Some (to_packetIn pktIn));
	    switch_thread ()
	  | (xid, msg) ->
	  	(* TODO(arjun): log ignored messages *)
	  	switch_thread () in
	Lwt.async (fun () -> Lwt.pick [ switch_thread (); termination_thread ]);
	{ handle; packet_ins; terminator }

let disconnect (t : t) : unit Lwt.t = 
  Lwt.wakeup t.terminator ();
  Lwt.return ()

let setup_flow_table (sw : t) (tbl : AL.flowTable) : unit Lwt.t =
  let priority = ref 65535 in
  let send_flow_mod (flow : AL.flow) =
    lwt flow_mod = Lwt.wrap2 from_flow !priority flow in
    lwt _ = Switch.send sw.handle 0l (Msg.FlowModMsg flow_mod) in
    decr priority; (* TODO(arjun): range check *)
    Lwt.return () in
  lwt _ = Switch.send sw.handle 0l (Msg.FlowModMsg Core.delete_all_flows) in
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
	  Core.apply_actions = actions
	} in
	Switch.send sw.handle 0l (Msg.PacketOutMsg pktOut)

let flow_stats_request (sw : t) (pat : AL.pattern) : AL.flowStats list Lwt.t =
  raise_lwt (Failure "flow_stats_request NYI")

