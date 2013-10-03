module Switch = OpenFlow0x04_Switch
module TxSwitch = OpenFlow0x04_TxSwitch
module AL = SDN_Types
module Core = OpenFlow0x04_Core
module Mod = ModComposition
module Msg = OpenFlow0x04.Message
module Fields = AL.FieldMap

let from_buffer_id (bufId : AL.bufferId) : int32 =
  let open SDN_Types in
  match bufId with
    | OF13BufferId n -> n
    | OF10BufferId _ ->
      raise (Invalid_argument "expected OpenFlow 1.3 buffer ID")
	
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
    | PhysicalPort (VInt.Int32 n) -> Core.PhysicalPort n
    | PhysicalPort _ -> raise (Invalid_argument "expected OpenFlow 1.3 port ID")

let to_reason (reason : Core.packetInReason) : AL.packetInReason =
  let open Core in
  match reason with
    | ExplicitSend -> AL.ExplicitSend
    | NoMatch -> AL.NoMatch
      
let to_packetIn (pktIn : Core.packetIn) : AL.pktIn =
  let open Core in
  let get_port = 
    List.fold_left (fun acc x -> match x with
      | OxmInPort p -> p
      | _ -> acc) Int32.zero
  in
  match pktIn with
    | { pi_payload; pi_total_len; pi_ofp_match; pi_reason } ->
      (to_payload pi_payload, pi_total_len, VInt.Int32 (get_port pi_ofp_match), to_reason pi_reason)

let from_pattern (pat : AL.pattern) : Core.oxmMatch * Core.portId option = 
  let v_to_m = Core.val_to_mask in
  let lookup conv field =
    try [(conv (Fields.find field pat))]
    with Not_found -> [] in
    ((lookup (fun x -> Core.OxmEthSrc (v_to_m (VInt.get_int48 x))) AL.EthSrc) 
     @ (lookup (fun x -> Core.OxmEthDst (v_to_m (VInt.get_int48 x))) AL.EthDst) 
     @ (lookup (fun x -> Core.OxmEthType (VInt.get_int16 x)) AL.EthType)
     @ (try match VInt.get_int16 (Fields.find AL.Vlan pat) with
       | 0xFFFF -> []
       | x -> [ Core.OxmVlanVId (v_to_m x) ]
       with Not_found -> [])
     @ (lookup (fun x -> Core.OxmVlanPcp (VInt.get_int4 x)) AL.VlanPcp)
     @ (lookup (fun x -> Core.OxmIP4Src (v_to_m (VInt.get_int32 x))) AL.IP4Src)
     @ (lookup (fun x -> Core.OxmIP4Dst (v_to_m (VInt.get_int32 x))) AL.IP4Dst)
     @ (lookup (fun x -> Core.OxmIPProto (VInt.get_int8 x)) AL.IPProto)
     @ (lookup (fun x -> Core.OxmTCPSrc (v_to_m (VInt.get_int16 x))) AL.TCPSrcPort)
     @ (lookup (fun x -> Core.OxmTCPDst (v_to_m (VInt.get_int16 x))) AL.TCPDstPort)
     @ (lookup (fun x -> Core.OxmInPort (VInt.get_int32 x)) AL.InPort),
     try Some (VInt.get_int32 (Fields.find AL.InPort pat))
     with Not_found -> None)

(* Converts an abstract action into an OpenFlow 1.3 action. The operation may
   fail if the action in unrealizable. *)
let from_action (inPort : Core.portId option) (act : AL.action) 
  : Mod.t * Core.action list =
  let v_to_m = Core.val_to_mask in
  let open Core in
  let open SDN_Types in
  match act with
    | OutputAllPorts -> (Mod.none, [Output Core.AllPorts])
    | OutputPort (VInt.Int32 n) ->
      if Some n = inPort then
	(Mod.none, [Output Core.InPort])
      else
	(Mod.none, [Output (Core.PhysicalPort n)])
    | OutputPort _ -> raise (Invalid_argument "expected OpenFlow 1.3 port number")
    | SetField (AL.InPort, _) -> raise (Invalid_argument "cannot set input port")
    | SetField (EthType, _) -> raise (Invalid_argument "cannot set frame type")
    | SetField (EthSrc, VInt.Int48 n) -> (Mod.dlSrc, [Core.SetField (OxmEthSrc (v_to_m n))])
    | SetField (EthDst, VInt.Int48 n) -> (Mod.dlDst , [Core.SetField (OxmEthDst (v_to_m n))])
    | SetField (Vlan, VInt.Int16 0xFFFF) -> (Mod.dlVlan, [Core.PopVlan])
    | SetField (Vlan, VInt.Int16 n) -> (Mod.dlVlan, [Core.SetField (OxmVlanVId (v_to_m n))])
    | SetField (VlanPcp, VInt.Int4 n) -> (Mod.dlVlanPcp, [Core.SetField (OxmVlanPcp n)])
    (* MJR: This seems silly. OF 1.3 has no such restriction *)
    | SetField (IPProto, _) -> raise (Invalid_argument "cannot set IP protocol")
    | SetField (IP4Src, VInt.Int32 n) -> (Mod.nwSrc, [Core.SetField (OxmIP4Src (v_to_m n))])
    | SetField (IP4Dst, VInt.Int32 n) -> (Mod.nwDst, [Core.SetField (OxmIP4Dst (v_to_m n))])
    | SetField (TCPSrcPort, VInt.Int16 n) -> (Mod.tpSrc, [Core.SetField (OxmTCPSrc (v_to_m n))])
    | SetField (TCPDstPort, VInt.Int16 n) -> (Mod.tpDst, [Core.SetField (OxmTCPDst (v_to_m n))])
    | SetField _ -> raise (Invalid_argument "invalid SetField combination")
    | EmptyAction -> (Mod.none, [])

let rec from_seq (inPort : Core.portId option) (seq : AL.seq) 
  : Mod.t * Core.action list =
  let open SDN_Types in
  let open OpenFlow0x01_Core in
  match seq with
  | Act act -> from_action inPort act
  | Seq (act, s2) ->
    let (mods1, seq1) = from_action inPort act in
    let (mods2, seq2) = from_seq inPort s2 in
    (Mod.par mods1 mods2, seq1 @ seq2)

let rec from_par (inPort : Core.portId option) (par : AL.par) 
  : Mod.t * Core.action list =
  let open SDN_Types in
  let open OpenFlow0x01_Core in
  match par with
  | SeqP seq -> from_seq inPort seq
  | Par (seq, p2) ->
    let (mods1, seq1) = from_seq inPort seq in
    let (mods2, seq2) = from_par inPort p2 in
    (Mod.par mods1 mods2, seq1 @ seq2)

let from_group (inPort : Core.portId option) (act : AL.group) 
  : Core.action list =
  let open SDN_Types in
  let open OpenFlow0x01_Core in
  match act with
  | Action par -> let (_, act2) = from_par inPort par in act2
  (* MJR TODO: fix this *)
  (* How do we allocated the group Id? *)
  | Failover fo -> [Core.Group Int32.zero]

(*
  One set of actions per bucket.
    begin
      let groups = List.map (fun par -> let (_, act) = from_par inPort par in act) fo in
    end
*)
  
let from_timeout (timeout : AL.timeout) : Core.timeout =
  match timeout with
    | AL.Permanent -> Core.Permanent
    | AL.ExpiresAfter n -> Core.ExpiresAfter n
      
(* TODO: in case of failover, the flow mod is a group action *)
let from_flow (priority : int) (flow : AL.flow) : Core.flowMod = 
  let open AL in
      match flow with
	| { pattern; action; cookie; idle_timeout; hard_timeout } ->
	  let pat,inport = from_pattern pattern in
	  let open Core in 
	  { mfCommand = AddFlow;
  	    mfOfp_match = pat;
	    mfPriority = priority;
      mfInstructions = [Core.ApplyActions (from_group inport action)];
	    mfCookie = Core.val_to_mask cookie;
	    mfIdle_timeout = from_timeout idle_timeout;
	    mfHard_timeout = from_timeout hard_timeout;
            mfTable_id = 0;
            mfFlags = {fmf_send_flow_rem = false; 
                       fmf_check_overlap = false;
                       fmf_reset_counts = false;
                       fmf_no_pkt_counts = false;
                       fmf_no_byt_counts = false };
	    mfBuffer_id = None;
	    mfOut_port = None;
            mfOut_group = None }
	    
type t =
    { switch : Switch.t;
      tx_switch : TxSwitch.t;
      packet_ins : AL.pktIn Lwt_stream.t }

let features (sw : t) : AL.switchFeatures =
  let feats = Switch.features sw.switch in
  let ports = Switch.ports sw.switch in
  let open OpenFlow0x04.SwitchFeatures in
  let from_portDesc desc =
    VInt.Int32 desc.Core.port_no in
  { AL.switch_id = VInt.Int64 feats.datapath_id;
    AL.switch_ports = List.map from_portDesc ports }

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
    Core.po_payload = pay;
    (* I believe port_id affects the semantics of action of the (Output InPort)
       action. Since the abstract actions cannot state InPort, by applying 
       [from_action None], we never generate an InPort action. *)
    Core.po_in_port = Core.Controller 0;
    Core.po_actions = actions } in
  TxSwitch.send sw.tx_switch (Msg.PacketOutMsg pktOut)
    
let flow_stats_request (sw : t) (pat : AL.pattern) : AL.flowStats list Lwt.t =
  raise_lwt (Failure "flow_stats_request NYI")

