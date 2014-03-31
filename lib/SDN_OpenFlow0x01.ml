module AL = SDN_Types
module Core = OpenFlow0x01_Core
module Msg = OpenFlow0x01.Message


exception Invalid_port of int32

let to_payload (pay : Core.payload) : AL.payload =
  let open Core in
  match pay with
    | Buffered (buf_id, ct) ->
      AL.Buffered (buf_id, ct)
    | NotBuffered ct ->
      AL.NotBuffered ct
  
let from_payload (pay : AL.payload) : Core.payload =
  let open SDN_Types in
  match pay with
    | Buffered (buf_id, bytes) ->
      Core.Buffered (buf_id, bytes)
    | NotBuffered bytes -> Core.NotBuffered bytes
      
let to_reason (reason : Core.packetInReason) : AL.packetInReason =
  let open Core in
  match reason with
    | ExplicitSend -> AL.ExplicitSend
    | NoMatch -> AL.NoMatch
      
let to_packetIn (pktIn : Core.packetIn) : AL.pktIn =
  let open Core in
  match pktIn with
    | { input_payload; total_len; port; reason } ->
      (to_payload input_payload, total_len, Int32.of_int port, to_reason reason)

let from_pattern (pat : AL.pattern) : Core.pattern = 
  { Core.dlSrc = pat.AL.dlSrc
  ; Core.dlDst = pat.AL.dlDst
  ; Core.dlTyp = pat.AL.dlTyp
  ; Core.dlVlan = (match pat.AL.dlVlan with
      | Some(Some(0xffff)) -> Some None
      | _ -> pat.AL.dlVlan)
  ; Core.dlVlanPcp = pat.AL.dlVlanPcp
  ; Core.nwSrc = (match pat.AL.nwSrc with
    | None   -> None
    | Some v -> Some { Core.m_value = v; Core.m_mask = None })
  ; Core.nwDst = (match pat.AL.nwSrc with
    | None   -> None
    | Some v -> Some { Core.m_value = v; Core.m_mask = None })
  ; Core.nwProto = pat.AL.nwProto
  ; Core.nwTos = None
  ; Core.tpSrc = pat.AL.tpSrc
  ; Core.tpDst = pat.AL.tpDst
  ; Core.inPort = (match pat.AL.inPort with
    | None   -> None
    | Some v -> if v >= 0xff00l (* pport_id < OFPP_MAX *)
      then raise (Invalid_port v)
      else Some(Int32.to_int v))
  }

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
      | AL.OutputPort pport_id ->
        if pport_id >= 0xff00l then (* pport_id < OFPP_MAX *)
          raise (Invalid_port pport_id);
        let pport_id = Int32.to_int pport_id in
        if Some pport_id = inPort then
          (Mod.none, Output InPort)
        else
          (Mod.none, Output (PhysicalPort pport_id))
      | AL.Controller n -> 
        (Mod.none, Output (Controller n))
      | AL.Enqueue (pport_id, queue_id) ->
        if pport_id >= 0xff00l then (* pport_id < OFPP_MAX *)
          raise (Invalid_port pport_id);
        let pport_id = Int32.to_int pport_id in
        if Some pport_id = inPort then
          (Mod.none, Enqueue(InPort, queue_id))
        else 
          (Mod.none, Enqueue (PhysicalPort pport_id, queue_id))
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
