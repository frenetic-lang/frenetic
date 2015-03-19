open OpenFlow0x01
module AL = SDN_Types
module Msg = OpenFlow0x01.Message

exception Invalid_port of int32

let from_portId (pport_id : AL.portId) : portId =
  if pport_id > 0xff00l then (* pport_id <= OFPP_MAX *)
    raise (Invalid_port pport_id)
  else
    Int32.to_int pport_id

let to_payload (pay : payload) : AL.payload =
  let open Core in
  match pay with
    | Buffered (buf_id, ct) ->
      AL.Buffered (buf_id, ct)
    | NotBuffered ct ->
      AL.NotBuffered ct

let from_payload (pay : AL.payload) : payload =
  let open SDN_Types in
  match pay with
    | Buffered (buf_id, bytes) ->
      Buffered (buf_id, bytes)
    | NotBuffered bytes -> NotBuffered bytes

let to_reason (reason : packetInReason) : AL.packetInReason =
  let open Core in
  match reason with
    | ExplicitSend -> AL.ExplicitSend
    | NoMatch -> AL.NoMatch

let to_packetIn (pktIn : packetIn) : AL.pktIn =
  let open Core in
  match pktIn with
    | { input_payload; total_len; port; reason } ->
      (to_payload input_payload, total_len, Int32.of_int port, to_reason reason)

let to_pattern (p:pattern) : SDN_Types.Pattern.t =
  let open SDN_Types.Pattern in
  { dlSrc = p.dlSrc
  ; dlDst = p.dlDst
  ; dlTyp = p.dlTyp
  ; dlVlan = begin match p.dlVlan with
      | None -> None
      | Some None -> Some(0xffff)
      | Some vlan -> vlan
    end
  ; dlVlanPcp = p.dlVlanPcp
  ; nwSrc = begin match p.nwSrc with
      | None -> None
      | Some({ m_value; m_mask = None }) ->
        Some(m_value, 0l)
      | Some({ m_value; m_mask = Some(m) }) ->
        Some(m_value, Int32.sub 32l m)
    end
  ; nwDst = begin match p.nwDst with
      | None -> None
      | Some({ m_value; m_mask = None }) ->
        Some(m_value, 0l)
      | Some({ m_value; m_mask = Some(m) }) ->
        Some(m_value, Int32.sub 32l m)
    end
  ; nwProto = p.nwProto
  ; tpSrc = p.tpSrc
  ; tpDst = p.tpDst
  ; inPort = match p.inPort with
      | None    -> None
      | Some(n) -> Some(Int32.of_int n)
  }

let to_pseudoPort (in_port : portId option) pport =
  let open Core in
  match pport with
  | PhysicalPort port ->
    if Some port = in_port
      then AL.InPort
      else AL.Physical (Int32.of_int port)
  | InPort -> AL.InPort
  | Table  -> AL.Table
  | Normal -> AL.Normal
  | Flood  -> AL.Flood
  | AllPorts -> AL.All
  | Controller(buf) -> AL.Controller(buf)
  | Local -> AL.Local

let to_action (in_port : portId option) (act : action) : AL.action =
  let open Core in
  match act with
  | Output pport -> AL.Output (to_pseudoPort in_port pport)
  | SetDlVlan dlVlan -> AL.(Modify(SetVlan dlVlan))
  | SetDlVlanPcp dlVlanPcp -> AL.(Modify(SetVlanPcp dlVlanPcp))
  | SetDlSrc dlAddr -> AL.(Modify(SetEthSrc dlAddr))
  | SetDlDst dlAddr -> AL.(Modify(SetEthDst dlAddr))
  | SetNwSrc nwAddr -> AL.(Modify(SetIP4Src nwAddr))
  | SetNwDst nwAddr -> AL.(Modify(SetIP4Dst nwAddr))
  | SetNwTos nwTos -> AL.(Modify(SetIPProto nwTos))
  | SetTpSrc tpPort -> AL.(Modify(SetTCPDstPort tpPort))
  | SetTpDst tpPort -> AL.(Modify(SetTCPSrcPort tpPort))
  | Enqueue _ -> assert false (* XXX(seliopou) raise an exception. It's not
      possible to implement this without changing the types in SDN_Types. *)

let to_flowStats stats : SDN_Types.flowStats =
  let open SDN_Types in
  let pattern = to_pattern stats.of_match in
  let inPort = match pattern.Pattern.inPort with
    | None -> None
    | Some(x) -> Some(from_portId x)
  in
  { flow_table_id = stats.table_id
  ; flow_pattern = pattern
  ; flow_duration_sec = stats.duration_sec
  ; flow_duration_nsec = stats.duration_nsec
  ; flow_priority = stats.priority
  ; flow_idle_timeout = stats.idle_timeout
  ; flow_hard_timeout = stats.hard_timeout
  ; flow_actions = List.map (to_action inPort) stats.actions
  ; flow_packet_count = stats.packet_count
  ; flow_byte_count = stats.byte_count
  }

let from_pattern (pat : AL.Pattern.t) : pattern =
  { dlSrc = pat.AL.Pattern.dlSrc
  ; dlDst = pat.AL.Pattern.dlDst
  ; dlTyp = pat.AL.Pattern.dlTyp
  ; dlVlan = (match pat.AL.Pattern.dlVlan with
      | Some(0xffff) -> Some None
      | Some(x) -> Some (Some x)
      | None -> None)
  ; dlVlanPcp = pat.AL.Pattern.dlVlanPcp
  ; nwSrc = (match pat.AL.Pattern.nwSrc with
    | None -> None
    | Some (p,m) ->
       let mo =
         if m = 32l then
           None
         else
           Some (Int32.sub 32l m) in
       Some { m_value = p; m_mask = mo })
  ; nwDst = (match pat.AL.Pattern.nwDst with
    | None -> None
    | Some (p,m) ->
       let mo =
         if m = 32l then
           None
         else
           Some (Int32.sub 32l m) in
       Some { m_value = p; m_mask = mo })
  ; nwProto = pat.AL.Pattern.nwProto
  ; nwTos = None
  ; tpSrc = pat.AL.Pattern.tpSrc
  ; tpDst = pat.AL.Pattern.tpDst
  ; inPort = Core_kernel.Option.map pat.AL.Pattern.inPort from_portId
  }

module Common = HighLevelSwitch_common.Make (struct
  type of_action = action
  type of_portId = portId

  module Mod = ModComposition

  let from_output (inPort : portId option) (pseudoport : AL.pseudoport) =
    match pseudoport with
      | AL.InPort ->
        (Mod.none, Output InPort)
      | AL.Table -> (* XXX(seliopou): Maybe table should take the portid *)
        (Mod.none, Output Table)
      | AL.Normal ->
        (Mod.none, Output Normal)
      | AL.Flood ->
        (Mod.none, Output Flood)
      | AL.All ->
        (Mod.none, Output AllPorts)
      | AL.Physical pport_id ->
         let pport_id = from_portId pport_id in
         if Some pport_id = inPort then
           (Mod.none, Output InPort)
         else
           (Mod.none, Output (PhysicalPort pport_id))
      | AL.Controller n ->
        (Mod.none, Output (Controller n))
      | AL.Local ->
        (Mod.none, Output Local)

  let from_action (inPort : portId option) (act : AL.action)
    : Mod.t * action  =
    match act with
      | AL.Output pseudoport ->
        from_output inPort pseudoport
      | AL.Enqueue (pport_id, queue_id) ->
        let pport_id = from_portId pport_id in
        if Some pport_id = inPort then
          (Mod.none, Enqueue(InPort, queue_id))
        else
          (Mod.none, Enqueue (PhysicalPort pport_id, queue_id))
      | AL.Modify (AL.SetEthSrc dlAddr) ->
        (Mod.dlSrc, SetDlSrc VInt.(get_int48 (Int64 dlAddr)))
      | AL.Modify (AL.SetEthDst dlAddr) ->
        (Mod.dlDst , SetDlDst VInt.(get_int48 (Int64 dlAddr)))
      | AL.Modify (AL.SetVlan vlan) ->
        begin match vlan with
          | None
          | Some(0xffff) ->
            (Mod.dlVlan, SetDlVlan None)
          | Some(n) ->
            let n = VInt.(get_int12 (Int16 n)) in
            (Mod.dlVlan, SetDlVlan (Some n))
        end
      | AL.Modify (AL.SetVlanPcp pcp) ->
        (Mod.dlVlanPcp, SetDlVlanPcp(VInt.(get_int4 (Int4 pcp))))
      | AL.Modify (AL.SetEthTyp _) ->
         raise (Invalid_argument "cannot set Ethernet type")
      | AL.Modify (AL.SetIPProto _) ->
         raise (Invalid_argument "cannot set IP protocol")
      | AL.Modify (AL.SetIP4Src nwAddr) ->
        (Mod.nwSrc, SetNwSrc nwAddr)
      | AL.Modify (AL.SetIP4Dst nwAddr) ->
        (Mod.nwDst, SetNwDst nwAddr)
      | AL.Modify (AL.SetTCPSrcPort tp) ->
         (Mod.tpSrc, SetTpSrc VInt.(get_int16 (Int16 tp)))
      | AL.Modify (AL.SetTCPDstPort tp) ->
        (Mod.tpDst, SetTpDst VInt.(get_int16 (Int16 tp)))
end)

open Common

let from_group (inPort : portId option) (group : AL.group)
  : action list =
  match group with
  | [] -> []
  | [par] -> flatten_par inPort par
  | _ ->
     raise (SDN_Types.Unsupported "OpenFlow 1.0 does not support fast-failover")

let from_timeout (timeout : AL.timeout) : timeout =
  match timeout with
    | AL.Permanent -> Permanent
    | AL.ExpiresAfter n -> ExpiresAfter n

let from_flow (priority : int) (flow : AL.flow) : flowMod =
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

let from_packetOut (pktOut : AL.pktOut) : packetOut =
  let open Core in
  let output_payload, port_id, apply_actions = pktOut in
  let output_payload = from_payload output_payload in
  let port_id = Core_kernel.Option.map port_id from_portId in
  let apply_actions = flatten_par port_id [apply_actions] in
  { output_payload; port_id; apply_actions }
