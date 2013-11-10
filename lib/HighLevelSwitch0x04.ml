module Switch = OpenFlow0x04_Switch
module TxSwitch = OpenFlow0x04_TxSwitch
module AL = SDN_Types
module Core = OpenFlow0x04_Core
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
       | -1 -> [Core.OxmVlanVId {Core.m_value = 0x1000; Core.m_mask = Some 0x1000}]
       | 0xFFFF -> [Core.OxmVlanVId (v_to_m 0)]
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

let from_timeout (timeout : AL.timeout) : Core.timeout =
  match timeout with
    | AL.Permanent -> Core.Permanent
    | AL.ExpiresAfter n -> Core.ExpiresAfter n

module Common = HighLevelSwitch_common.Make (struct
  type of_action = Core.action
  type of_portId = Core.portId

  module Mod = ModComposition

  let from_action (inPort : Core.portId option) (act : AL.action) 
    : Mod.t * Core.action =
    let v_to_m = Core.val_to_mask in
    let open Core in
    let open SDN_Types in
    match act with
      | OutputAllPorts -> (Mod.none, Output Core.AllPorts)
      | OutputPort (VInt.Int32 n) ->
        if Some n = inPort then
    (Mod.none, Output Core.InPort)
        else
    (Mod.none, Output (Core.PhysicalPort n))
      | OutputPort n -> let n = VInt.get_int32 n in
                        if Some n = inPort then
                          (Mod.none, Output Core.InPort)
                        else
                          (Mod.none, Output (Core.PhysicalPort n))
      | SetField (AL.InPort, _) -> raise (Invalid_argument "cannot set input port")
      | SetField (EthType, _) -> raise (Invalid_argument "cannot set frame type")
      | SetField (EthSrc, VInt.Int48 n) -> (Mod.dlSrc, Core.SetField (OxmEthSrc (v_to_m n)))
      | SetField (EthDst, VInt.Int48 n) -> (Mod.dlDst , Core.SetField (OxmEthDst (v_to_m n)))
      | SetField (Vlan, n) -> let n = VInt.get_int16 n in
                              begin
                                match n with
                                  | 0xFFFF -> (Mod.dlVlan, Core.PopVlan)
                                  | -1 -> (Mod.dlVlan, Core.PushVlan)
                                  | _ -> (Mod.dlVlan, Core.SetField (OxmVlanVId (v_to_m n)))
                              end
      | SetField (VlanPcp, n) -> let n = VInt.get_int4 n in
                                 (Mod.dlVlanPcp, Core.SetField (OxmVlanPcp n))
      (* MJR: This seems silly. OF 1.3 has no such restriction *)
      | SetField (IPProto, _) -> raise (Invalid_argument "cannot set IP protocol")
      | SetField (IP4Src, VInt.Int32 n) -> (Mod.nwSrc, Core.SetField (OxmIP4Src (v_to_m n)))
      | SetField (IP4Dst, VInt.Int32 n) -> (Mod.nwDst, Core.SetField (OxmIP4Dst (v_to_m n)))
      | SetField (TCPSrcPort, VInt.Int16 n) -> (Mod.tpSrc, Core.SetField (OxmTCPSrc (v_to_m n)))
      | SetField (TCPDstPort, VInt.Int16 n) -> (Mod.tpDst, Core.SetField (OxmTCPDst (v_to_m n)))
      | SetField _ -> raise (Invalid_argument "invalid SetField combination")

  end)

(* calculates the watch port *)
let rec auto_watch_port (actionSequence : Core.actionSequence) (inPort : Core.portId option)
  : Core.portId option = match actionSequence with
  | [] -> None
  | (Core.Output (Core.PhysicalPort n)) :: _ -> Some n
  | (Core.Output Core.InPort) :: _ -> inPort
  | _ :: rest -> auto_watch_port rest inPort

let auto_ff_bucket (inPort : Core.portId option) (par : AL.par) : Core.bucket = 
  let open Core in
  let bu_actions = Common.flatten_par inPort par in
  let bu_watch_port = auto_watch_port bu_actions inPort in
  let bu_watch_group = None in
  let bu_weight = 0 in
  { bu_weight; bu_watch_port; bu_watch_group; bu_actions }
  

let from_group (groupTable : GroupTable0x04.t) (inPort : Core.portId option)
  (act : AL.group) 
  : Core.action list =
  let open SDN_Types in
  match act with
  | [] -> []
  | [par] -> Common.flatten_par inPort par
  | pars ->
    let buckets = List.map (auto_ff_bucket inPort) pars in
    let group_id = GroupTable0x04.add_group groupTable Core.FF buckets in
    [Core.Group group_id]

let from_flow (groupTable : GroupTable0x04.t) (priority : int) (flow : AL.flow) : Core.flowMod = 
  let open AL in
  match flow with
  | { pattern; action; cookie; idle_timeout; hard_timeout } ->
    let pat,inport = from_pattern pattern in
    let open Core in 
    { 
      mfCommand = AddFlow;
      mfOfp_match = pat;
      mfPriority = priority;
      mfInstructions = [Core.ApplyActions (from_group groupTable inport action)];
      mfCookie = Core.val_to_mask cookie;
      mfIdle_timeout = from_timeout idle_timeout;
      mfHard_timeout = from_timeout hard_timeout;
      mfTable_id = 0;
      mfFlags = {
        fmf_send_flow_rem = false; 
        fmf_check_overlap = false;
        fmf_reset_counts = false;
        fmf_no_pkt_counts = false;
        fmf_no_byt_counts = false
      };
      mfBuffer_id = None;
      mfOut_port = None;
      mfOut_group = None
    }
      
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
  { AL.switch_id = VInt.Int64 feats.datapath_id;
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
    
(* Compiler may generate code that pops vlans w/o matching for vlan
   tags. We have to enforce that pop_vlan is only called on vlan tagged
   packets 

   Similarly, we have to push a vlan tag before we set vlan if the
   packet doesn't already have a vlan.
*)
let contains_vlan_pop (act : SDN_Types.group) = 
  let vlan_pop a = match a with
    | SDN_Types.SetField (SDN_Types.Vlan, VInt.Int16 0xFFFF) -> true
    | _ -> false in
  List.exists (List.exists (List.exists vlan_pop)) act

let contains_vlan_mod (act : SDN_Types.group) = 
  let vlan_mod a = match a with
    | SDN_Types.SetField (SDN_Types.Vlan, VInt.Int16 0xFFFF) -> false
    | SDN_Types.SetField (SDN_Types.Vlan, _) -> true
    | _ -> false in
  List.exists (List.exists (List.exists vlan_mod)) act

let contains_vlan = SDN_Types.FieldMap.mem SDN_Types.Vlan

let strip_vlan_pop = List.map (List.map (List.fold_left (fun acc a -> match a with
  | SDN_Types.SetField (SDN_Types.Vlan, VInt.Int16 0xFFFF) -> acc
  | _ -> a :: acc) []))

(* I use set vlan = -1 to signal HighLevelSwitch0x04 for a push_vlan *)
let add_vlan_push = List.map (List.map (List.fold_left (fun acc a -> match a with
  | SDN_Types.SetField (SDN_Types.Vlan, _ ) -> SDN_Types.SetField (SDN_Types.Vlan, VInt.Int16 (-1)) :: a :: acc
  | _ -> a :: acc) []))

(* I assume that vlan is not both set and popped in the same rule *)
let fix_vlan_in_flow fl =
  let open SDN_Types in
  if contains_vlan_pop fl.action && not (contains_vlan fl.pattern) then
    (* match on vlan_none, then drop the strip_vlan *)
    [{fl with pattern = FieldMap.add Vlan (VInt.Int16 0xFFFF) fl.pattern;
      action = strip_vlan_pop fl.action}] @
    (* match on vlan_any, use the same actions *)
      [{fl with pattern = FieldMap.add Vlan (VInt.Int16 (-1)) fl.pattern}]
  else if contains_vlan_mod fl.action && not (contains_vlan fl.pattern) then
    (* match on vlan_none, then push a vlan tag *)
    [{fl with pattern = FieldMap.add Vlan (VInt.Int16 0xFFFF) fl.pattern;
      action = add_vlan_push fl.action}] @
    (* match on vlan_any, use the same actions *)
      [{fl with pattern = FieldMap.add Vlan (VInt.Int16 (-1)) fl.pattern}]
  else
    [fl]

let rec fix_vlan_in_table tbl = match tbl with
  | [] -> []
  | fl :: tbl -> fix_vlan_in_flow fl @ fix_vlan_in_table tbl
 
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

