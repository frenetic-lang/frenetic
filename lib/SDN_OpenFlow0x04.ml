module AL = SDN_Types
module Core = OpenFlow0x04_Core
module Msg = OpenFlow0x04.Message


exception Invalid_port of int32

let from_portId (pport_id : AL.portId) : Core.portId =
  if pport_id > 0xffffff00l then (* pport_id <= OFPP_MAX *)
    raise (Invalid_port pport_id)
  else
    pport_id

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
    | InvalidTTL -> AL.InvalidTTL
      
let to_packetIn (pktIn : Core.packetIn) : AL.pktIn =
  let open Core in
  let get_port = 
    List.fold_left (fun acc x -> match x with
      | OxmInPort p -> p
      | _ -> acc) Int32.zero
  in
  match pktIn with
    | { pi_payload; pi_total_len; pi_ofp_match; pi_reason } ->
      (to_payload pi_payload, pi_total_len, get_port pi_ofp_match, to_reason pi_reason)

let from_pattern (pat : AL.pattern) : Core.oxmMatch * Core.portId option = 
  let v_to_m = Core.val_to_mask in
  (Core_kernel.Core_list.filter_opt
    [ Misc.map_option (fun x -> Core.OxmEthSrc (v_to_m x)) pat.AL.dlSrc
    ; Misc.map_option (fun x -> Core.OxmEthDst (v_to_m x)) pat.AL.dlDst
    ; Misc.map_option (fun x -> Core.OxmEthType x) pat.AL.dlTyp
    ; Misc.map_option (fun x -> match x with
        | -1 ->
          Core.OxmVlanVId { Core.m_value = 0x1000; Core.m_mask = Some 0x1000 }
        | 0xffff ->
          Core.OxmVlanVId (v_to_m 0)
        | _ ->
          Core.OxmVlanVId (v_to_m x))
      pat.AL.dlVlan
    ; Misc.map_option (fun x -> Core.OxmVlanPcp x) pat.AL.dlVlanPcp
    ; Misc.map_option (fun x -> Core.OxmIP4Src (v_to_m x)) pat.AL.nwSrc
    ; Misc.map_option (fun x -> Core.OxmIP4Dst (v_to_m x)) pat.AL.nwDst
    ; Misc.map_option (fun x -> Core.OxmIPProto x) pat.AL.nwProto
    ; Misc.map_option (fun x -> Core.OxmTCPSrc (v_to_m x)) pat.AL.tpSrc
    ; Misc.map_option (fun x -> Core.OxmTCPDst (v_to_m x)) pat.AL.tpDst
    ; Misc.map_option (fun x -> Core.OxmInPort x) pat.AL.inPort
    ], pat.AL.inPort)

let from_timeout (timeout : AL.timeout) : Core.timeout =
  match timeout with
    | AL.Permanent -> Core.Permanent
    | AL.ExpiresAfter n -> Core.ExpiresAfter n

module Common = HighLevelSwitch_common.Make (struct
  type of_action = Core.action
  type of_portId = Core.portId

  module Mod = ModComposition

  let from_output (inPort : Core.portId option) (pseudoport : AL.pseudoport) =
    let open OpenFlow0x04_Core in
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

  let from_action (inPort : Core.portId option) (act : AL.action)
    : Mod.t * Core.action =
    let v_to_m = Core.val_to_mask in
    let open Core in
    match act with
      | AL.Output pseudoport ->
        from_output inPort pseudoport
      | AL.Enqueue(_, _) ->
        raise (Invalid_argument "cannot enqueue")
      | AL.Modify (AL.SetEthSrc n) ->
        (Mod.dlSrc, Core.SetField (OxmEthSrc (v_to_m n)))
      | AL.Modify (AL.SetEthDst n) ->
        (Mod.dlDst , Core.SetField (OxmEthDst (v_to_m n)))
      | AL.Modify (AL.SetVlan vlan) ->
        begin match vlan with
          | None
          | Some(0xffff) ->
            (Mod.dlVlan, Core.PopVlan)
          | Some(-1) ->
            (Mod.dlVlan, Core.PushVlan)
          | Some(n) ->
            let n = VInt.(get_int12 (Int16 n)) in
            let vlan_id = v_to_m (n lor 0x1000) (*OFPVID_PRESENT*) in
            (Mod.dlVlan, Core.SetField (OxmVlanVId vlan_id))
        end
      | AL.Modify (AL.SetVlanPcp pcp) ->
        let pcp = VInt.(get_int4 (Int4 pcp)) in
        (Mod.dlVlanPcp, Core.SetField (OxmVlanPcp pcp))
      (* MJR: This seems silly. OF 1.3 has no such restriction *)
      | AL.Modify (AL.SetEthTyp _) -> raise (Invalid_argument "cannot set Ethernet type")
      | AL.Modify (AL.SetIPProto _) -> raise (Invalid_argument "cannot set IP protocol")
      | AL.Modify (AL.SetIP4Src n) -> (Mod.nwSrc, Core.SetField (OxmIP4Src (v_to_m n)))
      | AL.Modify (AL.SetIP4Dst n) -> (Mod.nwDst, Core.SetField (OxmIP4Dst (v_to_m n)))
      | AL.Modify (AL.SetTCPSrcPort n) -> (Mod.tpSrc, Core.SetField (OxmTCPSrc (v_to_m n)))
      | AL.Modify (AL.SetTCPDstPort n) -> (Mod.tpDst, Core.SetField (OxmTCPDst (v_to_m n)))
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
  

let from_group (inPort : Core.portId option) (groupTable : GroupTable0x04.t)
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
      mfInstructions = [Core.ApplyActions (from_group inport groupTable action)];
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

let from_packetOut (pktOut : AL.pktOut) : Core.packetOut =
  let open Core in
  let po_payload, po_port_id, po_actions = pktOut in
  let po_payload = from_payload po_payload in
  let po_port_id = Core_kernel.Option.map po_port_id from_portId in
  let po_actions = Common.flatten_par po_port_id [po_actions] in
  { po_payload; po_port_id; po_actions }

(* Compiler may generate code that pops vlans w/o matching for vlan
   tags. We have to enforce that pop_vlan is only called on vlan tagged
   packets 

   Similarly, we have to push a vlan tag before we set vlan if the
   packet doesn't already have a vlan.
*)
let contains_vlan_pop (act : SDN_Types.group) = 
  let vlan_pop a = match a with
    | AL.Modify (AL.SetVlan None)
    | AL.Modify (AL.SetVlan (Some 0xFFFF)) -> true
    | _ -> false in
  List.exists (List.exists (List.exists vlan_pop)) act

let contains_vlan_mod (act : SDN_Types.group) = 
  let vlan_mod a = match a with
    | AL.Modify (AL.SetVlan None)
    | AL.Modify (AL.SetVlan (Some 0xFFFF)) -> false
    | AL.Modify (AL.SetVlan (Some _)) -> true
    | _ -> false in
  List.exists (List.exists (List.exists vlan_mod)) act

let contains_vlan (pat:AL.pattern) =
  match pat.AL.dlVlan with
    | None -> false
    | Some _ -> true

let strip_vlan_pop = List.map (List.map (List.fold_left (fun acc a -> match a with
  | AL.Modify (AL.SetVlan None)
  | AL.Modify (AL.SetVlan (Some 0xFFFF)) -> acc
  | _ -> a :: acc) []))

(* I use set vlan = -1 to signal HighLevelSwitch0x04 for a push_vlan *)
let add_vlan_push = List.map (List.map (List.fold_left (fun acc a -> match a with
  | AL.Modify (AL.SetVlan _) -> AL.Modify (AL.SetVlan (Some (-1))) :: a :: acc
  | _ -> a :: acc) []))

(* I assume that vlan is not both set and popped in the same rule *)
let fix_vlan_in_flow fl =
  let open SDN_Types in
  if contains_vlan_pop fl.action && not (contains_vlan fl.pattern) then
    (* match on vlan_none, then drop the strip_vlan *)
    [ {fl with pattern = { fl.pattern with dlVlan = Some(0xffff) };
              action = strip_vlan_pop fl.action}
    (* match on vlan_any, use the same actions *)
    ; {fl with pattern = { fl.pattern with dlVlan = Some(-1) }}]
  else if contains_vlan_mod fl.action && not (contains_vlan fl.pattern) then
    (* match on vlan_none, then push a vlan tag *)
    [ {fl with pattern = { fl.pattern with dlVlan = Some(0xffff) };
               action = add_vlan_push fl.action}
    (* match on vlan_any, use the same actions *)
    ; {fl with pattern = { fl.pattern with dlVlan = Some(-1) }} ]
  else
    [fl]

let rec fix_vlan_in_table tbl = match tbl with
  | [] -> []
  | fl :: tbl -> fix_vlan_in_flow fl @ fix_vlan_in_table tbl
