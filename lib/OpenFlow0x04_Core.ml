open Packet

(** val coq_VLAN_NONE : dlVlan **)

let coq_VLAN_NONE = Some 65535

type 'a mask = { m_value : 'a; m_mask : 'a option }

type payload =
  | Buffered of int32 * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

(** val m_value : 'a1 mask -> 'a1 **)

let m_value x = x.m_value

(** val m_mask : 'a1 mask -> 'a1 option **)

let m_mask x = x.m_mask

type xid = int32
type int12 = int16

(** val val_to_mask : 'a1 -> 'a1 mask **)

let val_to_mask v =
  { m_value = v; m_mask = None }

type switchId = int64

type groupId = int32

type portId = int32

type tableId = int8

type bufferId = int32

type oxm =
| OxmInPort of portId
| OxmInPhyPort of portId
| OxmMetadata of int64 mask
| OxmEthType of int16
| OxmEthDst of int48 mask
| OxmEthSrc of int48 mask
| OxmVlanVId of int12 mask
| OxmVlanPcp of int8
| OxmIPProto of int8
| OxmIPDscp of int8
| OxmIPEcn of int8
| OxmIP4Src of int32 mask
| OxmIP4Dst of int32 mask
| OxmTCPSrc of int16 mask
| OxmTCPDst of int16 mask
| OxmARPOp of int16
| OxmARPSpa of int32 mask
| OxmARPTpa of int32 mask
| OxmARPSha of int48 mask
| OxmARPTha of int48 mask
| OxmICMPType of int8
| OxmICMPCode of int8
| OxmMPLSLabel of int32
| OxmMPLSTc of int8
| OxmTunnelId of int64 mask

type oxmMatch = oxm list

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of int16
| Any

type action =
| Output of pseudoPort
| Group of groupId
| PopVlan
| PushVlan
| PopMpls
| PushMpls
| SetField of oxm

type actionSequence = action list

type instruction =
| GotoTable of tableId
| ApplyActions of actionSequence
| WriteActions of actionSequence

type bucket = { bu_weight : int16; bu_watch_port : portId option;
                bu_watch_group : groupId option; bu_actions : actionSequence }

(** val bu_weight : bucket -> int16 **)

let bu_weight x = x.bu_weight

(** val bu_watch_port : bucket -> portId option **)

let bu_watch_port x = x.bu_watch_port

(** val bu_watch_group : bucket -> groupId option **)

let bu_watch_group x = x.bu_watch_group

(** val bu_actions : bucket -> actionSequence **)

let bu_actions x = x.bu_actions

type groupType =
| All
| Select
| Indirect
| FF

type groupMod =
| AddGroup of groupType * groupId * bucket list
| DeleteGroup of groupType * groupId

type timeout =
| Permanent
| ExpiresAfter of int16

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type flowModFlags = { fmf_send_flow_rem : bool; fmf_check_overlap : bool;
                      fmf_reset_counts : bool; fmf_no_pkt_counts : bool;
                      fmf_no_byt_counts : bool }

(** val fmf_send_flow_rem : flowModFlags -> bool **)

let fmf_send_flow_rem x = x.fmf_send_flow_rem

(** val fmf_check_overlap : flowModFlags -> bool **)

let fmf_check_overlap x = x.fmf_check_overlap

(** val fmf_reset_counts : flowModFlags -> bool **)

let fmf_reset_counts x = x.fmf_reset_counts

(** val fmf_no_pkt_counts : flowModFlags -> bool **)

let fmf_no_pkt_counts x = x.fmf_no_pkt_counts

(** val fmf_no_byt_counts : flowModFlags -> bool **)

let fmf_no_byt_counts x = x.fmf_no_byt_counts

type flowMod = { mfCookie : int64 mask; mfTable_id : tableId;
                 mfCommand : flowModCommand; mfIdle_timeout : timeout;
                 mfHard_timeout : timeout; mfPriority : int16;
                 mfBuffer_id : bufferId option;
                 mfOut_port : pseudoPort option;
                 mfOut_group : groupId option; mfFlags : flowModFlags;
                 mfOfp_match : oxmMatch; mfInstructions : instruction list }

let match_all = []

let default_fm_flags = 
  { fmf_send_flow_rem = false
  ; fmf_check_overlap = false
  ; fmf_reset_counts = false
  ; fmf_no_pkt_counts = false
  ; fmf_no_byt_counts = false }

let add_flow prio pat insts =
  { mfCookie = val_to_mask 0L
  ; mfTable_id = 0
  ; mfCommand = AddFlow
  ; mfIdle_timeout = Permanent
  ; mfHard_timeout = Permanent
  ; mfPriority = prio
  ; mfBuffer_id = None
  ; mfOut_port = None
  ; mfOut_group = None
  ; mfFlags = default_fm_flags
  ; mfOfp_match = pat
  ; mfInstructions = insts }

let delete_all_flows =
  { mfCookie = val_to_mask 0L
  ; mfTable_id = 0
  ; mfCommand = DeleteFlow
  ; mfIdle_timeout = Permanent
  ; mfHard_timeout = Permanent
  ; mfPriority = 0
  ; mfBuffer_id = None
  ; mfOut_port = None
  ; mfOut_group = None
  ; mfFlags = default_fm_flags
  ; mfOfp_match = match_all
  ; mfInstructions = [] }

(** val mfCookie : flowMod -> int64 mask **)

let mfCookie x = x.mfCookie

(** val mfTable_id : flowMod -> tableId **)

let mfTable_id x = x.mfTable_id

(** val mfCommand : flowMod -> flowModCommand **)

let mfCommand x = x.mfCommand

(** val mfIdle_timeout : flowMod -> timeout **)

let mfIdle_timeout x = x.mfIdle_timeout

(** val mfHard_timeout : flowMod -> timeout **)

let mfHard_timeout x = x.mfHard_timeout

(** val mfPriority : flowMod -> int16 **)

let mfPriority x = x.mfPriority

(** val mfBuffer_id : flowMod -> bufferId option **)

let mfBuffer_id x = x.mfBuffer_id

(** val mfOut_port : flowMod -> pseudoPort option **)

let mfOut_port x = x.mfOut_port

(** val mfOut_group : flowMod -> groupId option **)

let mfOut_group x = x.mfOut_group

(** val mfFlags : flowMod -> flowModFlags **)

let mfFlags x = x.mfFlags

(** val mfOfp_match : flowMod -> oxmMatch **)

let mfOfp_match x = x.mfOfp_match

(** val mfInstructions : flowMod -> instruction list **)

let mfInstructions x = x.mfInstructions

type packetInReason =
| NoMatch
| ExplicitSend

type packetIn = { pi_total_len : int16; pi_reason : packetInReason; 
		  pi_table_id : tableId; pi_cookie : int64;
		  pi_ofp_match : oxmMatch; pi_payload : payload 
		}

(** val pi_total_len : packetIn -> int16 **)

let pi_total_len x = x.pi_total_len

(** val pi_reason : packetIn -> packetInReason **)

let pi_reason x = x.pi_reason

(** val pi_table_id : packetIn -> tableId **)

let pi_table_id x = x.pi_table_id

(** val pi_cookie : packetIn -> int64 **)

let pi_cookie x = x.pi_cookie

(** val pi_ofp_match : packetIn -> oxmMatch **)

let pi_ofp_match x = x.pi_ofp_match

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; ip_reasm : 
                      bool; queue_stats : bool; port_blocked : bool }

type portState = { link_down : bool; blocked : bool; live : bool }

type portDesc = { port_no : portId;
		  (* hw_addr : int48; *)
		  (* name; *)
		  (* config; *)
		  state : portState
		  (* curr; *)
		  (* advertised; *)
		  (* supported; *)
		  (* peer; *)
		  (* curr_speed; *)
		  (* max_speed *) }

type portReason =
  | PortAdd 
  | PortDelete
  | PortModify

type portStatus = { reason : portReason; desc : portDesc }

(** val flow_stats : capabilities -> bool **)

let flow_stats x = x.flow_stats

(** val table_stats : capabilities -> bool **)

let table_stats x = x.table_stats

(** val port_stats : capabilities -> bool **)

let port_stats x = x.port_stats

(** val group_stats : capabilities -> bool **)

let group_stats x = x.group_stats

(** val ip_reasm : capabilities -> bool **)

let ip_reasm x = x.ip_reasm

(** val queue_stats : capabilities -> bool **)

let queue_stats x = x.queue_stats

(** val port_blocked : capabilities -> bool **)

let port_blocked x = x.port_blocked

type features = { datapath_id : int64; num_buffers : int32;
                  num_tables : int8; aux_id : int8;
                  supported_capabilities : capabilities }

(** val datapath_id : features -> int64 **)

let datapath_id x = x.datapath_id

(** val num_buffers : features -> int32 **)

let num_buffers x = x.num_buffers

(** val num_tables : features -> int8 **)

let num_tables x = x.num_tables

(** val aux_id : features -> int8 **)

let aux_id x = x.aux_id

(** val supported_capabilities : features -> capabilities **)

let supported_capabilities x = x.supported_capabilities

type packetOut = { po_in_port : pseudoPort;
                   po_actions : actionSequence; po_payload : payload }


(** val po_in_port : packetOut -> pseudoPort **)

let po_in_port x = x.po_in_port

(** val po_actions : packetOut -> actionSequence **)

let po_actions x = x.po_actions

type multipartRequest = 
  | SwitchDescReq
  | PortsDescReq 

type multipartReply = 
  | PortsDescReply of portDesc list

type message =
| Hello
| EchoRequest of bytes
| EchoReply of bytes
| FeaturesRequest
| FeaturesReply of features
| FlowModMsg of flowMod
| GroupModMsg of groupMod
| PacketInMsg of packetIn
| PacketOutMsg of packetOut
| PortStatusMsg of portStatus
| MultipartReq of multipartRequest
| MultipartReply of multipartReply
| BarrierRequest
| BarrierReply

let portsDescRequest = MultipartReq PortsDescReq
