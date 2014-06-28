open Packet

type 'a mask = { m_value : 'a; m_mask : 'a option }

type payload =
  | Buffered of int32 * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

type xid = OpenFlow_Header.xid
type int12 = int16

val val_to_mask : 'a1 -> 'a1 mask

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

val match_all : oxmMatch

(** A pseudo-port, as described by the [ofp_port_no] enumeration in
    Section A.2.1 of the OpenFlow 1.3.0 specification. *)
type pseudoPort =
  | PhysicalPort of portId
  | InPort            (** Send the packet out the input port. This reserved port
                          must be explicitly used in order to send back out of
                          the input port. *)
  | Table             (** Submit the packet to the first flow table NB: This
                          destination port can only be used in packet-out
                          messages. *)
  | Normal            (** Process with normal L2/L3 switching. *)
  | Flood             (** All physical ports in VLAN, except input port and
                          those blocked or link down. *)
  | AllPorts          (** All physical ports except input port. *)
  | Controller of int16 (** Send to controller along with [n] (max 1024) bytes
                            of the packet *)
  | Local             (** Local openflow "port". *)
  | Any               (** Wildcard port used only for flow mod (delete) and flow
                          stats requests. Selects all flows regardless of output
                          port (including flows with no output port). *)

type actionTyp = 
 | Output
 | CopyTTLOut
 | CopyTTLIn
 | SetMPLSTTL
 | DecMPLSTTL
 | PushVLAN
 | PopVLAN
 | PushMPLS
 | PopMPLS
 | SetQueue
 | Group
 | SetNWTTL
 | DecNWTTL
 | SetField
 | PushPBB
 | PopPBB
 | Experimenter
 
type action =
| Output of pseudoPort
| Group of groupId
| PopVlan
| PushVlan
| PopMpls
| PushMpls
| SetField of oxm
| CopyTtlOut
| CopyTtlIn
| SetNwTtl of int8
| DecNwTtl
| PushPbb
| PopPbb
| SetMplsTtl of int8
| DecMplsTtl
| SetQueue of int32
| Experimenter of int32


type actionSequence = action list

type instruction =
| GotoTable of tableId
| ApplyActions of actionSequence
| WriteActions of actionSequence
| WriteMetadata of int64 mask
| Clear
| Meter of int32
| Experimenter of int32

type bucket = { bu_weight : int16; bu_watch_port : portId option;
                bu_watch_group : groupId option; bu_actions : actionSequence }

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

type flowMod = { mfCookie : int64 mask; mfTable_id : tableId;
                 mfCommand : flowModCommand; mfIdle_timeout : timeout;
                 mfHard_timeout : timeout; mfPriority : int16;
                 mfBuffer_id : bufferId option;
                 mfOut_port : pseudoPort option;
                 mfOut_group : groupId option; mfFlags : flowModFlags;
                 mfOfp_match : oxmMatch; mfInstructions : instruction list }

val add_flow : int16 -> oxmMatch -> instruction list -> flowMod

val delete_all_flows : flowMod

type packetInReason =
| NoMatch
| ExplicitSend
| InvalidTTL

type packetIn = { pi_total_len : int16;
                  pi_reason : packetInReason; pi_table_id : tableId;
                  pi_cookie : int64; pi_ofp_match : oxmMatch;
                  pi_payload : payload }

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; ip_reasm : 
                      bool; queue_stats : bool; port_blocked : bool }

type portState = { link_down : bool; blocked : bool; live : bool }

type portConfig = { port_down : bool; no_recv : bool; no_fwd : bool;
                    no_packet_in : bool }

type portFeatures = { rate_10mb_hd : bool; rate_10mb_fd : bool; 
                      rate_100mb_hd : bool; rate_100mb_fd : bool;
                      rate_1gb_hd : bool; rate_1gb_fd : bool;
                      rate_10gb_fd : bool; rate_40gb_fd : bool;
                      rate_100gb_fd : bool; rate_1tb_fd : bool;
                      other : bool; copper : bool; fiber : bool;
                      autoneg : bool; pause : bool; pause_asym : bool }     


type portDesc = { port_no : portId; config : portConfig; state : portState;
                  curr : portFeatures; advertised : portFeatures; supported :
                  portFeatures; peer : portFeatures }

type portReason =
  | PortAdd
  | PortDelete
  | PortModify

type portStatus = { reason : portReason; desc : portDesc }

type packetOut = {
  po_payload : payload;
  po_port_id : portId option;
  po_actions : actionSequence
}

type flowRequest = {fr_table_id : tableId; fr_out_port : portId; 
                    fr_out_group : portId; fr_cookie : int64 mask;
                    fr_match : oxmMatch}

type queueRequest = {port_number : portId; queue_id : int32}

type experimenter = {exp_id : int32; exp_type : int32}

type tableFeatureProp =
  | TfpInstruction of instruction list 
  | TfpInstructionMiss of instruction list
  | TfpNextTable of tableId list
  | TfpNextTableMiss of tableId list
  | TfpWriteAction of action list
  | TfpWriteActionMiss of action list
  | TfpApplyAction of action list
  | TfpApplyActionMiss of action list
  | TfpMatch of oxm list
  | TfpWildcard of oxm list
  | TfpWriteSetField of oxm list
  | TfpWriteSetFieldMiss of oxm list
  | TfpApplySetField of oxm list
  | TfpApplySetFieldMiss of oxm list
  | TfpExperimenter of (experimenter*bytes)
  | TfpExperimenterMiss of (experimenter*bytes)

type tableConfig = Deprecated

type tableFeatures = {length : int16; table_id : tableId; name : string;
                      metadata_match : int64; metadata_write : int64;
                      config : tableConfig; max_entries: int32;
                      feature_prop : tableFeatureProp}

type tableFeaturesRequest = tableFeatures list

type multipartType =
  | SwitchDescReq
  | PortsDescReq 
  | FlowStatsReq of flowRequest
  | AggregFlowStatsReq of flowRequest
  | TableStatsReq
  | PortStatsReq of portId
  | QueueStatsReq of queueRequest
  | GroupStatsReq of int32
  | GroupDescReq
  | GroupFeatReq
  | MeterStatsReq of int32
  | MeterConfReq of int32
  | MeterFeatReq
  | TableFeatReq of tableFeaturesRequest option
  | ExperimentReq of experimenter  

type multipartRequest = { mpr_type : multipartType; mpr_flags : bool }


val portDescReq : multipartRequest

type switchDesc = { mfr_desc :string ; hw_desc : string; sw_desc : string;
                         serial_num : string }

type flowStats = { table_id : tableId; duration_sec : int32; duration_nsec : 
                   int32; priority : int16; idle_timeout : timeout; 
                   hard_timeout : timeout; flags : flowModFlags; cookie : int64;
                   packet_count : int64; byte_count : int64; ofp_match : oxmMatch;
                   instructions : instruction list}

type aggregStats = { packet_count : int64; byte_count : int64; flow_count : int32}

type tableStats = { table_id : tableId; active_count : int32; lookup_count : int64;
                    matched_count : int64}

type portStats = { psPort_no : portId; rx_packets : int64; tx_packets : int64; 
                   rx_bytes : int64; tx_bytes : int64; rx_dropped : int64; 
                   tx_dropped : int64; rx_errors : int64; tx_errors : int64;
                   rx_frame_err : int64; rx_over_err : int64; rx_crc_err : int64;
                   collisions : int64; duration_sec : int32; duration_nsec : int32}

type queueStats = { qsPort_no : portId; queue_id : int32; tx_bytes : int64; tx_packets : int64;
                    tx_errors : int64; duration_sec : int32; duration_nsec : int32 }

type bucketStats = { packet_count : int64; byte_count : int64}

type groupStats = { length : int16; group_id : int32; ref_count : int32;
                    packet_count : int64; byte_count : int64; duration_sec : int32;
                    duration_nsec : int32; bucket_stats : bucketStats list}

type groupDesc = { length : int16; typ : groupType; group_id : int32; bucket : bucket list}

type groupCapabilities = { select_weight : bool; select_liveness : bool;
                           chaining : bool; chaining_checks : bool}

type groupTypeMap = { all : bool; select : bool; indirect : bool; ff : bool}

type actionTypeMap = { output : bool; copy_ttl_out : bool; copy_ttl_in : bool;
                       set_mpls_ttl : bool; dec_mpls_ttl : bool; push_vlan : bool;
                       pop_vlan : bool; push_mpls : bool; pop_mpls : bool; set_queue : bool;
                       group : bool; set_nw_ttl : bool; dec_nw_ttl : bool; set_field : bool;
                       push_pbb : bool; pop_pbb : bool }

type groupFeatures = { typ : groupTypeMap; capabilities : groupCapabilities; 
                       max_groups_all : int32; max_groups_select : int32; 
                       max_groups_indirect : int32; max_groups_ff : int32;
                       actions_all : actionTypeMap; actions_select : actionTypeMap; 
                       actions_indirect : actionTypeMap; actions_ff : actionTypeMap }
                       

type multipartReplyTyp = 
  | PortsDescReply of portDesc list
  | SwitchDescReply of switchDesc
  | FlowStatsReply of flowStats list
  | AggregateReply of aggregStats
  | TableReply of tableStats list
  | PortStatsReply of portStats list
  | QueueStatsReply of queueStats list
  | GroupStatsReply of groupStats list

type multipartReply = {mpreply_typ : multipartReplyTyp; mpreply_flags : bool}

type tableMod = { table_id : tableId; config : tableConfig }
 
