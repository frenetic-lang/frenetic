open Packet

type 'a mask = { m_value : 'a; m_mask : 'a option }

type 'a asyncMask = { m_master : 'a ; m_slave : 'a }

type payload =
  | Buffered of int32 * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

type xid = OpenFlow_Header.xid
type int12 = int16
type int24 = int32
type int128 = int64 * int64

let val_to_mask v =
  { m_value = v; m_mask = None }

let ip_to_mask (p,m) =
  if m = 32l then { m_value = p; m_mask = None }
  else { m_value = p; m_mask = Some m }

type switchId = int64

type groupId = int32

type portId = int32

type tableId = int8

type bufferId = int32

type helloFailed = 
 | HelloIncompatible
 | HelloPermError

type badRequest = 
 | ReqBadVersion
 | ReqBadType
 | ReqBadMultipart
 | ReqBadExp
 | ReqBadExpType
 | ReqPermError
 | ReqBadLen
 | ReqBufferEmpty
 | ReqBufferUnknown
 | ReqBadTableId
 | ReqIsSlave
 | ReqBadPort
 | ReqBadPacket
 | ReqMultipartBufOverflow

type badAction = 
 | ActBadType
 | ActBadLen
 | ActBadExp
 | ActBadExpType
 | ActBadOutPort
 | ActBadArg
 | ActPermError
 | ActTooMany
 | ActBadQueue
 | ActBadOutGroup
 | ActMatchInconsistent
 | ActUnsupportedOrder
 | ActBadTag
 | ActBadSetTyp
 | ActBadSetLen
 | ActBadSetArg

type badInstruction =
 | InstUnknownInst
 | InstBadTableId
 | InstUnsupInst
 | InstUnsupMeta
 | InstUnsupMetaMask
 | InstBadExp
 | InstBadExpTyp
 | InstBadLen
 | InstPermError

type badMatch = 
 | MatBadTyp
 | MatBadLen
 | MatBadTag
 | MatBadDlAddrMask
 | MatBadNwAddrMask
 | MatBadWildcards
 | MatBadField
 | MatBadValue
 | MatBadMask
 | MatBadPrereq
 | MatDupField
 | MatPermError

type flowModFailed =
 | FlUnknown
 | FlTableFull
 | FlBadTableId
 | FlOverlap
 | FlPermError
 | FlBadTimeout
 | FlBadCommand
 | FlBadFlags

type groupModFailed =
 | GrGroupExists
 | GrInvalidGroup
 | GrWeightUnsupported
 | GrOutOfGroups
 | GrOutOfBuckets
 | GrChainingUnsupported
 | GrWatcHUnsupported
 | GrLoop
 | GrUnknownGroup
 | GrChainedGroup
 | GrBadTyp
 | GrBadCommand
 | GrBadBucket
 | GrBadWatch
 | GrPermError
 
type portModFailed =
 | PoBadPort
 | PoBadHwAddr
 | PoBadConfig
 | PoBadAdvertise
 | PoPermError

type tableModFailed =
 | TaBadTable
 | TaBadConfig
 | TaPermError

type queueOpFailed =
 | QuBadPort
 | QuBadQUeue
 | QuPermError

type switchConfigFailed =
 | ScBadFlags
 | ScBadLen
 | ScPermError

type roleReqFailed = 
 | RoStale
 | RoUnsup
 | RoBadRole

type meterModFailed = 
 | MeUnknown
 | MeMeterExists
 | MeInvalidMeter
 | MeUnknownMeter
 | MeBadCommand
 | MeBadFlags
 | MeBadRate
 | MeBadBurst
 | MeBadBand
 | MeBadBandValue
 | MeOutOfMeters
 | MeOutOfBands

type tableFeatFailed =
 | TfBadTable
 | TfBadMeta
 | TfBadType
 | TfBadLen
 | TfBadArg
 | TfPermError

type experimenterFailed = { exp_typ : int16; exp_id : int32}

type errorTyp = 
 | HelloFailed of helloFailed
 | BadRequest of badRequest
 | BadAction of badAction
 | BadInstruction of badInstruction
 | BadMatch of badMatch
 | FlowModFailed of flowModFailed
 | GroupModFailed of groupModFailed
 | PortModFailed of portModFailed
 | TableModFailed of tableModFailed
 | QueueOpFailed of queueOpFailed
 | SwitchConfigFailed of switchConfigFailed
 | RoleReqFailed of roleReqFailed
 | MeterModFailed of meterModFailed
 | TableFeatFailed of tableFeatFailed
 | ExperimenterFailed of experimenterFailed

type length = int16

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
| OxmTCPSrc of int16
| OxmTCPDst of int16
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
| OxmUDPSrc of int16
| OxmUDPDst of int16
| OxmSCTPSrc of int16
| OxmSCTPDst of int16
| OxmIPv6Src of int128 mask
| OxmIPv6Dst of int128 mask
| OxmIPv6FLabel of int32 mask
| OxmICMPv6Type of int8
| OxmICMPv6Code of int8
| OxmIPv6NDTarget of int128 mask
| OxmIPv6NDSll of int48
| OxmIPv6NDTll of int48
| OxmMPLSBos of int8
| OxmPBBIsid of int24 mask
| OxmIPv6ExtHdr of int16 mask

type oxmMatch = oxm list

type pseudoPort =
  | PhysicalPort of portId
  | InPort
  | Table
  | Normal
  | Flood
  | AllPorts
  | Controller of int16
  | Local
  | Any

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
| ModifyGroup of groupType * groupId * bucket list

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
  ; mfTable_id = 0xff (* OFPTT_ALL *)
  ; mfCommand = DeleteFlow
  ; mfIdle_timeout = Permanent
  ; mfHard_timeout = Permanent
  ; mfPriority = 0
  ; mfBuffer_id = None
  ; mfOut_port = None
  ; mfOut_group = Some 0xffffffffl (* OFPG_ANY *)
  ; mfFlags = default_fm_flags
  ; mfOfp_match = match_all
  ; mfInstructions = [] }

type packetInReason =
| NoMatch
| ExplicitSend
| InvalidTTL

type packetIn = { pi_total_len : int16; pi_reason : packetInReason; 
                  pi_table_id : tableId; pi_cookie : int64;
                  pi_ofp_match : oxmMatch; pi_payload : payload }

type flowReason = 
  | FlowIdleTimeout
  | FlowHardTiemout
  | FlowDelete
  | FlowGroupDelete

type flowRemoved = { cookie : int64; priority : int16; reason : flowReason;
                     table_id : tableId; duration_sec : int32; duration_nsec : int32;
                     idle_timeout : timeout; hard_timeout : timeout; packet_count : int64;
                     byte_count : int64; oxm : oxmMatch }

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

type portDesc = { port_no : portId;
                  hw_addr : int48;
                  name : string;
                  config : portConfig;
                  state : portState;
                  curr : portFeatures;
                  advertised : portFeatures;
                  supported : portFeatures; 
                  peer : portFeatures;
                  curr_speed : int32;
                  max_speed : int32}

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

type rate = int32

type burst = int32

type experimenterId = int32

type meterBand =
  | Drop of (rate*burst)
  | DscpRemark of (rate*burst*int8)
  | ExpMeter of (rate*burst*experimenterId)

type meterFlags = { kbps : bool; pktps : bool; burst : bool; stats : bool}

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

type tableFeatures = {length : int16;table_id : tableId; name : string;
                      metadata_match : int64; metadata_write : int64;
                      config : tableConfig; max_entries: int32;
                      feature_prop : tableFeatureProp}

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
  | TableFeatReq of (tableFeatures list) option
  | ExperimentReq of experimenter  

type multipartRequest = { mpr_type : multipartType; mpr_flags : bool }

let portDescReq = 
  { mpr_type = PortsDescReq
  ; mpr_flags = false }

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
                           chaining : bool; chaining_checks : bool }

type groupTypeMap = { all : bool; select : bool; indirect : bool; ff : bool}

type actionTypeMap = { output : bool; copy_ttl_out : bool; copy_ttl_in : bool;
                       set_mpls_ttl : bool; dec_mpls_ttl : bool; push_vlan : bool;
                       pop_vlan : bool; push_mpls : bool; pop_mpls : bool; set_queue : bool;
                       group : bool; set_nw_ttl : bool; dec_nw_ttl : bool; set_field : bool;
                       push_pbb : bool; pop_pbb : bool }

type groupFeatures = { typ : groupTypeMap; capabilities : groupCapabilities; 
                       max_groups_all : int32; max_groups_select : int32; 
                       max_groups_indirect : int32; max_groups_ff : 
                       int32; actions_all : actionTypeMap; actions_select : actionTypeMap; 
                       actions_indirect : actionTypeMap; actions_ff : actionTypeMap }

type meterBandStats = { packet_band_count : int64; byte_band_count : int64 }

type meterStats = { meter_id: int32; len : int16; flow_count : int32; packet_in_count :
                    int64; byte_in_count : int64; duration_sec : int32; duration_nsec : 
                    int32; band : meterBandStats list}

type meterConfig = { length : length; flags : meterFlags; meter_id : int32; bands : meterBand list}

type meterBandMaps = { drop : bool; dscpRemark : bool}

type meterFeaturesStats = { max_meter : int32; band_typ : meterBandMaps; 
                            capabilities : meterFlags; max_band : int8;
                            max_color : int8 }

type multipartReplyTyp = 
  | PortsDescReply of portDesc list
  | SwitchDescReply of switchDesc
  | FlowStatsReply of flowStats list
  | AggregateReply of aggregStats
  | TableReply of tableStats list
  | TableFeaturesReply of tableFeatures list
  | PortStatsReply of portStats list
  | QueueStatsReply of queueStats list
  | GroupStatsReply of groupStats list
  | GroupDescReply of groupDesc list
  | GroupFeaturesReply of groupFeatures
  | MeterReply of meterStats list
  | MeterConfig of meterConfig list
  | MeterFeaturesReply of meterFeaturesStats

type multipartReply = {mpreply_typ : multipartReplyTyp; mpreply_flags : bool}

type tableMod = { table_id : tableId; config : tableConfig }
 
type supportedList = int list
 
type element = 
  | VersionBitMap of supportedList

type helloElement = element list

type asyncConfig = { packet_in : packetInReason asyncMask; 
                     port_status : portReason asyncMask;
                     flow_removed : flowReason asyncMask }

