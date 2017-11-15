open Packet

type 'a mask = { m_value : 'a; m_mask : 'a option } [@@deriving sexp]

type 'a asyncMask = { m_master : 'a ; m_slave : 'a } [@@deriving sexp]

type payload =
  | Buffered of int32 * Cstruct.t
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of Cstruct.t
  [@@deriving sexp]

type xid = OpenFlow_Header.xid [@@deriving sexp]
type int12 = int16 [@@deriving sexp]
type int24 = int32 [@@deriving sexp]
type int128 = int64 * int64 [@@deriving sexp]

val val_to_mask : 'a1 -> 'a1 mask

val ip_to_mask : (nwAddr * int32) -> nwAddr mask

type switchId = int64 [@@deriving sexp]

type groupId = int32 [@@deriving sexp]

type portId = int32 [@@deriving sexp]

type tableId = int8 [@@deriving sexp]

type bufferId = int32 [@@deriving sexp]

type switchFlags = { frag_normal: bool; frag_drop: bool; frag_reasm: bool; } [@@deriving sexp]

type switchConfig = {flags : switchFlags; miss_send_len : int16 } [@@deriving sexp]

type helloFailed =
 | HelloIncompatible
 | HelloPermError
 [@@deriving sexp]

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
 [@@deriving sexp]

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
 [@@deriving sexp]

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
 [@@deriving sexp]

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
 [@@deriving sexp]

type flowModFailed =
 | FlUnknown
 | FlTableFull
 | FlBadTableId
 | FlOverlap
 | FlPermError
 | FlBadTimeout
 | FlBadCommand
 | FlBadFlags
 [@@deriving sexp]

type groupModFailed =
 | GrGroupExists
 | GrInvalidGroup
 | GrWeightUnsupported
 | GrOutOfGroups
 | GrOutOfBuckets
 | GrChainingUnsupported
 | GrWatchUnsupported
 | GrLoop
 | GrUnknownGroup
 | GrChainedGroup
 | GrBadTyp
 | GrBadCommand
 | GrBadBucket
 | GrBadWatch
 | GrPermError
 [@@deriving sexp]

type portModFailed =
 | PoBadPort
 | PoBadHwAddr
 | PoBadConfig
 | PoBadAdvertise
 | PoPermError
 [@@deriving sexp]

type tableModFailed =
 | TaBadTable
 | TaBadConfig
 | TaPermError
 [@@deriving sexp]

type queueOpFailed =
 | QuBadPort
 | QuBadQueue
 | QuPermError
 [@@deriving sexp]

type switchConfigFailed =
 | ScBadFlags
 | ScBadLen
 | ScPermError
 [@@deriving sexp]

type roleReqFailed =
 | RoStale
 | RoUnsup
 | RoBadRole
 [@@deriving sexp]

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
 [@@deriving sexp]

type tableFeatFailed =
 | TfBadTable
 | TfBadMeta
 | TfBadType
 | TfBadLen
 | TfBadArg
 | TfPermError
 [@@deriving sexp]

type experimenterFailed = { exp_typ : int16; exp_id : int32} [@@deriving sexp]

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
 [@@deriving sexp]

type length = int16 [@@deriving sexp]

type oxmIPv6ExtHdr = { noext : bool; esp : bool; auth : bool; dest : bool; frac : bool;
                       router : bool; hop : bool; unrep : bool; unseq : bool } [@@deriving sexp]

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
| OxmMPLSBos of bool
| OxmPBBIsid of int24 mask
| OxmIPv6ExtHdr of oxmIPv6ExtHdr mask
[@@deriving sexp]

type oxmMatch = oxm list [@@deriving sexp]

(** {2 Convenient Functions} *)

val parse_payload : payload -> Packet.packet

(** [marshal_payload buf pkt] serializes pkt, where [buf] is an optional
buffer ID. *)
val marshal_payload : int32 option -> Packet.packet -> payload

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
  [@@deriving sexp]

type actionHdr =
| OutputHdr
| GroupHdr
| PopVlanHdr
| PushVlanHdr
| PopMplsHdr
| PushMplsHdr
| SetFieldHdr
| CopyTtlOutHdr
| CopyTtlInHdr
| SetNwTtlHdr
| DecNwTtlHdr
| PushPbbHdr
| PopPbbHdr
| SetMplsTtlHdr
| DecMplsTtlHdr
| SetQueueHdr
| ExperimenterAHdr of int32
[@@deriving sexp]

type action =
| Output of pseudoPort
| Group of groupId
| PopVlan
| PushVlan of int16
| PopMpls of int16
| PushMpls of int16
| SetField of oxm
| CopyTtlOut
| CopyTtlIn
| SetNwTtl of int8
| DecNwTtl
| PushPbb of int16
| PopPbb
| SetMplsTtl of int8
| DecMplsTtl
| SetQueue of int32
| Experimenter of int32
[@@deriving sexp]


type actionSequence = action list [@@deriving sexp]

type instructionHdr =
 | GotoTableHdr
 | ApplyActionsHdr
 | WriteActionsHdr
 | WriteMetadataHdr
 | ClearHdr
 | MeterHdr
 | ExperimenterHdr of int32
 [@@deriving sexp]

type instruction =
| GotoTable of tableId
| ApplyActions of actionSequence
| WriteActions of actionSequence
| WriteMetadata of int64 mask
| Clear
| Meter of int32
| Experimenter of int32
[@@deriving sexp]

type bucket = { bu_weight : int16; bu_watch_port : portId option;
                bu_watch_group : groupId option; bu_actions : actionSequence }
  [@@deriving sexp]

type groupType =
| All
| Select
| Indirect
| FF
[@@deriving sexp]

type groupMod =
| AddGroup of groupType * groupId * bucket list
| DeleteGroup of groupType * groupId
| ModifyGroup of groupType * groupId * bucket list
[@@deriving sexp]

type timeout =
| Permanent
| ExpiresAfter of int16
[@@deriving sexp]

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow
[@@deriving sexp]

type flowModFlags = { fmf_send_flow_rem : bool; fmf_check_overlap : bool;
                      fmf_reset_counts : bool; fmf_no_pkt_counts : bool;
                      fmf_no_byt_counts : bool } [@@deriving sexp]

type flowMod = { mfCookie : int64 mask; mfTable_id : tableId;
                 mfCommand : flowModCommand; mfIdle_timeout : timeout;
                 mfHard_timeout : timeout; mfPriority : int16;
                 mfBuffer_id : bufferId option;
                 mfOut_port : pseudoPort option;
                 mfOut_group : groupId option; mfFlags : flowModFlags;
                 mfOfp_match : oxmMatch; mfInstructions : instruction list } [@@deriving sexp]

val add_flow : tbl:tableId -> prio:int16 -> pat:oxmMatch -> insts:instruction list -> flowMod

val delete_all_flows : flowMod

val delete_all_groups : groupMod

type packetInReason =
| NoMatch
| ExplicitSend
| InvalidTTL
[@@deriving sexp]

type packetIn = { pi_total_len : int16;
                  pi_reason : packetInReason; pi_table_id : tableId;
                  pi_cookie : int64; pi_ofp_match : oxmMatch;
                  pi_payload : payload } [@@deriving sexp]

type flowReason =
  | FlowIdleTimeout
  | FlowHardTiemout
  | FlowDelete
  | FlowGroupDelete
  [@@deriving sexp]

type flowRemoved = { cookie : int64; priority : int16; reason : flowReason;
                     table_id : tableId; duration_sec : int32; duration_nsec : int32;
                     idle_timeout : timeout; hard_timeout : timeout; packet_count : int64;
                     byte_count : int64; oxm : oxmMatch } [@@deriving sexp]

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; ip_reasm :
                      bool; queue_stats : bool; port_blocked : bool } [@@deriving sexp]

type portState = { link_down : bool; blocked : bool; live : bool } [@@deriving sexp]

type portConfig = { port_down : bool; no_recv : bool; no_fwd : bool;
                    no_packet_in : bool } [@@deriving sexp]

type portFeatures = { rate_10mb_hd : bool; rate_10mb_fd : bool;
                      rate_100mb_hd : bool; rate_100mb_fd : bool;
                      rate_1gb_hd : bool; rate_1gb_fd : bool;
                      rate_10gb_fd : bool; rate_40gb_fd : bool;
                      rate_100gb_fd : bool; rate_1tb_fd : bool;
                      other : bool; copper : bool; fiber : bool;
                      autoneg : bool; pause : bool; pause_asym : bool } [@@deriving sexp]


type portDesc = { port_no : portId; hw_addr : int48; name : string; config :
                  portConfig; state : portState; curr : portFeatures;
                  advertised : portFeatures; supported : portFeatures; peer :
                  portFeatures; curr_speed : int32; max_speed : int32} [@@deriving sexp]

type portMod = { mpPortNo : portId; mpHw_addr : int48; mpConfig : portConfig;
                 mpMask : int32; mpAdvertise : portFeatures } [@@deriving sexp]

type portReason =
  | PortAdd
  | PortDelete
  | PortModify
  [@@deriving sexp]

type portStatus = { reason : portReason; desc : portDesc } [@@deriving sexp]

type packetOut = {
  po_payload : payload;
  po_port_id : portId option;
  po_actions : actionSequence
} [@@deriving sexp]

type rate = int32 [@@deriving sexp]

type burst = int32 [@@deriving sexp]

type experimenterId = int32 [@@deriving sexp]

type meterBand =
  | Drop of (rate*burst)
  | DscpRemark of (rate*burst*int8)
  | ExpMeter of (rate*burst*experimenterId)
  [@@deriving sexp]

type meterCommand =
  | AddMeter
  | ModifyMeter
  | DeleteMeter
  [@@deriving sexp]

type meterFlags = { kbps : bool; pktps : bool; burst : bool; stats : bool} [@@deriving sexp]

type meterMod = { command : meterCommand; flags : meterFlags; meter_id : int32;
                  bands : meterBand list} [@@deriving sexp]

type flowRequest = {fr_table_id : tableId; fr_out_port : portId;
                    fr_out_group : portId; fr_cookie : int64 mask;
                    fr_match : oxmMatch} [@@deriving sexp]

type queueRequest = {port_number : portId; queue_id : int32} [@@deriving sexp]

type experimenter = {exp_id : experimenterId; exp_type : int32} [@@deriving sexp]

type tableFeatureProp =
  | TfpInstruction of instructionHdr list
  | TfpInstructionMiss of instructionHdr list
  | TfpNextTable of tableId list
  | TfpNextTableMiss of tableId list
  | TfpWriteAction of actionHdr list
  | TfpWriteActionMiss of actionHdr list
  | TfpApplyAction of actionHdr list
  | TfpApplyActionMiss of actionHdr list
  | TfpMatch of oxm list
  | TfpWildcard of oxm list
  | TfpWriteSetField of oxm list
  | TfpWriteSetFieldMiss of oxm list
  | TfpApplySetField of oxm list
  | TfpApplySetFieldMiss of oxm list
  | TfpExperimenter of (experimenter*Cstruct.t)
  | TfpExperimenterMiss of (experimenter*Cstruct.t)
  [@@deriving sexp]

type tableConfig = Deprecated [@@deriving sexp]

type tableFeatures = {length : int16; table_id : tableId; name : string;
                      metadata_match : int64; metadata_write : int64;
                      config : tableConfig; max_entries: int32;
                      feature_prop : tableFeatureProp list} [@@deriving sexp]

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
  [@@deriving sexp]

type multipartRequest = { mpr_type : multipartType; mpr_flags : bool } [@@deriving sexp]


val portDescReq : multipartRequest

type switchDesc = { mfr_desc :string ; hw_desc : string; sw_desc : string;
                         serial_num : string; dp_desc : string } [@@deriving sexp]

type flowStats = { table_id : tableId; duration_sec : int32; duration_nsec :
                   int32; priority : int16; idle_timeout : timeout;
                   hard_timeout : timeout; flags : flowModFlags; cookie : int64;
                   packet_count : int64; byte_count : int64; ofp_match : oxmMatch;
                   instructions : instruction list} [@@deriving sexp]

type aggregStats = { packet_count : int64; byte_count : int64; flow_count : int32} [@@deriving sexp]

type tableStats = { table_id : tableId; active_count : int32; lookup_count : int64;
                    matched_count : int64} [@@deriving sexp]

type portStats = { psPort_no : portId; rx_packets : int64; tx_packets : int64;
                   rx_bytes : int64; tx_bytes : int64; rx_dropped : int64;
                   tx_dropped : int64; rx_errors : int64; tx_errors : int64;
                   rx_frame_err : int64; rx_over_err : int64; rx_crc_err : int64;
                   collisions : int64; duration_sec : int32; duration_nsec : int32} [@@deriving sexp]

type queueStats = { qsPort_no : portId; queue_id : int32; tx_bytes : int64; tx_packets : int64;
                    tx_errors : int64; duration_sec : int32; duration_nsec : int32 } [@@deriving sexp]

type bucketStats = { packet_count : int64; byte_count : int64} [@@deriving sexp]

type groupStats = { length : int16; group_id : int32; ref_count : int32;
                    packet_count : int64; byte_count : int64; duration_sec : int32;
                    duration_nsec : int32; bucket_stats : bucketStats list} [@@deriving sexp]

type groupDesc = { length : int16; typ : groupType; group_id : int32; bucket : bucket list} [@@deriving sexp]

type groupCapabilities = { select_weight : bool; select_liveness : bool;
                           chaining : bool; chaining_checks : bool} [@@deriving sexp]

type groupTypeMap = { all : bool; select : bool; indirect : bool; ff : bool} [@@deriving sexp]

type actionTypeMap = { output : bool; copy_ttl_out : bool; copy_ttl_in : bool;
                       set_mpls_ttl : bool; dec_mpls_ttl : bool; push_vlan : bool;
                       pop_vlan : bool; push_mpls : bool; pop_mpls : bool; set_queue : bool;
                       group : bool; set_nw_ttl : bool; dec_nw_ttl : bool; set_field : bool;
                       push_pbb : bool; pop_pbb : bool } [@@deriving sexp]

type groupFeatures = { typ : groupTypeMap; capabilities : groupCapabilities;
                       max_groups_all : int32; max_groups_select : int32;
                       max_groups_indirect : int32; max_groups_ff : int32;
                       actions_all : actionTypeMap; actions_select : actionTypeMap;
                       actions_indirect : actionTypeMap; actions_ff : actionTypeMap } [@@deriving sexp]

type meterBandStats = { packet_band_count : int64; byte_band_count : int64 } [@@deriving sexp]

type meterStats = { meter_id: int32; len : int16; flow_count : int32; packet_in_count :
                    int64; byte_in_count : int64; duration_sec : int32; duration_nsec :
                    int32; band : meterBandStats list} [@@deriving sexp]

type meterConfig = { length : length; flags : meterFlags; meter_id : int32; bands : meterBand list}
  [@@deriving sexp]

type meterBandMaps = { drop : bool; dscpRemark : bool} [@@deriving sexp]

type meterFeatures = { max_meter : int32; band_typ : meterBandMaps;
                            capabilities : meterFlags; max_band : int8;
                            max_color : int8 } [@@deriving sexp]

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
  | MeterFeaturesReply of meterFeatures
  [@@deriving sexp]

type multipartReply = {mpreply_typ : multipartReplyTyp; mpreply_flags : bool} [@@deriving sexp]

type tableMod = { table_id : tableId; config : tableConfig } [@@deriving sexp]

type rateQueue =
  | Rate of int
  | Disabled
  [@@deriving sexp]

type queueProp =
  | MinRateProp of rateQueue
  | MaxRateProp of rateQueue
  | ExperimenterProp of int32
  [@@deriving sexp]

type queueDesc = { queue_id : int32; port : portId; len : int16; properties : queueProp list }
  [@@deriving sexp]

type queueConfReq = { port : portId } [@@deriving sexp]

type queueConfReply = { port : portId; queues : queueDesc list } [@@deriving sexp]

type controllerRole =
  | NoChangeRole
  | EqualRole
  | MasterRole
  | SlaveRole
  [@@deriving sexp]

type roleRequest = { role : controllerRole; generation_id : int64 } [@@deriving sexp]

type supportedList = int list [@@deriving sexp]

type element =
  | VersionBitMap of supportedList
  [@@deriving sexp]

type helloElement = element list [@@deriving sexp]

type packetInReasonMap =  { table_miss : bool; apply_action : bool; invalid_ttl : bool }
  [@@deriving sexp]

type portReasonMap =  { add : bool; delete : bool; modify : bool } [@@deriving sexp]

type flowReasonMask = { idle_timeout : bool; hard_timeout : bool; delete : bool;
                        group_delete : bool} [@@deriving sexp]

type asyncConfig = { packet_in : packetInReasonMap asyncMask;
                     port_status : portReasonMap asyncMask;
                     flow_removed : flowReasonMask asyncMask } [@@deriving sexp]


type msg_code =  | HELLO | ERROR | ECHO_REQ | ECHO_RESP | VENDOR | FEATURES_REQ
                 | FEATURES_RESP | GET_CONFIG_REQ | GET_CONFIG_RESP
                 | SET_CONFIG | PACKET_IN | FLOW_REMOVED | PORT_STATUS | PACKET_OUT
                 | FLOW_MOD | GROUP_MOD | PORT_MOD | TABLE_MOD | MULTIPART_REQ
                 | MULTIPART_RESP | BARRIER_REQ | BARRIER_RESP | QUEUE_GET_CONFIG_REQ
                 | QUEUE_GET_CONFIG_RESP | ROLE_REQ | ROLE_RESP | GET_ASYNC_REQ
                 | GET_ASYNC_REP | SET_ASYNC | METER_MOD

val msg_code_to_int : msg_code -> int

(** See the [ofp_port_config] enumeration in section 7.2.1 of the OpenFlow 1.3.4 specification *)
module PortConfig : sig

  type t = portConfig

  val marshal : t -> int32

  val parse : int32 -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** See the [ofp_port_features] enumeration in section 7.2.1 of the OpenFlow 1.3.4 specification *)
module PortFeatures : sig

  type t = portFeatures

  val marshal : t -> int32

  val parse : int32 -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Flow Match Fields structure. See the section 7.2.3.2 of the OpenFlow 1.3.4 specification *)
module Oxm : sig

  type t = oxm

  val field_name : t -> string

  (** [sizeof t] size of the oxm field *)
  val sizeof : t -> int

  (** [sizeof_header t] size of the oxm field without the payload *)
  val sizeof_header : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  (** [match_to_string v] pretty-prints oxmMatch [v] *)
  val match_to_string : oxmMatch -> string

  (** [marshal buf t] serializes [t] *)
  val marshal : Cstruct.t -> t -> int

  (** [marshal_header buf t] same as [marshal] but doesn't serialize the payload *)
  val marshal_header : Cstruct.t -> t -> int

  (** [parse bits] parse the buffer [bits] *)
  val parse : Cstruct.t -> t * Cstruct.t

  (** [parse_header bits] same as [parse] but doesn't parse the payload *)
  val parse_header : Cstruct.t -> t * Cstruct.t

  val from_of_pattern : OpenFlow.Pattern.t -> t list
end

module PseudoPort : sig

  type t = pseudoPort

  val size_of : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : t -> int32

  val make : int32 -> int16 -> t

end

(** Queue Description structure. See the section 7.2.2 of the OpenFlow 1.3.4 specification *)
module QueueDesc : sig

  (** Queue Property Description structure. See the 7.2.2 of the OpenFlow 1.3.4 specification *)
  module QueueProp : sig

    type t = queueProp

    val sizeof : t -> int

    (** [to_string v] pretty-prints [v] *)
    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = queueDesc

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Switch Configuration structure. See the section 7.3.2 of the OpenFlow 1.3.4 specification *)
module SwitchConfig : sig

  type t = switchConfig

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Match structure. See the section 7.2.3.1 of the OpenFlow 1.3.4 specification *)
module OfpMatch : sig

  type t = oxmMatch

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t * Cstruct.t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Action structure. See the section 7.2.5 of the OpenFlow 1.3.4 specification *)
module Action : sig

  type t = action

  type sequence = actionSequence

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val parse_sequence : Cstruct.t -> sequence

  (** [to_string v] pretty-prints [v] *)
  val to_string :  t -> string

  val from_of_action : OpenFlow.action -> t

  val from_of_seq : OpenFlow.seq -> sequence

end

(** Bucket structure for use in groups. See the section 7.3.4.2 of OpenFlow 1.3.4 specification *)
module Bucket : sig

  type t = bucket

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Flow message structure. See the section 7.3.4.1 of the OpenFlow 1.3.4 specification *)
module FlowModCommand : sig

  type t = flowModCommand

  val sizeof : t -> int

  val marshal : t -> int

  val parse : int -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** See the [ofp_group_type] enumeration in section 7.3.4.2 of the OpenFlow 1.3.4 specification *)
module GroupType : sig

  type t = groupType

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : t -> int

  val parse : int -> t

end

(** Modify Group message structure. See the section 7.3.4.2 of the OpenFlow 1.3.4 specification *)
module GroupMod : sig

  type t = groupMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Port message structure. See the section 7.3.4.3 of the OpenFlow 1.3.4 specification *)
module PortMod : sig

  type t = portMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Meter message structure. See the section 7.3.4.4 of the OpenFlow 1.3.4 specification *)
module MeterMod : sig

  type t = meterMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Instruction structure. See the section 7.2.4 of the OpenFlow 1.3.4 specification *)
module Instruction : sig

  type t = instruction

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t ->  t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

module Instructions : sig

  type t = instruction list

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val from_of_group : OpenFlow.group -> t

end

(** Modify flow message structure. See the section 7.3.4.1 of the OpenFlow 1.3.4 specification *)
module FlowMod : sig

  type t = flowMod

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** See the [ofp_capabilities] enumeration in section 7.3.1 of OpenFlow 1.3.4 specification *)
module Capabilities : sig

  type t = capabilities

 (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val to_int32 : t -> int32

  val parse : int32  -> t

end

type switchFeatures = { datapath_id : int64;
			num_buffers : int32;
			num_tables : int8;
			aux_id : int8;
			supported_capabilities : capabilities }


(** Switch Features structure. See the section 7.3.1 of the OpenFlow 1.3.4 specification *)
module SwitchFeatures : sig

  type t = switchFeatures

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** See the [ofp_port_state] enumeration in section 7.2.1 of the OpenFlow 1.3.4 specification *)
module PortState : sig

  type t = portState

  val marshal : portState -> int32

  val parse : int32 -> portState

  (** [to_string v] pretty-prints [v] *)
  val to_string : portState -> string

end

(** Description of a port structure. See the section 7.3.1 of the OpenFlow 1.3.4 specification *)
module PortDesc : sig

  type t = portDesc

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Port Status structure. See the section 7.4.3 of the OpenFlow 1.3.4 specification *)
module PortStatus : sig

  type t = portStatus

  val sizeof : t -> int

  val marshal : Cstruct.t ->  t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Packet received by the datapath and sent to the controller structure. See the section
    7.4.1 of the OpenFlow 1.3.4 specification *)
module PacketIn : sig

  type t = packetIn

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Packet send out of the datapath structure. See the section 7.3.7 of the OpenFlow 1.3.4 specification *)
module PacketOut : sig

  type t = packetOut

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter bands structure. See the section 7.3.4.4 of the OpenFlow 1.3.4 specification *)
module MeterBand : sig

  type t = meterBand

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Removed structure. See the section 7.4.2 of the OpenFlow 1.3.4 specification *)
module FlowRemoved : sig

  type t = flowRemoved

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Flow Statistics request structure. See the section 7.3.5.2 of the OpenFlow 1.3.4 specification
    this structure is the same for indidual and aggregate flow request *)
module FlowRequest : sig

  type t = flowRequest

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Statistics request structure. See the section 7.3.5.8 of the OpenFlow 1.3.4 specification *)
module QueueRequest : sig

  type t = queueRequest

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val sizeof : t -> int

  val to_string : t -> string

end

(** Table Features property structure. See the section 7.3.5.5.2 of the OpenFlow 1.3.4 specification *)
module TableFeatureProp : sig

  type t = tableFeatureProp

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Table Feature structure. See the section 7.3.5.5.1 of the OpenFlow 1.3.4 specification *)
module TableFeature : sig

  type t = tableFeatures

  val sizeof : t -> int

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Multipart request message structure. See the section 7.3.5 of the OpenFlow 1.3.4 specification *)
module MultipartReq : sig

  type t = multipartRequest

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Group statistics structure. See the section 7.3.5.9 of the OpenFlow 1.3.4 specification *)
module GroupStats : sig

  (** Bucket statistics structure. See the section 7.3.5.9 of the OpenFlow 1.3.4 specification *)
  module BucketStats : sig

    type t = bucketStats

    val sizeof : t -> int

    (** [to_string v] pretty-prints [v] *)
    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = groupStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t ->  t -> int

  val parse : Cstruct.t ->  t
end

(** Switch Description structure. See the section 7.3.5.1 of the OpenFlow 1.3.4 specification *)
module SwitchDescriptionReply : sig

  type t = switchDesc

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Individual Flow Statistics structure. See the section 7.3.5.2 of the OpenFlow 1.3.4 specification *)
module FlowStats : sig

  type t = flowStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Aggregate Flow Statistics structure. See the section 7.3.5.3 of the OpenFlow 1.3.4 specification *)
module AggregateStats : sig

  type t = aggregStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Table Statistics structure. See the section 7.3.5.4 of the OpenFlow 1.3.4 specification *)
module TableStats : sig

  type t = tableStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Port Statistics structure. See the section 7.3.5.6 of the OpenFlow 1.3.4 specification *)
module PortStats : sig

  type t = portStats

  val sizeof : t-> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Statistics structure. See the section 7.3.5.8 of the OpenFlow 1.3.4 specification *)
module QueueStats : sig

  type t = queueStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Group Description structure. See the section 7.3.5.10 of the OpenFlow 1.3.4 specification *)
module GroupDesc : sig

  type t = groupDesc

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Group Features structure. See the section 7.3.5.10 of the OpenFlow 1.3.4 specification *)
module GroupFeatures : sig

  type t = groupFeatures

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter Statistics structure. See the section 7.3.5.12 of the OpenFlow 1.3.4 specification *)
module MeterStats : sig

  type t = meterStats

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter Configuration structure. See the section 7.3.5.13 of the OpenFlow 1.3.4 specification *)
module MeterConfig : sig

  type t = meterConfig

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Meter Features structure. See the section 7.3.5.14 of the OpenFlow 1.3.4 specification *)
module MeterFeatures : sig

  type t = meterFeatures

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Multipart reply message structure. See the section 7.3.5 of the OpenFlow 1.3.4 specification *)
module MultipartReply : sig

  type t = multipartReply

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Modify Table message structure. See the section 7.3.3 of the OpenFlow 1.3.4 specification *)
module TableMod : sig

  type t = tableMod

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Configuration request message structure. See the section 7.3.6 of the OpenFlow 1.3.4 specification *)
module QueueConfReq : sig

  type t = queueConfReq

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Queue Configuration respond message structure. See the section 7.3.6 of the OpenFlow 1.3.4 specification *)
module QueueConfReply : sig

  type t = queueConfReply

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

type error = {
    err : errorTyp;
    data : Cstruct.t;
  }

(** Error message structure. See the section 7.4.4 of the OpenFlow 1.3.4 specification *)
module Error : sig

  type t = error

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

end

(** Role Request message structure. See the section 7.3.9 of OpenFlow 1.3.4 specification *)
module RoleRequest : sig

  type t = roleRequest

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Hello message structure. See the section 7.5.1 of the OpenFlow 1.3.4 specification *)
module Hello : sig

  (** Hello Element structure. See the section 7.5.1 of OpenFlow 1.3.4 specification *)
  module Element : sig

    (** Supported Version Bitmap structure. See the section 7.5.1. of OpenFlow 1.3.4 specification *)
    module VersionBitMap : sig

      type t = supportedList

      val sizeof : t -> int

      (** [to_string v] pretty-prints [v] *)
      val to_string : t -> string

      val marshal : Cstruct.t -> t -> int

      val parse : Cstruct.t -> t

    end

    type t = element

    val sizeof : t -> int

    (** [to_string v] pretty-prints [v] *)
    val to_string : t -> string

    val marshal : Cstruct.t -> t -> int

    val parse : Cstruct.t -> t

  end

  type t = helloElement

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

(** Set Asynchronous message structure. See the section 7.3.10 of OpenFlow 1.3.4 specification *)
module AsyncConfig : sig

  type t = asyncConfig

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val marshal : Cstruct.t -> t -> int

  val parse : Cstruct.t -> t

end

module Message : sig

  type t =
    | Hello of element list
    | EchoRequest of Cstruct.t
    | EchoReply of Cstruct.t
    | FeaturesRequest
    | FeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | GroupModMsg of GroupMod.t
    | PortModMsg of PortMod.t
    | MeterModMsg of MeterMod.t
    | PacketInMsg of PacketIn.t
    | FlowRemoved of FlowRemoved.t
    | PacketOutMsg of PacketOut.t
    | PortStatusMsg of PortStatus.t
    | MultipartReq of MultipartReq.t
    | MultipartReply of MultipartReply.t
    | BarrierRequest
    | BarrierReply
    | RoleRequest of RoleRequest.t
    | RoleReply of RoleRequest.t
    | QueueGetConfigReq of QueueConfReq.t
    | QueueGetConfigReply of QueueConfReply.t
    | GetConfigRequestMsg
    | GetConfigReplyMsg of SwitchConfig.t
    | SetConfigMsg of SwitchConfig.t
    | TableModMsg of TableMod.t
    | GetAsyncRequest
    | GetAsyncReply of AsyncConfig.t
    | SetAsync of AsyncConfig.t
    | Error of Error.t
    [@@deriving sexp]

  val sizeof : t -> int

  (** [to_string v] pretty-prints [v] *)
  val to_string : t -> string

  val blit_message : t -> Cstruct.t -> int

  val header_of : xid -> t -> OpenFlow_Header.t

  val marshal : xid -> t -> string

  val parse : OpenFlow_Header.t -> string -> (xid * t)

  val marshal_body : t -> Cstruct.t -> unit

end
