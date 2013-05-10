open NetworkPacket
open WordInterface

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val coq_VLAN_NONE : dlVlan **)

let coq_VLAN_NONE = 65535

type of_match = { matchDlSrc : dlAddr option; matchDlDst : dlAddr option;
                  matchDlTyp : dlTyp option; matchDlVlan : dlVlan option;
                  matchDlVlanPcp : dlVlanPcp option;
                  matchNwSrc : nwAddr option; matchNwDst : nwAddr option;
                  matchNwProto : nwProto option; matchNwTos : nwTos option;
                  matchTpSrc : tpPort option; matchTpDst : tpPort option;
                  matchInPort : portId option }

(** val of_match_rect :
    (dlAddr option -> dlAddr option -> dlTyp option -> dlVlan option ->
    dlVlanPcp option -> nwAddr option -> nwAddr option -> nwProto option ->
    nwTos option -> tpPort option -> tpPort option -> portId option -> 'a1)
    -> of_match -> 'a1 **)

let of_match_rect f o =
  let { matchDlSrc = x; matchDlDst = x0; matchDlTyp = x1; matchDlVlan = x2;
    matchDlVlanPcp = x3; matchNwSrc = x4; matchNwDst = x5; matchNwProto = x6;
    matchNwTos = x7; matchTpSrc = x8; matchTpDst = x9; matchInPort = x10 } =
    o
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

(** val of_match_rec :
    (dlAddr option -> dlAddr option -> dlTyp option -> dlVlan option ->
    dlVlanPcp option -> nwAddr option -> nwAddr option -> nwProto option ->
    nwTos option -> tpPort option -> tpPort option -> portId option -> 'a1)
    -> of_match -> 'a1 **)

let of_match_rec f o =
  let { matchDlSrc = x; matchDlDst = x0; matchDlTyp = x1; matchDlVlan = x2;
    matchDlVlanPcp = x3; matchNwSrc = x4; matchNwDst = x5; matchNwProto = x6;
    matchNwTos = x7; matchTpSrc = x8; matchTpDst = x9; matchInPort = x10 } =
    o
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

(** val matchDlSrc : of_match -> dlAddr option **)

let matchDlSrc x = x.matchDlSrc

(** val matchDlDst : of_match -> dlAddr option **)

let matchDlDst x = x.matchDlDst

(** val matchDlTyp : of_match -> dlTyp option **)

let matchDlTyp x = x.matchDlTyp

(** val matchDlVlan : of_match -> dlVlan option **)

let matchDlVlan x = x.matchDlVlan

(** val matchDlVlanPcp : of_match -> dlVlanPcp option **)

let matchDlVlanPcp x = x.matchDlVlanPcp

(** val matchNwSrc : of_match -> nwAddr option **)

let matchNwSrc x = x.matchNwSrc

(** val matchNwDst : of_match -> nwAddr option **)

let matchNwDst x = x.matchNwDst

(** val matchNwProto : of_match -> nwProto option **)

let matchNwProto x = x.matchNwProto

(** val matchNwTos : of_match -> nwTos option **)

let matchNwTos x = x.matchNwTos

(** val matchTpSrc : of_match -> tpPort option **)

let matchTpSrc x = x.matchTpSrc

(** val matchTpDst : of_match -> tpPort option **)

let matchTpDst x = x.matchTpDst

(** val matchInPort : of_match -> portId option **)

let matchInPort x = x.matchInPort

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; stp : bool; ip_reasm : bool;
                      queue_stats : bool; arp_match_ip : bool }

(** val capabilities_rect :
    (bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
    capabilities -> 'a1 **)

let capabilities_rect f c =
  let { flow_stats = x; table_stats = x0; port_stats = x1; stp = x2;
    ip_reasm = x3; queue_stats = x4; arp_match_ip = x5 } = c
  in
  f x x0 x1 x2 x3 x4 x5

(** val capabilities_rec :
    (bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
    capabilities -> 'a1 **)

let capabilities_rec f c =
  let { flow_stats = x; table_stats = x0; port_stats = x1; stp = x2;
    ip_reasm = x3; queue_stats = x4; arp_match_ip = x5 } = c
  in
  f x x0 x1 x2 x3 x4 x5

(** val flow_stats : capabilities -> bool **)

let flow_stats x = x.flow_stats

(** val table_stats : capabilities -> bool **)

let table_stats x = x.table_stats

(** val port_stats : capabilities -> bool **)

let port_stats x = x.port_stats

(** val stp : capabilities -> bool **)

let stp x = x.stp

(** val ip_reasm : capabilities -> bool **)

let ip_reasm x = x.ip_reasm

(** val queue_stats : capabilities -> bool **)

let queue_stats x = x.queue_stats

(** val arp_match_ip : capabilities -> bool **)

let arp_match_ip x = x.arp_match_ip

type actions = { output : bool; set_vlan_id : bool; set_vlan_pcp : bool;
                 strip_vlan : bool; set_dl_src : bool; set_dl_dst : bool;
                 set_nw_src : bool; set_nw_dst : bool; set_nw_tos : bool;
                 set_tp_src : bool; set_tp_dst : bool; enqueue : bool;
                 vendor : bool }

(** val actions_rect :
    (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool ->
    bool -> bool -> bool -> bool -> 'a1) -> actions -> 'a1 **)

let actions_rect f a =
  let { output = x; set_vlan_id = x0; set_vlan_pcp = x1; strip_vlan = x2;
    set_dl_src = x3; set_dl_dst = x4; set_nw_src = x5; set_nw_dst = x6;
    set_nw_tos = x7; set_tp_src = x8; set_tp_dst = x9; enqueue = x10;
    vendor = x11 } = a
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11

(** val actions_rec :
    (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool ->
    bool -> bool -> bool -> bool -> 'a1) -> actions -> 'a1 **)

let actions_rec f a =
  let { output = x; set_vlan_id = x0; set_vlan_pcp = x1; strip_vlan = x2;
    set_dl_src = x3; set_dl_dst = x4; set_nw_src = x5; set_nw_dst = x6;
    set_nw_tos = x7; set_tp_src = x8; set_tp_dst = x9; enqueue = x10;
    vendor = x11 } = a
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11

(** val output : actions -> bool **)

let output x = x.output

(** val set_vlan_id : actions -> bool **)

let set_vlan_id x = x.set_vlan_id

(** val set_vlan_pcp : actions -> bool **)

let set_vlan_pcp x = x.set_vlan_pcp

(** val strip_vlan : actions -> bool **)

let strip_vlan x = x.strip_vlan

(** val set_dl_src : actions -> bool **)

let set_dl_src x = x.set_dl_src

(** val set_dl_dst : actions -> bool **)

let set_dl_dst x = x.set_dl_dst

(** val set_nw_src : actions -> bool **)

let set_nw_src x = x.set_nw_src

(** val set_nw_dst : actions -> bool **)

let set_nw_dst x = x.set_nw_dst

(** val set_nw_tos : actions -> bool **)

let set_nw_tos x = x.set_nw_tos

(** val set_tp_src : actions -> bool **)

let set_tp_src x = x.set_tp_src

(** val set_tp_dst : actions -> bool **)

let set_tp_dst x = x.set_tp_dst

(** val enqueue : actions -> bool **)

let enqueue x = x.enqueue

(** val vendor : actions -> bool **)

let vendor x = x.vendor

type features = { switch_id : Word64.t; num_buffers : Word32.t;
                  num_tables : Word8.t;
                  supported_capabilities : capabilities;
                  supported_actions : actions }

(** val features_rect :
    (Word64.t -> Word32.t -> Word8.t -> capabilities -> actions -> 'a1) ->
    features -> 'a1 **)

let features_rect f f0 =
  let { switch_id = x; num_buffers = x0; num_tables = x1;
    supported_capabilities = x2; supported_actions = x3 } = f0
  in
  f x x0 x1 x2 x3

(** val features_rec :
    (Word64.t -> Word32.t -> Word8.t -> capabilities -> actions -> 'a1) ->
    features -> 'a1 **)

let features_rec f f0 =
  let { switch_id = x; num_buffers = x0; num_tables = x1;
    supported_capabilities = x2; supported_actions = x3 } = f0
  in
  f x x0 x1 x2 x3

(** val switch_id : features -> Word64.t **)

let switch_id x = x.switch_id

(** val num_buffers : features -> Word32.t **)

let num_buffers x = x.num_buffers

(** val num_tables : features -> Word8.t **)

let num_tables x = x.num_tables

(** val supported_capabilities : features -> capabilities **)

let supported_capabilities x = x.supported_capabilities

(** val supported_actions : features -> actions **)

let supported_actions x = x.supported_actions

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

(** val flowModCommand_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> flowModCommand -> 'a1 **)

let flowModCommand_rect f f0 f1 f2 f3 = function
| AddFlow -> f
| ModFlow -> f0
| ModStrictFlow -> f1
| DeleteFlow -> f2
| DeleteStrictFlow -> f3

(** val flowModCommand_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> flowModCommand -> 'a1 **)

let flowModCommand_rec f f0 f1 f2 f3 = function
| AddFlow -> f
| ModFlow -> f0
| ModStrictFlow -> f1
| DeleteFlow -> f2
| DeleteStrictFlow -> f3

type switchId = Word64.t

type priority = Word16.t

type bufferId = Word32.t

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of Word16.t

(** val pseudoPort_rect :
    (portId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (Word16.t -> 'a1) -> pseudoPort
    -> 'a1 **)

let pseudoPort_rect f f0 f1 f2 f3 = function
| PhysicalPort x -> f x
| InPort -> f0
| Flood -> f1
| AllPorts -> f2
| Controller x -> f3 x

(** val pseudoPort_rec :
    (portId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (Word16.t -> 'a1) -> pseudoPort
    -> 'a1 **)

let pseudoPort_rec f f0 f1 f2 f3 = function
| PhysicalPort x -> f x
| InPort -> f0
| Flood -> f1
| AllPorts -> f2
| Controller x -> f3 x

type action =
| Output of pseudoPort
| SetDlVlan of dlVlan
| SetDlVlanPcp of dlVlanPcp
| StripVlan
| SetDlSrc of dlAddr
| SetDlDst of dlAddr
| SetNwSrc of nwAddr
| SetNwDst of nwAddr
| SetNwTos of nwTos
| SetTpSrc of tpPort
| SetTpDst of tpPort

(** val action_rect :
    (pseudoPort -> 'a1) -> (dlVlan -> 'a1) -> (dlVlanPcp -> 'a1) -> 'a1 ->
    (dlAddr -> 'a1) -> (dlAddr -> 'a1) -> (nwAddr -> 'a1) -> (nwAddr -> 'a1)
    -> (nwTos -> 'a1) -> (tpPort -> 'a1) -> (tpPort -> 'a1) -> action -> 'a1 **)

let action_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
| Output x -> f x
| SetDlVlan x -> f0 x
| SetDlVlanPcp x -> f1 x
| StripVlan -> f2
| SetDlSrc x -> f3 x
| SetDlDst x -> f4 x
| SetNwSrc x -> f5 x
| SetNwDst x -> f6 x
| SetNwTos x -> f7 x
| SetTpSrc x -> f8 x
| SetTpDst x -> f9 x

(** val action_rec :
    (pseudoPort -> 'a1) -> (dlVlan -> 'a1) -> (dlVlanPcp -> 'a1) -> 'a1 ->
    (dlAddr -> 'a1) -> (dlAddr -> 'a1) -> (nwAddr -> 'a1) -> (nwAddr -> 'a1)
    -> (nwTos -> 'a1) -> (tpPort -> 'a1) -> (tpPort -> 'a1) -> action -> 'a1 **)

let action_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
| Output x -> f x
| SetDlVlan x -> f0 x
| SetDlVlanPcp x -> f1 x
| StripVlan -> f2
| SetDlSrc x -> f3 x
| SetDlDst x -> f4 x
| SetNwSrc x -> f5 x
| SetNwDst x -> f6 x
| SetNwTos x -> f7 x
| SetTpSrc x -> f8 x
| SetTpDst x -> f9 x

type actionSequence = action list

type timeout =
| Permanent
| ExpiresAfter of Word16.t

(** val timeout_rect : 'a1 -> (Word16.t -> __ -> 'a1) -> timeout -> 'a1 **)

let timeout_rect f f0 = function
| Permanent -> f
| ExpiresAfter x -> f0 x __

(** val timeout_rec : 'a1 -> (Word16.t -> __ -> 'a1) -> timeout -> 'a1 **)

let timeout_rec f f0 = function
| Permanent -> f
| ExpiresAfter x -> f0 x __

type flowMod = { mfModCmd : flowModCommand; mfMatch : of_match;
                 mfPriority : priority; mfActions : actionSequence;
                 mfCookie : Word64.t; mfIdleTimeOut : timeout;
                 mfHardTimeOut : timeout; mfNotifyWhenRemoved : bool;
                 mfApplyToPacket : bufferId option;
                 mfOutPort : pseudoPort option; mfCheckOverlap : bool }

(** val flowMod_rect :
    (flowModCommand -> of_match -> priority -> actionSequence -> Word64.t ->
    timeout -> timeout -> bool -> bufferId option -> pseudoPort option ->
    bool -> 'a1) -> flowMod -> 'a1 **)

let flowMod_rect f f0 =
  let { mfModCmd = x; mfMatch = x0; mfPriority = x1; mfActions = x2;
    mfCookie = x3; mfIdleTimeOut = x4; mfHardTimeOut = x5;
    mfNotifyWhenRemoved = x6; mfApplyToPacket = x7; mfOutPort = x8;
    mfCheckOverlap = x9 } = f0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9

(** val flowMod_rec :
    (flowModCommand -> of_match -> priority -> actionSequence -> Word64.t ->
    timeout -> timeout -> bool -> bufferId option -> pseudoPort option ->
    bool -> 'a1) -> flowMod -> 'a1 **)

let flowMod_rec f f0 =
  let { mfModCmd = x; mfMatch = x0; mfPriority = x1; mfActions = x2;
    mfCookie = x3; mfIdleTimeOut = x4; mfHardTimeOut = x5;
    mfNotifyWhenRemoved = x6; mfApplyToPacket = x7; mfOutPort = x8;
    mfCheckOverlap = x9 } = f0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9

(** val mfModCmd : flowMod -> flowModCommand **)

let mfModCmd x = x.mfModCmd

(** val mfMatch : flowMod -> of_match **)

let mfMatch x = x.mfMatch

(** val mfPriority : flowMod -> priority **)

let mfPriority x = x.mfPriority

(** val mfActions : flowMod -> actionSequence **)

let mfActions x = x.mfActions

(** val mfCookie : flowMod -> Word64.t **)

let mfCookie x = x.mfCookie

(** val mfIdleTimeOut : flowMod -> timeout **)

let mfIdleTimeOut x = x.mfIdleTimeOut

(** val mfHardTimeOut : flowMod -> timeout **)

let mfHardTimeOut x = x.mfHardTimeOut

(** val mfNotifyWhenRemoved : flowMod -> bool **)

let mfNotifyWhenRemoved x = x.mfNotifyWhenRemoved

(** val mfApplyToPacket : flowMod -> bufferId option **)

let mfApplyToPacket x = x.mfApplyToPacket

(** val mfOutPort : flowMod -> pseudoPort option **)

let mfOutPort x = x.mfOutPort

(** val mfCheckOverlap : flowMod -> bool **)

let mfCheckOverlap x = x.mfCheckOverlap

type packetInReason =
| NoMatch
| ExplicitSend

(** val packetInReason_rect : 'a1 -> 'a1 -> packetInReason -> 'a1 **)

let packetInReason_rect f f0 = function
| NoMatch -> f
| ExplicitSend -> f0

(** val packetInReason_rec : 'a1 -> 'a1 -> packetInReason -> 'a1 **)

let packetInReason_rec f f0 = function
| NoMatch -> f
| ExplicitSend -> f0

type packetIn = { packetInBufferId : bufferId option;
                  packetInTotalLen : Word16.t; packetInPort : portId;
                  packetInReason_ : packetInReason; packetInPacket : 
                  packet }

(** val packetIn_rect :
    (bufferId option -> Word16.t -> portId -> packetInReason -> packet ->
    'a1) -> packetIn -> 'a1 **)

let packetIn_rect f p =
  let { packetInBufferId = x; packetInTotalLen = x0; packetInPort = x1;
    packetInReason_ = x2; packetInPacket = x3 } = p
  in
  f x x0 x1 x2 x3

(** val packetIn_rec :
    (bufferId option -> Word16.t -> portId -> packetInReason -> packet ->
    'a1) -> packetIn -> 'a1 **)

let packetIn_rec f p =
  let { packetInBufferId = x; packetInTotalLen = x0; packetInPort = x1;
    packetInReason_ = x2; packetInPacket = x3 } = p
  in
  f x x0 x1 x2 x3

(** val packetInBufferId : packetIn -> bufferId option **)

let packetInBufferId x = x.packetInBufferId

(** val packetInTotalLen : packetIn -> Word16.t **)

let packetInTotalLen x = x.packetInTotalLen

(** val packetInPort : packetIn -> portId **)

let packetInPort x = x.packetInPort

(** val packetInReason_ : packetIn -> packetInReason **)

let packetInReason_ x = x.packetInReason_

(** val packetInPacket : packetIn -> packet **)

let packetInPacket x = x.packetInPacket

type xid = Word32.t

type packetOut = { pktOutBufOrBytes : (bufferId, bytes) Misc.sum;
                   pktOutPortId : portId option;
                   pktOutActions : actionSequence }

(** val packetOut_rect :
    ((bufferId, bytes) sum -> portId option -> actionSequence -> 'a1) ->
    packetOut -> 'a1 **)

let packetOut_rect f p =
  let { pktOutBufOrBytes = x; pktOutPortId = x0; pktOutActions = x1 } = p in
  f x x0 x1

(** val packetOut_rec :
    ((bufferId, bytes) sum -> portId option -> actionSequence -> 'a1) ->
    packetOut -> 'a1 **)

let packetOut_rec f p =
  let { pktOutBufOrBytes = x; pktOutPortId = x0; pktOutActions = x1 } = p in
  f x x0 x1

(** val pktOutBufOrBytes : packetOut -> (bufferId, bytes) sum **)

let pktOutBufOrBytes x = x.pktOutBufOrBytes

(** val pktOutPortId : packetOut -> portId option **)

let pktOutPortId x = x.pktOutPortId

(** val pktOutActions : packetOut -> actionSequence **)

let pktOutActions x = x.pktOutActions

(* Component types of stats_request messages. *)

type table_id = Word8.t

module IndividualFlowRequest = struct
    type t = { of_match : of_match
             ; table_id : table_id
             ; port : pseudoPort
             }
end

module AggregateFlowRequest = struct
    type t = { of_match : of_match
             ; table_id : table_id
             ; port : pseudoPort
             }
end

(* Component types of stats_reply messages. *)

module DescriptionStats = struct
  type t = { manufacturer : string
           ; hardware : string
           ; software : string
           ; serial_number : string
           ; datapath : string
           }
end

module IndividualFlowStats = struct
    type t = { table_id : table_id
             ; of_match : of_match
             ; duration_sec : int
             ; duration_msec : int
             ; priority : int
             ; idle_timeout : int
             ; hard_timeout : int
             ; cookie : int
             ; byte_count : int
             ; actions : actionSequence
             }
end

module AggregateFlowStats = struct
    type t = { packet_count : int
             ; byte_count : int
             ; flow_count : int
             }
end

module TableStats = struct
    type t = { table_id : table_id
             ; name : string
             ; wildcards : Word32.t
             ; max_entries : int
             ; active_count : int
             ; lookup_count : int
             ; matched_count : int
             }
end

module PortStats = struct
    type t = { port_no : pseudoPort
             ; rx_packets : int
             ; tx_packets : int
             ; rx_bytes : int
             ; tx_bytes : int
             ; rx_dropped : int
             ; tx_dropped : int
             ; rx_errors : int
             ; tx_errors : int
             ; rx_frame_err : int
             ; rx_over_err : int
             ; rx_crc_err : int
             ; collisions : int
             }
end

type statsRequest =
| DescriptionReq
| IndividualFlowReq of IndividualFlowRequest.t
| AggregateFlowReq of AggregateFlowRequest.t
| TableReq
| PortReq of pseudoPort
(* TODO(cole): queue and vendor stats requests. *)

type statsReply =
| DescriptionRep of DescriptionStats.t
| IndividualFlowRep of IndividualFlowStats.t
| AggregateFlowRep of AggregateFlowStats.t
| TableRep of TableStats.t
| PortRep of PortStats.t

(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

type message =
| Hello of bytes
| EchoRequest of bytes
| EchoReply of bytes
| FeaturesRequest
| FeaturesReply of features
| FlowModMsg of flowMod
| PacketInMsg of packetIn
| PacketOutMsg of packetOut
| BarrierRequest
| BarrierReply
| StatsRequestMsg of statsRequest
| StatsReplyMsg of statsReply

