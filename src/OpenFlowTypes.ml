open NetworkPacket
open WordInterface

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val coq_VLAN_NONE : dlVlan **)

let coq_VLAN_NONE = 65535

type 'a mask = { m_value : 'a; m_mask : 'a option }

(** val mask_rect : ('a1 -> 'a1 option -> 'a2) -> 'a1 mask -> 'a2 **)

let mask_rect f m =
  let { m_value = x; m_mask = x0 } = m in f x x0

(** val mask_rec : ('a1 -> 'a1 option -> 'a2) -> 'a1 mask -> 'a2 **)

let mask_rec f m =
  let { m_value = x; m_mask = x0 } = m in f x x0

(** val m_value : 'a1 mask -> 'a1 **)

let m_value x = x.m_value

(** val m_mask : 'a1 mask -> 'a1 option **)

let m_mask x = x.m_mask

type xid = Word32.t

(** val val_to_mask : 'a1 -> 'a1 mask **)

let val_to_mask v =
  { m_value = v; m_mask = None }

type switchId = Word64.t

type groupId = Word32.t

type portId = Word32.t

type tableId = Word8.t

type bufferId = Word32.t

type oxm =
| OxmInPort of portId
| OxmInPhyPort of portId
| OxmMetadata of Word64.t mask
| OxmEthType of Word16.t
| OxmEthDst of Word48.t mask
| OxmEthSrc of Word48.t mask
| OxmVlanVId of Word12.t mask
| OxmVlanPcp of Word8.t
| OxmIPProto of Word8.t
| OxmIPDscp of Word8.t
| OxmIPEcn of Word8.t
| OxmIP4Src of Word32.t mask
| OxmIP4Dst of Word32.t mask
| OxmTCPSrc of Word16.t mask
| OxmTCPDst of Word16.t mask
| OxmARPOp of Word16.t
| OxmARPSpa of Word32.t mask
| OxmARPTpa of Word32.t mask
| OxmARPSha of Word48.t mask
| OxmARPTha of Word48.t mask
| OxmICMPType of Word8.t
| OxmICMPCode of Word8.t
| OxmMPLSLabel of Word32.t
| OxmMPLSTc of Word8.t
| OxmTunnelId of Word64.t mask

(** val oxm_rect :
    (portId -> 'a1) -> (portId -> 'a1) -> (Word64.t mask -> 'a1) -> (Word16.t
    -> 'a1) -> (Word48.t mask -> 'a1) -> (Word48.t mask -> 'a1) -> (Word12.t
    mask -> 'a1) -> (Word8.t -> 'a1) -> (Word8.t -> 'a1) -> (Word8.t -> 'a1)
    -> (Word8.t -> 'a1) -> (Word32.t mask -> 'a1) -> (Word32.t mask -> 'a1)
    -> (Word16.t mask -> 'a1) -> (Word16.t mask -> 'a1) -> (Word16.t -> 'a1)
    -> (Word32.t mask -> 'a1) -> (Word32.t mask -> 'a1) -> (Word48.t mask ->
    'a1) -> (Word48.t mask -> 'a1) -> (Word8.t -> 'a1) -> (Word8.t -> 'a1) ->
    (Word32.t -> 'a1) -> (Word8.t -> 'a1) -> (Word64.t mask -> 'a1) -> oxm ->
    'a1 **)

let oxm_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 = function
| OxmInPort x -> f x
| OxmInPhyPort x -> f0 x
| OxmMetadata x -> f1 x
| OxmEthType x -> f2 x
| OxmEthDst x -> f3 x
| OxmEthSrc x -> f4 x
| OxmVlanVId x -> f5 x
| OxmVlanPcp x -> f6 x
| OxmIPProto x -> f7 x
| OxmIPDscp x -> f8 x
| OxmIPEcn x -> f9 x
| OxmIP4Src x -> f10 x
| OxmIP4Dst x -> f11 x
| OxmTCPSrc x -> f12 x
| OxmTCPDst x -> f13 x
| OxmARPOp x -> f14 x
| OxmARPSpa x -> f15 x
| OxmARPTpa x -> f16 x
| OxmARPSha x -> f17 x
| OxmARPTha x -> f18 x
| OxmICMPType x -> f19 x
| OxmICMPCode x -> f20 x
| OxmMPLSLabel x -> f21 x
| OxmMPLSTc x -> f22 x
| OxmTunnelId x -> f23 x

(** val oxm_rec :
    (portId -> 'a1) -> (portId -> 'a1) -> (Word64.t mask -> 'a1) -> (Word16.t
    -> 'a1) -> (Word48.t mask -> 'a1) -> (Word48.t mask -> 'a1) -> (Word12.t
    mask -> 'a1) -> (Word8.t -> 'a1) -> (Word8.t -> 'a1) -> (Word8.t -> 'a1)
    -> (Word8.t -> 'a1) -> (Word32.t mask -> 'a1) -> (Word32.t mask -> 'a1)
    -> (Word16.t mask -> 'a1) -> (Word16.t mask -> 'a1) -> (Word16.t -> 'a1)
    -> (Word32.t mask -> 'a1) -> (Word32.t mask -> 'a1) -> (Word48.t mask ->
    'a1) -> (Word48.t mask -> 'a1) -> (Word8.t -> 'a1) -> (Word8.t -> 'a1) ->
    (Word32.t -> 'a1) -> (Word8.t -> 'a1) -> (Word64.t mask -> 'a1) -> oxm ->
    'a1 **)

let oxm_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 = function
| OxmInPort x -> f x
| OxmInPhyPort x -> f0 x
| OxmMetadata x -> f1 x
| OxmEthType x -> f2 x
| OxmEthDst x -> f3 x
| OxmEthSrc x -> f4 x
| OxmVlanVId x -> f5 x
| OxmVlanPcp x -> f6 x
| OxmIPProto x -> f7 x
| OxmIPDscp x -> f8 x
| OxmIPEcn x -> f9 x
| OxmIP4Src x -> f10 x
| OxmIP4Dst x -> f11 x
| OxmTCPSrc x -> f12 x
| OxmTCPDst x -> f13 x
| OxmARPOp x -> f14 x
| OxmARPSpa x -> f15 x
| OxmARPTpa x -> f16 x
| OxmARPSha x -> f17 x
| OxmARPTha x -> f18 x
| OxmICMPType x -> f19 x
| OxmICMPCode x -> f20 x
| OxmMPLSLabel x -> f21 x
| OxmMPLSTc x -> f22 x
| OxmTunnelId x -> f23 x

type oxmMatch = oxm list

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of Word16.t
| Any

(** val pseudoPort_rect :
    (portId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (Word16.t -> 'a1) -> 'a1 ->
    pseudoPort -> 'a1 **)

let pseudoPort_rect f f0 f1 f2 f3 f4 = function
| PhysicalPort x -> f x
| InPort -> f0
| Flood -> f1
| AllPorts -> f2
| Controller x -> f3 x
| Any -> f4

(** val pseudoPort_rec :
    (portId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (Word16.t -> 'a1) -> 'a1 ->
    pseudoPort -> 'a1 **)

let pseudoPort_rec f f0 f1 f2 f3 f4 = function
| PhysicalPort x -> f x
| InPort -> f0
| Flood -> f1
| AllPorts -> f2
| Controller x -> f3 x
| Any -> f4

type action =
| Output of pseudoPort
| Group of groupId
| PopVlan
| PushVlan
| PopMpls
| PushMpls
| SetField of oxm

(** val action_rect :
    (pseudoPort -> 'a1) -> (groupId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    (oxm -> 'a1) -> action -> 'a1 **)

let action_rect f f0 f1 f2 f3 f4 f5 = function
| Output x -> f x
| Group x -> f0 x
| PopVlan -> f1
| PushVlan -> f2
| PopMpls -> f3
| PushMpls -> f4
| SetField x -> f5 x

(** val action_rec :
    (pseudoPort -> 'a1) -> (groupId -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    (oxm -> 'a1) -> action -> 'a1 **)

let action_rec f f0 f1 f2 f3 f4 f5 = function
| Output x -> f x
| Group x -> f0 x
| PopVlan -> f1
| PushVlan -> f2
| PopMpls -> f3
| PushMpls -> f4
| SetField x -> f5 x

type actionSequence = action list

type instruction =
| GotoTable of tableId
| ApplyActions of actionSequence
| WriteActions of actionSequence

(** val instruction_rect :
    (tableId -> 'a1) -> (actionSequence -> 'a1) -> (actionSequence -> 'a1) ->
    instruction -> 'a1 **)

let instruction_rect f f0 f1 = function
| GotoTable x -> f x
| ApplyActions x -> f0 x
| WriteActions x -> f1 x

(** val instruction_rec :
    (tableId -> 'a1) -> (actionSequence -> 'a1) -> (actionSequence -> 'a1) ->
    instruction -> 'a1 **)

let instruction_rec f f0 f1 = function
| GotoTable x -> f x
| ApplyActions x -> f0 x
| WriteActions x -> f1 x

type bucket = { bu_weight : Word16.t; bu_watch_port : portId option;
                bu_watch_group : groupId option; bu_actions : actionSequence }

(** val bucket_rect :
    (Word16.t -> portId option -> groupId option -> actionSequence -> 'a1) ->
    bucket -> 'a1 **)

let bucket_rect f b =
  let { bu_weight = x; bu_watch_port = x0; bu_watch_group = x1; bu_actions =
    x2 } = b
  in
  f x x0 x1 x2

(** val bucket_rec :
    (Word16.t -> portId option -> groupId option -> actionSequence -> 'a1) ->
    bucket -> 'a1 **)

let bucket_rec f b =
  let { bu_weight = x; bu_watch_port = x0; bu_watch_group = x1; bu_actions =
    x2 } = b
  in
  f x x0 x1 x2

(** val bu_weight : bucket -> Word16.t **)

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

(** val groupType_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> groupType -> 'a1 **)

let groupType_rect f f0 f1 f2 = function
| All -> f
| Select -> f0
| Indirect -> f1
| FF -> f2

(** val groupType_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> groupType -> 'a1 **)

let groupType_rec f f0 f1 f2 = function
| All -> f
| Select -> f0
| Indirect -> f1
| FF -> f2

type groupMod =
| AddGroup of groupType * groupId * bucket list
| DeleteGroup of groupType * groupId

(** val groupMod_rect :
    (groupType -> groupId -> bucket list -> 'a1) -> (groupType -> groupId ->
    'a1) -> groupMod -> 'a1 **)

let groupMod_rect f f0 = function
| AddGroup (x, x0, x1) -> f x x0 x1
| DeleteGroup (x, x0) -> f0 x x0

(** val groupMod_rec :
    (groupType -> groupId -> bucket list -> 'a1) -> (groupType -> groupId ->
    'a1) -> groupMod -> 'a1 **)

let groupMod_rec f f0 = function
| AddGroup (x, x0, x1) -> f x x0 x1
| DeleteGroup (x, x0) -> f0 x x0

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

type flowModFlags = { fmf_send_flow_rem : bool; fmf_check_overlap : bool;
                      fmf_reset_counts : bool; fmf_no_pkt_counts : bool;
                      fmf_no_byt_counts : bool }

(** val flowModFlags_rect :
    (bool -> bool -> bool -> bool -> bool -> 'a1) -> flowModFlags -> 'a1 **)

let flowModFlags_rect f f0 =
  let { fmf_send_flow_rem = x; fmf_check_overlap = x0; fmf_reset_counts = x1;
    fmf_no_pkt_counts = x2; fmf_no_byt_counts = x3 } = f0
  in
  f x x0 x1 x2 x3

(** val flowModFlags_rec :
    (bool -> bool -> bool -> bool -> bool -> 'a1) -> flowModFlags -> 'a1 **)

let flowModFlags_rec f f0 =
  let { fmf_send_flow_rem = x; fmf_check_overlap = x0; fmf_reset_counts = x1;
    fmf_no_pkt_counts = x2; fmf_no_byt_counts = x3 } = f0
  in
  f x x0 x1 x2 x3

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

type flowMod = { mfCookie : Word64.t mask; mfTable_id : tableId;
                 mfCommand : flowModCommand; mfIdle_timeout : timeout;
                 mfHard_timeout : timeout; mfPriority : Word16.t;
                 mfBuffer_id : bufferId option;
                 mfOut_port : pseudoPort option;
                 mfOut_group : groupId option; mfFlags : flowModFlags;
                 mfOfp_match : oxmMatch; mfInstructions : instruction list }

(** val flowMod_rect :
    (Word64.t mask -> tableId -> flowModCommand -> timeout -> timeout ->
    Word16.t -> bufferId option -> pseudoPort option -> groupId option ->
    flowModFlags -> oxmMatch -> instruction list -> 'a1) -> flowMod -> 'a1 **)

let flowMod_rect f f0 =
  let { mfCookie = x; mfTable_id = x0; mfCommand = x1; mfIdle_timeout = x2;
    mfHard_timeout = x3; mfPriority = x4; mfBuffer_id = x5; mfOut_port = x6;
    mfOut_group = x7; mfFlags = x8; mfOfp_match = x9; mfInstructions =
    x10 } = f0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

(** val flowMod_rec :
    (Word64.t mask -> tableId -> flowModCommand -> timeout -> timeout ->
    Word16.t -> bufferId option -> pseudoPort option -> groupId option ->
    flowModFlags -> oxmMatch -> instruction list -> 'a1) -> flowMod -> 'a1 **)

let flowMod_rec f f0 =
  let { mfCookie = x; mfTable_id = x0; mfCommand = x1; mfIdle_timeout = x2;
    mfHard_timeout = x3; mfPriority = x4; mfBuffer_id = x5; mfOut_port = x6;
    mfOut_group = x7; mfFlags = x8; mfOfp_match = x9; mfInstructions =
    x10 } = f0
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

(** val mfCookie : flowMod -> Word64.t mask **)

let mfCookie x = x.mfCookie

(** val mfTable_id : flowMod -> tableId **)

let mfTable_id x = x.mfTable_id

(** val mfCommand : flowMod -> flowModCommand **)

let mfCommand x = x.mfCommand

(** val mfIdle_timeout : flowMod -> timeout **)

let mfIdle_timeout x = x.mfIdle_timeout

(** val mfHard_timeout : flowMod -> timeout **)

let mfHard_timeout x = x.mfHard_timeout

(** val mfPriority : flowMod -> Word16.t **)

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

(** val packetInReason_rect : 'a1 -> 'a1 -> packetInReason -> 'a1 **)

let packetInReason_rect f f0 = function
| NoMatch -> f
| ExplicitSend -> f0

(** val packetInReason_rec : 'a1 -> 'a1 -> packetInReason -> 'a1 **)

let packetInReason_rec f f0 = function
| NoMatch -> f
| ExplicitSend -> f0

type packetIn = { pi_buffer_id : Word32.t option; pi_total_len : Word16.t;
                  pi_reason : packetInReason; pi_table_id : tableId;
                  pi_cookie : Word64.t; pi_ofp_match : oxmMatch;
                  pi_pkt : packet option }

(** val packetIn_rect :
    (Word32.t option -> Word16.t -> packetInReason -> tableId -> Word64.t ->
    oxmMatch -> packet option -> 'a1) -> packetIn -> 'a1 **)

let packetIn_rect f p =
  let { pi_buffer_id = x; pi_total_len = x0; pi_reason = x1; pi_table_id =
    x2; pi_cookie = x3; pi_ofp_match = x4; pi_pkt = x5 } = p
  in
  f x x0 x1 x2 x3 x4 x5

(** val packetIn_rec :
    (Word32.t option -> Word16.t -> packetInReason -> tableId -> Word64.t ->
    oxmMatch -> packet option -> 'a1) -> packetIn -> 'a1 **)

let packetIn_rec f p =
  let { pi_buffer_id = x; pi_total_len = x0; pi_reason = x1; pi_table_id =
    x2; pi_cookie = x3; pi_ofp_match = x4; pi_pkt = x5 } = p
  in
  f x x0 x1 x2 x3 x4 x5

(** val pi_buffer_id : packetIn -> Word32.t option **)

let pi_buffer_id x = x.pi_buffer_id

(** val pi_total_len : packetIn -> Word16.t **)

let pi_total_len x = x.pi_total_len

(** val pi_reason : packetIn -> packetInReason **)

let pi_reason x = x.pi_reason

(** val pi_table_id : packetIn -> tableId **)

let pi_table_id x = x.pi_table_id

(** val pi_cookie : packetIn -> Word64.t **)

let pi_cookie x = x.pi_cookie

(** val pi_ofp_match : packetIn -> oxmMatch **)

let pi_ofp_match x = x.pi_ofp_match

(** val pi_pkt : packetIn -> packet option **)

let pi_pkt x = x.pi_pkt

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; group_stats : bool; ip_reasm : 
                      bool; queue_stats : bool; port_blocked : bool }

(** val capabilities_rect :
    (bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
    capabilities -> 'a1 **)

let capabilities_rect f c =
  let { flow_stats = x; table_stats = x0; port_stats = x1; group_stats = x2;
    ip_reasm = x3; queue_stats = x4; port_blocked = x5 } = c
  in
  f x x0 x1 x2 x3 x4 x5

(** val capabilities_rec :
    (bool -> bool -> bool -> bool -> bool -> bool -> bool -> 'a1) ->
    capabilities -> 'a1 **)

let capabilities_rec f c =
  let { flow_stats = x; table_stats = x0; port_stats = x1; group_stats = x2;
    ip_reasm = x3; queue_stats = x4; port_blocked = x5 } = c
  in
  f x x0 x1 x2 x3 x4 x5

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

type features = { datapath_id : Word64.t; num_buffers : Word32.t;
                  num_tables : Word8.t; aux_id : Word8.t;
                  supported_capabilities : capabilities }

(** val features_rect :
    (Word64.t -> Word32.t -> Word8.t -> Word8.t -> capabilities -> 'a1) ->
    features -> 'a1 **)

let features_rect f f0 =
  let { datapath_id = x; num_buffers = x0; num_tables = x1; aux_id = x2;
    supported_capabilities = x3 } = f0
  in
  f x x0 x1 x2 x3

(** val features_rec :
    (Word64.t -> Word32.t -> Word8.t -> Word8.t -> capabilities -> 'a1) ->
    features -> 'a1 **)

let features_rec f f0 =
  let { datapath_id = x; num_buffers = x0; num_tables = x1; aux_id = x2;
    supported_capabilities = x3 } = f0
  in
  f x x0 x1 x2 x3

(** val datapath_id : features -> Word64.t **)

let datapath_id x = x.datapath_id

(** val num_buffers : features -> Word32.t **)

let num_buffers x = x.num_buffers

(** val num_tables : features -> Word8.t **)

let num_tables x = x.num_tables

(** val aux_id : features -> Word8.t **)

let aux_id x = x.aux_id

(** val supported_capabilities : features -> capabilities **)

let supported_capabilities x = x.supported_capabilities

type packetOut = { po_buffer_id : bufferId option; po_in_port : pseudoPort;
                   po_actions : actionSequence; po_pkt : packet option }

(** val packetOut_rect :
    (bufferId option -> pseudoPort -> actionSequence -> packet option -> 'a1)
    -> packetOut -> 'a1 **)

let packetOut_rect f p =
  let { po_buffer_id = x; po_in_port = x0; po_actions = x1; po_pkt = x2 } = p
  in
  f x x0 x1 x2

(** val packetOut_rec :
    (bufferId option -> pseudoPort -> actionSequence -> packet option -> 'a1)
    -> packetOut -> 'a1 **)

let packetOut_rec f p =
  let { po_buffer_id = x; po_in_port = x0; po_actions = x1; po_pkt = x2 } = p
  in
  f x x0 x1 x2

(** val po_buffer_id : packetOut -> bufferId option **)

let po_buffer_id x = x.po_buffer_id

(** val po_in_port : packetOut -> pseudoPort **)

let po_in_port x = x.po_in_port

(** val po_actions : packetOut -> actionSequence **)

let po_actions x = x.po_actions

(** val po_pkt : packetOut -> packet option **)

let po_pkt x = x.po_pkt

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
| BarrierRequest
| BarrierReply

(** val message_rect :
    'a1 -> (bytes -> 'a1) -> (bytes -> 'a1) -> 'a1 -> (features -> 'a1) ->
    (flowMod -> 'a1) -> (groupMod -> 'a1) -> (packetIn -> 'a1) -> (packetOut
    -> 'a1) -> 'a1 -> 'a1 -> message -> 'a1 **)

let message_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
| Hello -> f
| EchoRequest x -> f0 x
| EchoReply x -> f1 x
| FeaturesRequest -> f2
| FeaturesReply x -> f3 x
| FlowModMsg x -> f4 x
| GroupModMsg x -> f5 x
| PacketInMsg x -> f6 x
| PacketOutMsg x -> f7 x
| BarrierRequest -> f8
| BarrierReply -> f9

(** val message_rec :
    'a1 -> (bytes -> 'a1) -> (bytes -> 'a1) -> 'a1 -> (features -> 'a1) ->
    (flowMod -> 'a1) -> (groupMod -> 'a1) -> (packetIn -> 'a1) -> (packetOut
    -> 'a1) -> 'a1 -> 'a1 -> message -> 'a1 **)

let message_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 = function
| Hello -> f
| EchoRequest x -> f0 x
| EchoReply x -> f1 x
| FeaturesRequest -> f2
| FeaturesReply x -> f3 x
| FlowModMsg x -> f4 x
| GroupModMsg x -> f5 x
| PacketInMsg x -> f6 x
| PacketOutMsg x -> f7 x
| BarrierRequest -> f8
| BarrierReply -> f9

