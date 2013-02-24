(** Types for OpenFlow 1.3.1, based on
    
    https://www.opennetworking.org/images/stories/downloads/specification/openflow-spec-v1.3.1.pdf

    Using this as a guide:

    https://github.com/CPqD/ofsoftswitch13/blob/master/include/openflow/openflow.h

*)

open Cstruct

type uint48 = uint64
type uint12 = uint16
type bytes = string

type +'a mask = {
  value : 'a;
  mask : 'a option
}

type groupId = uint32
type portId = uint32
type tableId = uint8
type bufferId = uint32

(** See Table 11 of the specification *)
type oxm = 
  | OxmInPort of portId
  | OxmInPhyPort of portId
  | OxmEthType of uint16
  | OxmEthDst of uint48 mask
  | OxmEthSrc of uint48 mask
  | OxmVlanVId of uint12 mask

(**  Hard-codes OFPMT_OXM as the match type, since OFPMT_STANDARD is deprecated.
*)
type oxmMatch = oxm list

type pseudoPort =
  | PhysicalPort of portId
  | InPort
  | Flood
  | AllPorts
  | Controller of uint16 (* number of bytes to send *)

type action =
  | Output of pseudoPort
  | Group of groupId
  | SetField of oxm

type instruction =
  | GotoTable of int
  | WriteActions of action list

type bucket = {
  weight : uint16;
  watch_port : portId;
  watch_group : groupId;
  actions : action list
}

cenum groupType {
  All = 0; (* All (multicast/broadcast) group. *)
  Select = 1; (* Select group. *)
  Indirect = 2; (* Indirect group. *)
  FF = 3 (* Fast failover group. *)
} as uint16_t

type groupMod =
  | AddGroup of groupType * groupId * bucket list
  | DeleteGroup of groupType * groupId

type timeout =
| Permanent
| ExpiresAfter of uint16

cenum flowModCommand {
  AddFlow           = 0; (* New flow. *)
  ModFlow           = 1; (* Modify all matching flows. *)
  ModStrictFlow     = 2; (* Modify entry strictly matching wildcards and
                              priority. *)
  DeleteFlow        = 3; (* Delete all matching flows. *)
  DeleteStrictFlow  = 4  (* Delete entry strictly matching wildcards and
                              priority. *)
} as uint8_t

type flowModFlags = {
  send_flow_rem : bool;
  check_overlap : bool;
  reset_counts : bool;
  no_pkt_counts : bool;
  no_byt_counts : bool
}

type flowMod = { 
  cookie : uint64 mask;
  table_id : tableId;
  command : flowModCommand;
  idle_timeout : timeout;
  hard_timeout : timeout;
  priority : uint16;
  buffer_id : bufferId option;
  out_port : pseudoPort option;
  out_group : groupId option;
  flags : flowModFlags;
  ofp_match : oxmMatch;
  instructions : instruction list
}

type capabilities = {
  flow_stats : bool;
  table_stats : bool;
  port_stats : bool; 
  group_stats : bool;
  ip_reasm : bool;
  queue_stats : bool;
  port_blocked : bool
}

type features = {
  datapath_id : uint64; 
  num_buffers : uint32;
  num_tables : uint8;
  aux_id : uint8;
  supported_capabilities : capabilities
}

type message =
  | Hello
  | EchoRequest of bytes
  | EchoReply of bytes
  | FeaturesRequest
  | FeaturesReply of features
  | FlowMod of flowMod
  | GroupMod of groupMod
