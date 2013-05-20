open NetworkPacket
open Word
open Misc

type of_match = { 
  matchDlSrc : dlAddr option; 
  matchDlDst : dlAddr option;
  matchDlTyp : dlTyp option; 
  matchDlVlan : dlVlan option;
  matchDlVlanPcp : dlVlanPcp option;
  matchNwSrc : nwAddr option; 
  matchNwDst : nwAddr option;
  matchNwProto : nwProto option; 
  matchNwTos : nwTos option;
  matchTpSrc : tpPort option; 
  matchTpDst : tpPort option;
  matchInPort : portId option }

val matchDlSrc : of_match -> dlAddr option

val matchDlDst : of_match -> dlAddr option

val matchDlTyp : of_match -> dlTyp option

val matchDlVlan : of_match -> dlVlan option

val matchDlVlanPcp : of_match -> dlVlanPcp option

val matchNwSrc : of_match -> nwAddr option

val matchNwDst : of_match -> nwAddr option

val matchNwProto : of_match -> nwProto option

val matchNwTos : of_match -> nwTos option

val matchTpSrc : of_match -> tpPort option

val matchTpDst : of_match -> tpPort option

val matchInPort : of_match -> portId option

type capabilities = 
  { flow_stats : bool; 
    table_stats : bool;
    port_stats : bool; 
    stp : bool; 
    ip_reasm : bool;
    queue_stats : bool; 
    arp_match_ip : bool }

val flow_stats : capabilities -> bool

val table_stats : capabilities -> bool

val port_stats : capabilities -> bool

val stp : capabilities -> bool

val ip_reasm : capabilities -> bool

val queue_stats : capabilities -> bool

val arp_match_ip : capabilities -> bool

type actions = 
  { output : bool; 
    set_vlan_id : bool; 
    set_vlan_pcp : bool;
    strip_vlan : bool; 
    set_dl_src : bool; 
    set_dl_dst : bool;
    set_nw_src : bool; 
    set_nw_dst : bool; 
    set_nw_tos : bool;
    set_tp_src : bool; 
    set_tp_dst : bool; 
    enqueue : bool;
    vendor : bool }

val output : actions -> bool

val set_vlan_id : actions -> bool

val set_vlan_pcp : actions -> bool

val strip_vlan : actions -> bool

val set_dl_src : actions -> bool

val set_dl_dst : actions -> bool

val set_nw_src : actions -> bool

val set_nw_dst : actions -> bool

val set_nw_tos : actions -> bool

val set_tp_src : actions -> bool

val set_tp_dst : actions -> bool

val enqueue : actions -> bool

val vendor : actions -> bool

type features = 
  { switch_id : Word64.t; 
    num_buffers : Word32.t;
    num_tables : Word8.t;
    supported_capabilities : capabilities;
    supported_actions : actions }

val switch_id : features -> Word64.t

val num_buffers : features -> Word32.t

val num_tables : features -> Word8.t

val supported_capabilities : features -> capabilities

val supported_actions : features -> actions

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type switchId = Word64.t

val string_of_switchId : switchId -> string

type priority = Word16.t

type bufferId = Word32.t

type pseudoPort =
| PhysicalPort of portId
| InPort
| Flood
| AllPorts
| Controller of Word16.t

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

type actionSequence = action list

type timeout =
| Permanent
| ExpiresAfter of Word16.t

type flowMod = 
  { mfModCmd : flowModCommand; 
    mfMatch : of_match;
    mfPriority : priority; 
    mfActions : actionSequence;
    mfCookie : Word64.t; 
    mfIdleTimeOut : timeout;
    mfHardTimeOut : timeout; 
    mfNotifyWhenRemoved : bool;
    mfApplyToPacket : bufferId option;
    mfOutPort : pseudoPort option; 
    mfCheckOverlap : bool }

val mfModCmd : flowMod -> flowModCommand

val mfMatch : flowMod -> of_match

val mfPriority : flowMod -> priority

val mfActions : flowMod -> actionSequence

val mfCookie : flowMod -> Word64.t

val mfIdleTimeOut : flowMod -> timeout

val mfHardTimeOut : flowMod -> timeout

val mfNotifyWhenRemoved : flowMod -> bool

val mfApplyToPacket : flowMod -> bufferId option

val mfOutPort : flowMod -> pseudoPort option

val mfCheckOverlap : flowMod -> bool

type reason =
| NoMatch
| ExplicitSend

type packetIn = 
  { packetInBufferId : bufferId option;
    packetInTotalLen : Word16.t; 
    packetInPort : portId;
    packetInReason : reason; 
    packetInPacket : packet }
    
val packetInBufferId : packetIn -> bufferId option

val packetInTotalLen : packetIn -> Word16.t

val packetInPort : packetIn -> portId

val packetInReason : packetIn -> reason

val packetInPacket : packetIn -> packet

type xid = Word32.t

type packetOut = 
  { pktOutBufOrBytes : (bufferId, bytes) sum;
    pktOutPortId : portId option;
    pktOutActions : actionSequence }
    
val pktOutBufOrBytes : packetOut -> (bufferId, bytes) sum

val pktOutPortId : packetOut -> portId option

val pktOutActions : packetOut -> actionSequence

(* Component types of stats_request messages. *)
type table_id = Word8.t

module IndividualFlowRequest : sig
  type t = { of_match : of_match;
             table_id : table_id;
             port : pseudoPort }
end
  
module AggregateFlowRequest : sig
  type t = { of_match : of_match;
             table_id : table_id;
             port : pseudoPort }
end
  
(* Component types of stats_reply messages. *)

module DescriptionStats : sig
  type t = { manufacturer : string;
             hardware : string;
             software : string;
             serial_number : string;
             datapath : string }
end

module IndividualFlowStats : sig
    type t = { table_id : table_id;
               of_match : of_match;
               duration_sec : int;
               duration_msec : int;
               priority : int;
               idle_timeout : int;
               hard_timeout : int;
               cookie : int;
               byte_count : int;
               actions : actionSequence }
end

module AggregateFlowStats : sig
  type t = { packet_count : int;
             byte_count : int;
             flow_count : int }
end

module TableStats : sig
    type t = { table_id : table_id;
               name : string;
               wildcards : Word32.t;
               max_entries : int;
               active_count : int;
               lookup_count : int;
               matched_count : int }
             
end

module PortStats : sig
  type t = { port_no : pseudoPort;
             rx_packets : int;
             tx_packets : int;
             rx_bytes : int;
             tx_bytes : int;
             rx_dropped : int;
             tx_dropped : int;
             rx_errors : int;
             tx_errors : int;
             rx_frame_err : int;
             rx_over_err : int;
             rx_crc_err : int;
             collisions : int }
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

