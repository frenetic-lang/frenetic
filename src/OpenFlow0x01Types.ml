open NetworkPacket
open Word

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type of_match = { matchDlSrc : dlAddr option; matchDlDst : dlAddr option;
                  matchDlTyp : dlTyp option; matchDlVlan : dlVlan option;
                  matchDlVlanPcp : dlVlanPcp option;
                  matchNwSrc : nwAddr option; matchNwDst : nwAddr option;
                  matchNwProto : nwProto option; matchNwTos : nwTos option;
                  matchTpSrc : tpPort option; matchTpDst : tpPort option;
                  matchInPort : portId option }

let matchDlSrc x = x.matchDlSrc

let matchDlDst x = x.matchDlDst

let matchDlTyp x = x.matchDlTyp

let matchDlVlan x = x.matchDlVlan

let matchDlVlanPcp x = x.matchDlVlanPcp

let matchNwSrc x = x.matchNwSrc

let matchNwDst x = x.matchNwDst

let matchNwProto x = x.matchNwProto

let matchNwTos x = x.matchNwTos

let matchTpSrc x = x.matchTpSrc

let matchTpDst x = x.matchTpDst

let matchInPort x = x.matchInPort

type capabilities = { flow_stats : bool; table_stats : bool;
                      port_stats : bool; stp : bool; ip_reasm : bool;
                      queue_stats : bool; arp_match_ip : bool }

let flow_stats x = x.flow_stats

let table_stats x = x.table_stats

let port_stats x = x.port_stats

let stp x = x.stp

let ip_reasm x = x.ip_reasm

let queue_stats x = x.queue_stats

let arp_match_ip x = x.arp_match_ip

type actions = { output : bool; set_vlan_id : bool; set_vlan_pcp : bool;
                 strip_vlan : bool; set_dl_src : bool; set_dl_dst : bool;
                 set_nw_src : bool; set_nw_dst : bool; set_nw_tos : bool;
                 set_tp_src : bool; set_tp_dst : bool; enqueue : bool;
                 vendor : bool }

let output x = x.output

let set_vlan_id x = x.set_vlan_id

let set_vlan_pcp x = x.set_vlan_pcp

let strip_vlan x = x.strip_vlan

let set_dl_src x = x.set_dl_src

let set_dl_dst x = x.set_dl_dst

let set_nw_src x = x.set_nw_src

let set_nw_dst x = x.set_nw_dst

let set_nw_tos x = x.set_nw_tos

let set_tp_src x = x.set_tp_src

let set_tp_dst x = x.set_tp_dst

let enqueue x = x.enqueue

let vendor x = x.vendor

type features = { switch_id : Word64.t; num_buffers : Word32.t;
                  num_tables : Word8.t;
                  supported_capabilities : capabilities;
                  supported_actions : actions }

let switch_id x = x.switch_id

let num_buffers x = x.num_buffers

let num_tables x = x.num_tables

let supported_capabilities x = x.supported_capabilities

let supported_actions x = x.supported_actions

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type switchId = Word64.t

let string_of_switchId = Word64.to_string

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

let mfModCmd x = x.mfModCmd

let mfMatch x = x.mfMatch

let mfPriority x = x.mfPriority

let mfActions x = x.mfActions

let mfCookie x = x.mfCookie

let mfIdleTimeOut x = x.mfIdleTimeOut

let mfHardTimeOut x = x.mfHardTimeOut

let mfNotifyWhenRemoved x = x.mfNotifyWhenRemoved

let mfApplyToPacket x = x.mfApplyToPacket

let mfOutPort x = x.mfOutPort

let mfCheckOverlap x = x.mfCheckOverlap

type packetInReason =
| NoMatch
| ExplicitSend

type packetIn = 
  { packetInBufferId : bufferId option;
    packetInTotalLen : Word16.t; 
    packetInPort : portId;
    packetInReason_ : packetInReason; 
    packetInPacket : packet }

let packetInBufferId x = x.packetInBufferId

let packetInTotalLen x = x.packetInTotalLen

let packetInPort x = x.packetInPort

let packetInReason_ x = x.packetInReason_

let packetInPacket x = x.packetInPacket

type xid = Word32.t

type packetOut = 
  { pktOutBufOrBytes : (bufferId, bytes) Misc.sum;
    pktOutPortId : portId option;
    pktOutActions : actionSequence }

let pktOutBufOrBytes x = x.pktOutBufOrBytes

let pktOutPortId x = x.pktOutPortId

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
