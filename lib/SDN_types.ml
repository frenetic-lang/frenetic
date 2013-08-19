module OF10 = OpenFlow0x01_Core
module OF13 = OpenFlow0x04_Core

exception Unsupported of string

type int8 = int
type int12 = int
type int16 = int
type int32 = Int32.t
type int64 = Int64.t
type int48 = Int64.t
type bytes = string

type switchId =
  | OF10SwitchId of OF10.switchId
  | OF13SwitchId of OF13.switchId

type portId =
  | OF10PortId of OF10.portId
  | OF13PortId of OF13.portId

type bufferId =
  | OF10BufferId of int32
  | OF13BufferId of OF13.bufferId

type port =
  | PhysicalPort of portId
  | AllPorts
  | Controller of int

type field =
  | InPort of portId
  | EthType of int16
  | EthSrc of int48
  | EthDst of int48
  | Vlan of int12
  | VlanPcp of int8
  | IPProto of int8
  | IP4Src of int32
  | IP4Dst of int32
  | TCPSrcPort of int16
  | TCPDstPort of int16

type pattern = {
  inPort : portId option;
  ethType : int16 option;
  ethSrc : int48 option;
  ethDst : int48 option;
  vlan: int12 option;
  vlanPcp: int8 option;
  ipProto: int8 option;
  ip4Src: int32 option;
  ip4Dst: int32 option;
  tcpSrcPort: int16 option;
  tcpDstPort: int16 option
}

let pattern_all = {
  inPort = None;
  ethType = None;
  ethSrc = None;
  ethDst = None;
  vlan = None;
  vlanPcp = None;
  ipProto = None;
  ip4Src = None;
  ip4Dst = None;
  tcpSrcPort = None;
  tcpDstPort = None
}

let pattern_of_field (f : field) : pattern =
	failwith "NYI"

let pattern_inter (p1 : pattern) (p2 : pattern) : pattern option =
	failwith "NYI"

type action =
  | OutputAllPorts
  | OutputPort of portId
  | SetField of field
  | Seq of action * action (** directly corresponds to an _action sequence_ *)
  | Par of action * action 
    (** Either (i) compile to an action sequence, (ii) compile to an OpenFlow
        1.3 group of type "all", which clones the packet for each group, or
        (iii) signal an exception. *)
  | Failover of portId * action * action
    (** [Failover (watchPort, a1, a2)] uses OpenFlow 1.3 fast-failover to 
        move from [a1] to [a2] if [watchPort] goes down. *)

type timeout =
  | Permanent (** No timeout. *)
  | ExpiresAfter of int16 (** Time out after [n] seconds. *)

type flow = {
  pattern: pattern;
  action: action;
  cookie: int64;
  idle_timeout: timeout;
  hard_timeout: timeout
}

(** Priorities are implicit *)
type flowTable = flow list 

(** {1 Controller Packet Processing} *)

(** The payload for [packetIn] and [packetOut] messages. *)
type payload =
  | Buffered of bufferId * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

type pktIn = payload * int * portId * packetInReason

(* {1 Switch Configuration} *)

(** A simplification of the _switch features_ message from OpenFlow *)
type switchFeatures = {
  switch_id : switchId;
  switch_ports : portId list
}

(* {1 Statistics} *)

(** The body of a reply to an individual flow statistics request. *)
type flowStats = {
  flow_table_id : int8; (** ID of table flow came from. *)
  flow_pattern : pattern;
  flow_duration_sec: int32;
  flow_duration_nsec: int32;
  flow_priority: int16;
  flow_idle_timeout: int16;
  flow_hard_timeout: int16;
  flow_action: action;
  flow_packet_count: int64;
  flow_byte_count: int64
}


module type SWITCH = sig
  type t
  val setup_flow_table : t -> flowTable -> unit Lwt.t
  val flow_stats_request : t -> pattern -> flowStats list Lwt.t
  val packet_in : t -> pktIn Lwt_stream.t
  val packet_out : t -> payload -> action -> unit Lwt.t
  val disconnect : t -> unit Lwt.t
  val features : t -> switchFeatures  
end