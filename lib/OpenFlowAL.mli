(** A uniform interface for programming switches that can use both OpenFlow 1.0
	and OpenFlow 1.3 as its back-end. *)

(** {1 OpenFlow Identifier Types}

  OpenFlow requires identifiers for switches, ports, transaction numbers, etc.
  The representation of these identifiers varies across different versions
  of OpenFlow, which is why they are abstract.
*)

type switchId
type portId
type queueId
type bufferId
type xid

(** {1 Packet Forwarding} *)

type port =
  | PhysicalPort of portId
  | AllPorts
  | Controller of int

type field =
  | InPort of port
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

(** Abstract pattern that matches on fields *)
type pattern

val pattern_of_field : field -> pattern

(** [pattern_inter pat1 pat2] may fail if the two patterns are disjoint. *)
val pattern_inter : pattern -> pattern -> pattern option

(** A high-level language, such as Frenetic, should support OpenFlow 1.0
	and also exploit OpenFlow 1.3 features when possible. For example,
	when two Frenetic actions are composed in parallel, they logically work
	on two copies of a packet. Certain kinds of parallel composition cannot
	be realized in OpenFlow 1.0, but they are trivial to implement with
	group tables in OpenFlow 1.3.

	Similarly, OpenFlow 1.3 can implement failover efficiently using fast-
	failover groups. But, in OpenFlow 1.0, we have to incur a round-trip
	to the controller.

	Instead of creating two different versions of the Frenetic compiler, we
	here define a high-level action data type. When targeting OpenFlow 1.0,
	action translates to 1.0 action sequences and controller round-trips
	if needed. When targeting OpenFlow 1.3, action also builds group
    tables to realize actions efficiently. This requires a global analysis
    of all the actions in a flow table. Therefore, Frenetic needs to
    supply the entire flow table at once and cannot add and remove flow table
	entries individually. *)
type action =
  | OutputAllPorts
  | OutputPort of portId
  | SetField of field
  | Seq of action * action (** directly corresponds to an _action sequence_ *)
  | Par of action * action 
    (** Either (i) compile to an action sequence, (ii) compile to an OpenFlow
        1.3 group of type "all", which clones the packet for each group, or
        (iii) signal an exception. *)
  | Failover of (portId * action) list
    (** [Failover actions] uses OpenFlow 1.3 fast-failover to move between
        the actions as ports go down. *)

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
type flow_table = flow list 

(** {1 Controller Packet Processing} *)

(** The payload for [packetIn] and [packetOut] messages. *)
type payload =
  | Buffered of bufferId * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

(* {1 Switch Configuration} *)

(** A simplification of the _switch features_ message from OpenFlow *)
type switch = {
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

(* {1 Errors} *)

(* TODO: FILL *)

(* {1 Abstract OpenFlow API} *)

 (** [setup_flow_table sw tbl] returns after [sw] is configured to implement 
  [tbl]. [setup_flow_table] fails if [sw] runs a version of OpenFlow that
  does not support the features that [tbl] requires. *)
val setup_flow_table : switchId -> flow_table -> unit Lwt.t

val accept_switch : unit -> switch Lwt.t

val flow_stats_request : switchId -> pattern -> list flowStats Lwt.t

val packet_in : switchId -> (payload * int16 * portId * packetInReason) Lwt.t

val packet_out : switchId -> payload -> action -> Lwt.t  
