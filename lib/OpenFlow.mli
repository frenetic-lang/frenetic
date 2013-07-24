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
type flow_table = flow list 

(** [setup_flow_table sw tbl] returns after [sw] is configured to implement 
    [tbl]. [setup_flow_table] fails if [sw] runs a version of OpenFlow that
	does not support the features that [tbl] requires. *)
val setup_flow_table : switchId -> flow_table -> unit Lwt.t

(** {1 Controller Packet Processing} *)

(** The payload for [packetIn] and [packetOut] messages. *)
type payload =
  | Buffered of bufferId * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

type packetIn = {
  input_payload: payload;
  total_len: int16;
  input_port: portId;
  reason: packetInReason
}

type packetOut = {
  output_payload : payload;
  apply_action: action;
  (** If the action requires a group table entry, it will only be realized
  	  if a compatible group table exists. *)
}

(* {1 Switch Configuration} *)

(** A simplification of the _switch features_ message from OpenFlow *)
type switch = {
  switch_id : switchId;
  switch_ports : portId list
}


(* {1 Statistics} *)

[FILL]

(* {1 Errors} *)

[FILL]

