(** A uniform interface to OpenFlow 1.0 and 1.3.

  A high-level language, such as Frenetic, should support OpenFlow 1.0
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
  actions translates to 1.0 action sequences and controller round-trips
  if needed. When targeting OpenFlow 1.3, action also builds group
  tables to realize actions efficiently. This requires a global analysis
  of all the actions in a flow table. Therefore, Frenetic needs to
  supply the entire flow table at once and cannot add and remove flow table
  entries individually. *)

(** {1 OpenFlow Identifier Types}

  OpenFlow requires identifiers for switches, ports, transaction numbers, etc.
  The representation of these identifiers varies across different versions
  of OpenFlow, which is why they are abstract.


*)

open Packet

type switchId = int64
type portId = int32
type queueId = int32

type bufferId = int32

exception Unsupported of string

(** {1 Packet Forwarding} *)

(** WARNING: There are dependencies between different fields that must be met. *)
type pattern =
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan option
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr option
    ; nwDst : nwAddr option
    ; nwProto : nwProto option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option }

val all_pattern : pattern

type modify =
  | SetEthSrc of dlAddr
  | SetEthDst of dlAddr
  | SetVlan of dlVlan
  | SetVlanPcp of dlVlanPcp
  | SetEthTyp of dlTyp
  | SetIPProto of nwProto
  | SetIP4Src of nwAddr
  | SetIP4Dst of nwAddr
  | SetTCPSrcPort of tpPort
  | SetTCPDstPort of tpPort

type action =
  | OutputAllPorts
  | OutputPort of portId
  | Controller of int
  | Enqueue of portId * queueId
  | Modify of modify
 
type seq = action list

type par = seq list

type group = par list

type timeout =
  | Permanent (** No timeout. *)
  | ExpiresAfter of int16 (** Time out after [n] seconds. *)

type flow = {
  pattern: pattern;
  action: group;
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

(** [(payload, total_length, in_port, reason)] *)
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

(* {1 Errors} *)

(* TODO: FILL *)

(* {1 Pretty-printing } *)

val format_action : Format.formatter -> action -> unit
val format_seq : Format.formatter -> seq -> unit
val format_par : Format.formatter -> par -> unit
val format_group : Format.formatter -> group -> unit
val format_pattern : Format.formatter -> pattern -> unit
val format_flow : Format.formatter -> flow -> unit
val format_flowTable : Format.formatter -> flowTable -> unit

val string_of_action : action -> string
val string_of_seq : seq -> string
val string_of_pattern : pattern -> string
val string_of_flow : flow -> string
val string_of_flowTable : flowTable -> string
