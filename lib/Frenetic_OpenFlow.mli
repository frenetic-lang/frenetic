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
  entries individually *)

(** {1 OpenFlow Identifier Types}

  OpenFlow requires identifiers for switches, ports, transaction numbers, etc.
  The representation of these identifiers varies across different versions
  of OpenFlow, which is why they are abstract.


*)

open Frenetic_Packet

module OF10 = Frenetic_OpenFlow0x01

type switchId = int64 with sexp
type portId = int32 with sexp
type queueId = int32 with sexp
type bufferId = int32 with sexp

exception Unsupported of string

(** {1 Packet Forwarding} *)

module Pattern : sig

  module Ip : sig
    type t = nwAddr * int32 with sexp

    (** [match_all] is pattern that matches any address *)
    val match_all : t

    (** [less_eq x1 x2] returns true when [x2] matches any address that [x1] will
        match *)
    val less_eq : t -> t -> bool

    (** [eq p1 p2] returns true when [p1] and [p2] match the same set of
        addresses *)
    val eq : t -> t -> bool

    (** [join p1 p2] is the least pattern [pm] such that [less_eq p1 pm] and
        [less_eq p2 pm] *)
    val join : t -> t -> t

    (** [intersect x1 x2] returns the intersection of when [x1] and [x2] *)
    val intersect : t -> t -> t option

    (** [compatible x1 x2] returns true when [x1] and [x2] have a non-empty intersection *)
    val compatible : t -> t -> bool

    (** [ip_shift x] returns an [int32] after shifting [x] by its mask *)
    val shift : t -> int32

    val format : Format.formatter -> t -> unit
    val string_of : t -> string
  end

  (** WARNING: There are dependencies between different fields that must be met *)
  type t =
      { dlSrc : dlAddr option
      ; dlDst : dlAddr option
      ; dlTyp : dlTyp option
      ; dlVlan : dlVlan
      ; dlVlanPcp : dlVlanPcp option
      ; nwSrc : Ip.t option
      ; nwDst : Ip.t option
      ; nwProto : nwProto option
      ; tpSrc : tpPort option
      ; tpDst : tpPort option
      ; inPort : portId option }
    with sexp

  (** [match_all] is pattern that matches any packet *)
  val match_all : t

  (** [less_eq p1 p2] returns true when [p2] matches any packet that [p1] will
      match *)
  val less_eq : t -> t -> bool

  (** [eq p1 p2] returns true when [p1] and [p2] match the same set of packets *)
  val eq : t -> t -> bool

  (** [join p1 p2] is the least pattern [pm] such that [less_eq p1 pm] and
      [less_eq p2 pm] *)
  val join : t -> t -> t

  val format : Format.formatter -> t -> unit
  val string_of : t -> string
end

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
with sexp

type pseudoport =
  | Physical of portId
  | InPort
  | Table
  | Normal
  | Flood
  | All
  | Controller of int
  | Local
with sexp

type groupId = int32 with sexp

type action =
  | Output of pseudoport
  | Enqueue of portId * queueId
  | Modify of modify
  | FastFail of groupId
  with sexp

type seq = action list with sexp

type par = seq list with sexp

type group = par list with sexp

type timeout =
  | Permanent (** No timeout *)
  | ExpiresAfter of int16 (** Time out after [n] seconds *)
  with sexp

type flow = {
  pattern: Pattern.t;
  action: group;
  cookie: int64;
  idle_timeout: timeout;
  hard_timeout: timeout
} with sexp

(** Priorities are implicit *)
type flowTable = flow list with sexp

(** {1 Controller Packet Processing} *)

(** The payload for [packetIn] and [packetOut] messages *)
type payload =
  | Buffered of bufferId * Cstruct.t 
    (** [Buffered (id, buf)] is a packet buffered on a switch *)
  | NotBuffered of Cstruct.t
with sexp


(** [payload_bytes payload] returns the bytes for the given payload *)
val payload_bytes : payload -> Cstruct.t

type packetInReason =
  | NoMatch
  | ExplicitSend
  with sexp

(** [(payload, total_length, in_port, reason)] *)
type pktIn = payload * int * portId * packetInReason with sexp

(** [(payload, in_port option, action list)] *)
type pktOut = payload * (portId option) * (action list) with sexp

(* {1 Switch Configuration} *)

(** A simplification of the _switch features_ message from OpenFlow *)
type switchFeatures = {
  switch_id : switchId;
  switch_ports : portId list
} with sexp

(* {1 Statistics} *)

(** The body of a reply to an individual flow statistics request *)
type flowStats = {
  flow_table_id : int8; (** ID of table flow came from *)
  flow_pattern : Pattern.t;
  flow_duration_sec: int32;
  flow_duration_nsec: int32;
  flow_priority: int16;
  flow_idle_timeout: int16;
  flow_hard_timeout: int16;
  flow_actions: action list;
  flow_packet_count: int64;
  flow_byte_count: int64
} with sexp

(* {1 Errors} *)

(* TODO: FILL *)

(* {1 Pretty-printing } *)

val format_list : 'a list -> to_string:('a -> string) -> string
val format_action : Format.formatter -> action -> unit
val format_seq : Format.formatter -> seq -> unit
val format_par : Format.formatter -> par -> unit
val format_group : Format.formatter -> group -> unit
val format_flow : Format.formatter -> flow -> unit
val format_flowTable : Format.formatter -> flowTable -> unit

val string_of_action : action -> string
val string_of_seq : seq -> string
val string_of_par : par -> string
val string_of_flow : flow -> string
val string_of_flowTable : ?label:string -> flowTable -> string

module To0x01 : sig
  val from_pattern : Pattern.t -> OF10.pattern
  val from_flow : int -> flow -> OF10.flowMod
  val from_payload : payload -> OF10.payload
  val from_packetOut : pktOut -> OF10.packetOut
end
