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

open Packet

(** {1 OpenFlow Identifier Types}

  OpenFlow requires identifiers for switches, ports, transaction numbers, etc.
  The representation of these identifiers varies across different versions
  of OpenFlow, which is why they are abstract.
*)

type switchId = int64 [@@deriving sexp, compare, eq]
type portId = int32 [@@deriving sexp, compare, eq]
type queueId = int32 [@@deriving sexp, compare, eq]
type bufferId = int32 [@@deriving sexp, compare, eq]

exception Unsupported of string

(** {1 Packet Types } *)

(** Packet payloads *)
type payload =
  | Buffered of bufferId * Cstruct.t
    (** [Buffered (id, buf)] is a packet buffered on a switch *)
  | NotBuffered of Cstruct.t
[@@deriving sexp]

(** [payload_bytes payload] returns the bytes for the given payload *)
val payload_bytes : payload -> Cstruct.t

type packetInReason =
  | NoMatch
  | ExplicitSend
[@@deriving sexp]

(** {1 Switch Configuaration } *)

(** A simplification of the _switch features_ message from OpenFlow *)
type switchFeatures = {
  switch_id : switchId;
  switch_ports : portId list
} [@@deriving sexp]

(** {1 Packet Forwarding} *)

module Pattern : sig

  module Ip : sig
    type t = nwAddr * int32 [@@deriving sexp]

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
    [@@deriving sexp]

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
[@@deriving sexp]

type pseudoport =
  | Physical of portId
  | InPort
  | Table
  | Normal
  | Flood
  | All
  | Controller of int
  | Local
[@@deriving sexp]

type groupId = int32 [@@deriving sexp]

type action =
  | Output of pseudoport
  | Enqueue of portId * queueId
  | Modify of modify
  | FastFail of groupId
  [@@deriving sexp]

type seq = action list [@@deriving sexp]

type par = seq list [@@deriving sexp]

type group = par list [@@deriving sexp]

type timeout =
  | Permanent (** No timeout *)
  | ExpiresAfter of int16 (** Time out after [n] seconds *)
  [@@deriving sexp]

type flow = {
  pattern: Pattern.t;
  action: group;
  cookie: int64;
  idle_timeout: timeout;
  hard_timeout: timeout
} [@@deriving sexp]

(** Priorities are implicit *)
type flowTable = flow list [@@deriving sexp]

(* {1 Errors} *)

(* TODO: FILL *)

(* {1 Statistics} *)

(** The body of a reply to an individual flow statistics request *)
type flowStats = {
  flow_table_id : int64;
  flow_pattern : Pattern.t;
  flow_actions: action list;
  flow_duration_sec: int64;
  flow_duration_nsec: int64;
  flow_priority: int64;
  flow_idle_timeout: int64;
  flow_hard_timeout: int64;
  flow_packet_count: int64;
  flow_byte_count: int64
} [@@deriving sexp]

type portStats =
  { port_no : int64
  ; port_rx_packets : int64
  ; port_tx_packets : int64
  ; port_rx_bytes : int64
  ; port_tx_bytes : int64
  ; port_rx_dropped : int64
  ; port_tx_dropped : int64
  ; port_rx_errors : int64
  ; port_tx_errors : int64
  ; port_rx_frame_err : int64
  ; port_rx_over_err : int64
  ; port_rx_crc_err : int64
  ; port_collisions : int64
} [@@deriving sexp]

(* {1 Events, switch-to-controller messages} *)

type event =
  | SwitchUp of switchId * portId list
  | SwitchDown of switchId
  | PortUp of switchId * portId
  | PortDown of switchId * portId
  | PacketIn of string * switchId * portId * payload * int * packetInReason
  | PortStats of switchId * portStats
  | FlowStats of switchId * flowStats

(* {1 Commands, controller-to-switch messages} *)

(* TODO: Temporary *)
type pktOut = payload * (portId option) * (action list) [@@deriving sexp]

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
val string_of_group : group -> string
val string_of_flowTable : ?label:string -> flowTable -> string
val string_of_event : event -> string

module OF10 = OpenFlow0x01
module To0x01 : sig
  val from_pattern : Pattern.t -> OF10.pattern
  val from_action : OF10.portId option -> action -> OF10.action
  val from_flow : int -> flow -> OF10.flowMod
  val from_switch_features : switchFeatures -> OF10.SwitchFeatures.t
  val from_payload : payload -> OF10.payload
  val from_packet_in_reason : packetInReason -> OF10.packetInReason
  val from_packetOut : pktOut -> OF10.packetOut
  val message_from_event : event -> (OF10.switchId * OF10.Message.t) option
end
module From0x01 : sig
  val from_action : OF10.action -> action
  val from_switch_features : OF10.SwitchFeatures.t -> switchFeatures
  val event_from_message : OF10.switchId -> OF10.Message.t -> event option
  val from_port_stats : OF10.portStats -> portStats
  val from_individual_stats: OF10.individualStats -> flowStats
end
