open Packet

exception Unparsable of string

module Match : sig

  type t = {
    dlSrc : dlAddr option;
    dlDst : dlAddr option;
    dlTyp : dlTyp option;
    dlVlan : dlVlan option;
    dlVlanPcp : dlVlanPcp option;
    nwSrc : nwAddr option;
    nwDst : nwAddr option;
    nwProto : nwProto option;
    nwTos : nwTos option;
    tpSrc : tpPort option;
    tpDst : tpPort option;
    inPort : portId option
  }

  (** A pattern that matches all packets. (All fields wildcarded.) *)
  val all : t

  val size : int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
  val to_string : t -> string

end

module PseudoPort : sig

  type t =
    | PhysicalPort of portId
    | InPort
    | Flood
    | AllPorts
    | Controller of int

  val none : int (* TODO(arjun): wtf? *)

  val marshal : t -> int
  val marshal_optional : t option -> int
  val to_string : t -> string

end

module Action : sig

  type t =
    | Output of PseudoPort.t
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

  type sequence = t list

  val sizeof : t -> int
  val marshal : t -> Cstruct.t -> int
  val move_controller_last : sequence -> sequence

  val to_string : t -> string
  val sequence_to_string : sequence -> string

  val parse_sequence : Cstruct.t -> sequence

end

type capabilities = {
  flow_stats : bool;
  table_stats : bool;
  port_stats : bool;
  stp : bool;
  ip_reasm : bool;
  queue_stats : bool;
  arp_match_ip : bool
}

type actions = {
  output : bool;
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
  vendor : bool
}

type features = {
  switch_id : int64;
  num_buffers : int32;
  num_tables : int8;
  supported_capabilities : capabilities;
  supported_actions : actions
}

type flowModCommand =
  | AddFlow
  | ModFlow
  | ModStrictFlow
  | DeleteFlow
  | DeleteStrictFlow

type switchId = int64

val string_of_switchId : switchId -> string

type priority = int16

type bufferId = int32

type timeout =
  | Permanent
  | ExpiresAfter of int16

type flowMod = {
  mfModCmd : flowModCommand;
  mfMatch : Match.t;
  mfPriority : priority;
  mfActions : Action.sequence;
  mfCookie : int64;
  mfIdleTimeOut : timeout;
  mfHardTimeOut : timeout;
  mfNotifyWhenRemoved : bool;
  mfApplyToPacket : bufferId option;
  mfOutPort : PseudoPort.t option;
  mfCheckOverlap : bool }

type reason =
  | NoMatch
  | ExplicitSend

type packetIn = {
  packetInBufferId : bufferId option;
  packetInTotalLen : int16;
  packetInPort : portId;
  packetInReason : reason;
  packetInPacket :  packet }

type xid = int32

type payload =
| Buffer of bufferId
| Packet of bytes

type packetOut = {
  pktOutBufOrBytes : payload;
  pktOutPortId : portId option;
  pktOutActions : Action.sequence
}

(* Component types of stats_request messages. *)

type table_id = int8

module IndividualFlowRequest : sig

    type t = { of_match : Match.t
             ; table_id : table_id
             ; port : PseudoPort.t
             }

    val to_string : t -> string

    val sizeof : t -> int

    val marshal : t -> Cstruct.t -> int

end

module AggregateFlowRequest : sig

    type t = { of_match : Match.t
             ; table_id : table_id
             ; port : PseudoPort.t
             }

    val to_string : t -> string

    val sizeof : t -> int

    val marshal : t -> Cstruct.t -> int

end

(* Component types of stats_reply messages. *)

module DescriptionStats : sig
  type t = { manufacturer : string
           ; hardware : string
           ; software : string
           ; serial_number : string
           ; datapath : string
           }
end

module IndividualFlowStats : sig
    type t = { table_id : table_id
             ; of_match : Match.t
             ; duration_sec : int
             ; duration_nsec : int
             ; priority : int
             ; idle_timeout : int
             ; hard_timeout : int
             ; cookie : Int64.t
             ; packet_count : Int64.t
             ; byte_count : Int64.t
             ; actions : Action.sequence
             }
end

module AggregateFlowStats : sig
    type t = { packet_count : int
             ; byte_count : int
             ; flow_count : int
             }
end

module TableStats : sig
    type t = { table_id : table_id
             ; name : string
             ; wildcards : int32
             ; max_entries : int
             ; active_count : int
             ; lookup_count : int
             ; matched_count : int
             }
end

module PortStats : sig
    type t = { port_no : PseudoPort.t
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
| PortReq of PseudoPort.t
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

(** A message ([FlowModMsg]) that deletes all flows. *)
val delete_all_flows : message

(** A permanent [FlowModMsg] adding a rule. *)
val add_flow : int -> Match.t -> Action.sequence -> message

(** Interface for all platforms. *)
module type PLATFORM = sig

  (** [SwitchDisconnected switch_id] is raised by [send_to_switch] and
      [recv_from_switch]. This exception is only raised once per switch.
      If the functions are applied to [switch_id] again, they raise
      [Invalid_argument]. *)
  exception SwitchDisconnected of switchId

  (** [send_to_switch switch_id xid msg] sends [msg] to the switch,
      blocking until the send completes. *)
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t

  (** [recv_from_switch switch_id] blocks until [switch_id] sends a
      message.

      If the switch sends an [ECHO_REQUEST], [recv_from_switch] will
      itself respond with an [ECHO_REPLY] and block for the next
      message. *)
  val recv_from_switch : switchId -> (xid * message) Lwt.t

  (** [accept_switch] blocks until a switch connects, handles the
      OpenFlow handshake, and returns after the switch sends a
      [FEATURES_REPLY] message. *)
  val accept_switch : unit -> features Lwt.t

end

