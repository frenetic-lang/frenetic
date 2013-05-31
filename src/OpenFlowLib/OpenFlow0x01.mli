(** This module provides data structures and functions for constructing,
marshalling, and parsing OpenFlow 1.0 messages.  It is largely drawn from the
OpenFlow 1.0 specification:

http://www.openflow.org/documents/openflow-spec-v1.0.0.pdf

Most data structures are documented with a pointer to relevent section in the
OpenFlow 1.0 specification, Rather than reproducing the specification here. *)

open Packet

(** [Unparsable msg] signals an error in parsing, such as when a bit sequence
    has been corrupted. *)
exception Unparsable of string

(** [Ignored msg] signals the arrival of a valid OpenFlow message that the
    parser is not yet equipped to handle. *)
exception Ignored of string

(* [switchId] is the type of switch identifiers received as part of
   [SwitchFeature] replies. *)
type switchId = int64

type priority = int16
type bufferId = int32
type table_id = int8

val string_of_switchId : switchId -> string
val string_of_priority : priority -> string
val string_of_bufferId : bufferId -> string
val string_of_table_id : table_id -> string

module Match : sig

  type t =
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan option
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr option
    ; nwDst : nwAddr option
    ; nwProto : nwProto option
    ; nwTos : nwTos option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option }

  (** A pattern that matches all packets. (All fields wildcarded.) *)
  val all : t

  val to_string : t -> string

end

module PseudoPort : sig

  type t =
    | PhysicalPort of portId
    | InPort
    | Flood
    | AllPorts
    | Controller of int

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

  val move_controller_last : sequence -> sequence

  val to_string : t -> string
  val sequence_to_string : sequence -> string

end

module PortDescription : sig

  module PortConfig : sig

    type t =
      { down : bool (** Port is administratively down. *)
      ; no_stp : bool (** Disable 802.1D spanning tree on port. *)
      ; no_recv : bool (** Drop all packets except 802.1D spanning
                         * tree packets. *)
      ; no_recv_stp : bool (** Drop received 802.1D STP packets. *)
      ; no_flood : bool (** Do not include this port when flooding. *)
      ; no_fwd : bool (** Drop packets forwarded to port. *)
      ; no_packet_in : bool (** Do not send packet-in msgs for port. *)
      }

    val to_string : t -> string

  end

  module PortState : sig

    type t =
      { down : bool  (* No physical link present. *)
      ; stp_listen : bool
      ; stp_forward : bool
      ; stp_block : bool
      ; stp_mask : bool }

    val to_string : t -> string

  end

  module PortFeatures : sig

    type t =
      { f_10MBHD : bool (** 10 Mb half-duplex rate support. *)
      ; f_10MBFD : bool (** 10 Mb full-duplex rate support. *)
      ; f_100MBHD : bool (** 100 Mb half-duplex rate support. *)
      ; f_100MBFD : bool (** 100 Mb full-duplex rate support. *)
      ; f_1GBHD : bool (** 1 Gb half-duplex rate support. *)
      ; f_1GBFD : bool (** 1 Gb full-duplex rate support. *)
      ; f_10GBFD : bool (** 10 Gb full-duplex rate support. *)
      ; copper : bool (** Copper medium. *)
      ; fiber : bool (** Fiber medium. *)
      ; autoneg : bool (** Auto-negotiation. *)
      ; pause : bool (** Pause. *)
      ; pause_asym : bool (** Asymmetric pause. *)
      }

    val to_string : t -> string

  end

  type t =
    { port_no : portId
    ; hw_addr : dlAddr
    ; name : string
    ; config : PortConfig.t
    ; state : PortState.t
    ; curr : PortFeatures.t
    ; advertised : PortFeatures.t
    ; supported : PortFeatures.t
    ; peer : PortFeatures.t }

  val to_string : t -> string

end

module PortStatus : sig

  module ChangeReason : sig

    type t =
      | Add
      | Delete
      | Modify

    val to_string : t -> string

  end

  type t =
      { reason : ChangeReason.t
      ; desc : PortDescription.t }

  val to_string : t -> string

end

module SwitchFeatures : sig

  module Capabilities : sig

    type t =
      { flow_stats : bool
      ; table_stats : bool
      ; port_stats : bool
      ; stp : bool
      ; ip_reasm : bool
      ; queue_stats : bool
      ; arp_match_ip : bool }

    val to_string : t -> string

  end

  module SupportedActions : sig

    type t =
      { output : bool
      ; set_vlan_id : bool
      ; set_vlan_pcp : bool
      ; strip_vlan : bool
      ; set_dl_src : bool
      ; set_dl_dst : bool
      ; set_nw_src : bool
      ; set_nw_dst : bool
      ; set_nw_tos : bool
      ; set_tp_src : bool
      ; set_tp_dst : bool
      ; enqueue : bool
      ; vendor : bool }

    val to_string : t -> string

  end

  type t =
    { switch_id : switchId
    ; num_buffers : int32
    ; num_tables : int8
    ; supported_capabilities : Capabilities.t
    ; supported_actions : SupportedActions.t
    ; ports : PortDescription.t list }

  val to_string : t -> string

end

module FlowMod : sig

  module Command : sig

    type t =
      | AddFlow
      | ModFlow
      | ModStrictFlow
      | DeleteFlow
      | DeleteStrictFlow

    val to_string : t -> string

  end

  module Timeout : sig

    type t =
      | Permanent
      | ExpiresAfter of int16

    val to_string : t -> string

  end

  type t =
    { mod_cmd : Command.t
    ; match_ : Match.t
    ; priority : int16
    ; actions : Action.sequence
    ; cookie : int64
    ; idle_timeout : Timeout.t
    ; hard_timeout : Timeout.t
    ; notify_when_removed : bool
    ; apply_to_packet : bufferId option
    ; out_port : PseudoPort.t option
    ; check_overlap : bool }

  val to_string : t -> string

end

module PacketIn : sig

  module Reason : sig

    type t =
      | NoMatch
      | ExplicitSend

    val to_string : t -> string

  end

  type t =
    { buffer_id : bufferId option
    ; total_len : int16
    ; port : portId
    ; reason : Reason.t
    ; packet :  bytes }

  val to_string : t -> string

end

module PacketOut : sig

  module Payload : sig

    type t =
      | Buffer of bufferId
      | Packet of bytes

    val to_string : t -> string

  end

  type t =
    { buf_or_bytes : Payload.t
    ; port_id : portId option
    ; actions : Action.sequence }

  val to_string : t -> string

end

module StatsRequest : sig

  module IndividualFlowRequest : sig

    type t = { of_match : Match.t
             ; table_id : table_id
             ; port : PseudoPort.t option }

    val to_string : t -> string

  end

  module AggregateFlowRequest : sig

    type t = { of_match : Match.t
             ; table_id : table_id
             ; port : PseudoPort.t option }

    val to_string : t -> string

  end

  type t =
  | DescriptionReq
  | IndividualFlowReq of IndividualFlowRequest.t
  | AggregateFlowReq of AggregateFlowRequest.t
  | TableReq
  | PortReq of PseudoPort.t
  (* TODO(cole): queue and vendor stats requests. *)

  val to_string : t -> string

end

module StatsReply : sig

  module DescriptionStats : sig

    type t =
      { manufacturer : string
      ; hardware : string
      ; software : string
      ; serial_number : string
      ; datapath : string }

    val to_string : t -> string

  end

  module IndividualFlowStats : sig

    type t =
      { table_id : table_id
      ; of_match : Match.t
      ; duration_sec : int32
      ; duration_nsec : int32
      ; priority : int16
      ; idle_timeout : int16
      ; hard_timeout : int16
      ; cookie : int64
      ; packet_count : int64
      ; byte_count : int64
      ; actions : Action.sequence }

    val to_string : t -> string

  end

  module AggregateFlowStats : sig

    type t =
      { packet_count : int64
      ; byte_count : int64
      ; flow_count : int16 }

    val to_string : t -> string

  end

  module TableStats : sig

    type t =
      { table_id : table_id
      ; name : string
      ; wildcards : int32
      ; max_entries : int32
      ; active_count : int32
      ; lookup_count : int64
      ; matched_count : int64 }

    val to_string : t -> string

  end

  module PortStats : sig

    type t =
      { port_no : PseudoPort.t
      ; rx_packets : int64
      ; tx_packets : int64
      ; rx_bytes : int64
      ; tx_bytes : int64
      ; rx_dropped : int64
      ; tx_dropped : int64
      ; rx_errors : int64
      ; tx_errors : int64
      ; rx_frame_err : int64
      ; rx_over_err : int64
      ; rx_crc_err : int64
      ; collisions : int64 }

    val to_string : t -> string

  end

  type t =
    | DescriptionRep of DescriptionStats.t
    | IndividualFlowRep of IndividualFlowStats.t list
    | AggregateFlowRep of AggregateFlowStats.t
    | TableRep of TableStats.t
    | PortRep of PortStats.t

  val to_string : t -> string

end

module Error : sig

  module HelloFailed : sig

    type t =
      | Incompatible
      | Eperm

    val to_string : t -> string

  end

  module BadRequest : sig

    type t =
      | BadVersion
      | BadType
      | BadStat
      | BadVendor
      | BadSubType
      | Eperm
      | BadLen
      | BufferEmpty
      | BufferUnknown

    val to_string : t -> string

  end

  module BadAction : sig

    type t =
      | BadType
      | BadLen
      | BadVendor
      | BadVendorType
      | BadOutPort
      | BadArgument
      | Eperm
      | TooMany
      | BadQueue

    val to_string : t -> string

  end

  module FlowModFailed : sig

    type t =
      | AllTablesFull
      | Overlap
      | Eperm
      | BadEmergTimeout
      | BadCommand
      | Unsupported

    val to_string : t -> string

  end

  module PortModFailed : sig

    type t =
      | BadPort
      | BadHwAddr

    val to_string : t -> string

  end

  module QueueOpFailed : sig

    type t =
      | BadPort
      | BadQueue
      | Eperm

    val to_string : t -> string

  end

  (* Each error is composed of a pair (error_code, data) *)
  type t =
    | HelloFailed of HelloFailed.t * Cstruct.t
    | BadRequest of BadRequest.t * Cstruct.t
    | BadAction of BadAction.t * Cstruct.t
    | FlowModFailed of FlowModFailed.t * Cstruct.t
    | PortModFailed of PortModFailed.t * Cstruct.t
    | QueueOpFailed of QueueOpFailed.t  * Cstruct.t

  val to_string : t -> string

end


module Message : sig
(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

  module Header : sig

    type t

    (** Size in bytes of a serialized OpenFlow header structure (struct 
        ofp_header). *)
    val size : int

    (** Length in bytes of the serialized OpenFlow message with this header. *)
    val len : t -> int

    (** [to_string hdr] pretty-prints [hdr]. *)
    val to_string : t -> string

    (** [parse bits] parses [bits].
        @raise Unparsable if [bits] cannot be parsed. *)
    val parse : string -> t

  end

  (* Transaction ID of OpenFlow messages. *)
  type xid = int32

  type t =
    | Hello of bytes
    | ErrorMsg of Error.t
    | EchoRequest of bytes
    | EchoReply of bytes
    | SwitchFeaturesRequest
    | SwitchFeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | PacketInMsg of PacketIn.t
    | PortStatusMsg of PortStatus.t
    | PacketOutMsg of PacketOut.t
    | BarrierRequest
    | BarrierReply
    | StatsRequestMsg of StatsRequest.t
    | StatsReplyMsg of StatsReply.t

  (** [size_of msg] returns the size of [msg] in bytes when serialized. *)
  val size_of : t -> int

  (** [parse hdr bits] parses the body of a message with header [hdr] from
      buffer [bits]. 
      @param hdr Header of the message to be parsed from [bits].
      @param bits string containing a serialized message body.
      @return [(xid, message)] where [xid] is the transaction ID.
      @raise Unparsable if [bits] cannot be parsed.
      @raise Ignored if [bits] contains a valid OpenFlow message that the 
             parser does not yet handle. *)
  val parse : Header.t -> string -> (xid * t)

  (** [marshal xid msg] serializes [msg], giving it a transaction ID [xid]. *)
  val marshal : xid -> t -> string

  (** [to_string msg] pretty-prints [msg]. *)
  val to_string : t -> string

  (** A message ([FlowModMsg]) that deletes all flows. *)
  val delete_all_flows : t

  (** A permanent [FlowModMsg] adding a rule. *)
  val add_flow : int -> Match.t -> Action.sequence -> t

end

(** Interface for all platforms. *)
module type PLATFORM = sig

  (** [SwitchDisconnected switch_id] is raised by [send_to_switch] and
      [recv_from_switch]. This exception is only raised once per switch.
      If the functions are applied to [switch_id] again, they raise
      [Invalid_argument]. *)
  exception SwitchDisconnected of switchId

  (** [send_to_switch switch_id xid msg] sends [msg] to the switch,
      blocking until the send completes. *)
  val send_to_switch : switchId -> Message.xid -> Message.t-> unit Lwt.t

  (** [recv_from_switch switch_id] blocks until [switch_id] sends a
      message.

      If the switch sends an [ECHO_REQUEST], [recv_from_switch] will
      itself respond with an [ECHO_REPLY] and block for the next
      message. *)
  val recv_from_switch : switchId -> (Message.xid * Message.t) Lwt.t

  (** [accept_switch] blocks until a switch connects, handles the
      OpenFlow handshake, and returns after the switch sends a
      [FEATURES_REPLY] message. *)
  val accept_switch : unit -> SwitchFeatures.t Lwt.t

end
