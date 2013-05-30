open Packet

exception Unparsable of string
exception Ignored of string

(* TODO(cole): find a better place for these. *)
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

  val size_of : t -> int
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

  val size_of : t -> int
  val parse : Cstruct.t -> t
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

  val size_of : t -> int
  val size_of_sequence : sequence -> int

  val marshal : t -> Cstruct.t -> int
  val marshal_sequence : sequence -> Cstruct.t -> int

  val move_controller_last : sequence -> sequence

  val to_string : t -> string
  val sequence_to_string : sequence -> string

  val parse : Cstruct.t -> t
  val parse_sequence : Cstruct.t -> sequence

end

module PortDescription : sig

  module PortConfig : sig

    type t =
      { down : bool (* Port is administratively down. *)
      ; no_stp : bool (* Disable 802.1D spanning tree on port. *)
      ; no_recv : bool (* Drop all packets except 802.1D spanning
                                 * tree packets. *)
      ; no_recv_stp : bool (* Drop received 802.1D STP packets. *)
      ; no_flood : bool (* Do not include this port when flooding. *)
      ; no_fwd : bool (* Drop packets forwarded to port. *)
      ; no_packet_in : bool (* Do not send packet-in msgs for port. *)
      }

    val size_of : t -> int
    val of_int : int32 -> t
    val to_int : t -> int32
    val to_string : t -> string

  end

  module PortState : sig

    type t =
      { down : bool  (* No physical link present. *)
      ; stp_listen : bool
      ; stp_forward : bool
      ; stp_block : bool
      ; stp_mask : bool }

    val size_of : t -> int
    val of_int : int32 -> t
    val to_int : t -> int32
    val to_string : t -> string

  end

  module PortFeatures : sig

    type t =
      { f_10MBHD : bool (* 10 Mb half-duplex rate support. *)
      ; f_10MBFD : bool (* 10 Mb full-duplex rate support. *)
      ; f_100MBHD : bool (* 100 Mb half-duplex rate support. *)
      ; f_100MBFD : bool (* 100 Mb full-duplex rate support. *)
      ; f_1GBHD : bool (* 1 Gb half-duplex rate support. *)
      ; f_1GBFD : bool (* 1 Gb full-duplex rate support. *)
      ; f_10GBFD : bool (* 10 Gb full-duplex rate support. *)
      ; copper : bool (* Copper medium. *)
      ; fiber : bool (* Fiber medium. *)
      ; autoneg : bool (* Auto-negotiation. *)
      ; pause : bool (* Pause. *)
      ; pause_asym : bool (* Asymmetric pause. *)
      }

    val size_of : t -> int
    val of_int : int32 -> t
    val to_int : t -> int32
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

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
  val to_string : t -> string

end

module PortStatus : sig

  module ChangeReason : sig

    type t =
      | Add
      | Delete
      | Modify

    val of_int : int -> t
    val to_int : t -> int
    val to_string : t -> string
    val size_of : t -> int

  end

  type t =
      { reason : ChangeReason.t
      ; desc : PortDescription.t }

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
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

    val size_of : t -> int
    val of_int : int32 -> t
    val to_int : t -> int32
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

    val size_of : t -> int
    val of_int : int32 -> t
    val to_int : t -> int32
    val to_string : t -> string

  end

  type t =
    { switch_id : int64
    ; num_buffers : int32
    ; num_tables : int8
    ; supported_capabilities : Capabilities.t
    ; supported_actions : SupportedActions.t
    ; ports : PortDescription.t list }

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
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

    val size_of : t -> int
    val of_int : int -> t
    val to_int : t -> int
    val to_string : t -> string

  end

  module Timeout : sig

    type t =
      | Permanent
      | ExpiresAfter of int16

    val size_of : t -> int
    val of_int : int -> t
    val to_int : t -> int
    val to_string : t -> string

  end

  type t =
    { mod_cmd : Command.t
    ; match_ : Match.t
    ; priority : priority
    ; actions : Action.sequence
    ; cookie : int64
    ; idle_timeout : Timeout.t
    ; hard_timeout : Timeout.t
    ; notify_when_removed : bool
    ; apply_to_packet : bufferId option
    ; out_port : PseudoPort.t option
    ; check_overlap : bool }

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
  val to_string : t -> string

end

module PacketIn : sig

  module Reason : sig

    type t =
      | NoMatch
      | ExplicitSend

    val size_of : t -> int
    val of_int : int -> t
    val to_int : t -> int
    val to_string : t -> string

  end

  type t =
    { buffer_id : bufferId option
    ; total_len : int16
    ; port : portId
    ; reason : Reason.t
    ; packet :  bytes }

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
  val to_string : t -> string

end

module PacketOut : sig

  module Payload : sig

    type t =
      | Buffer of bufferId
      | Packet of bytes

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  type t =
    { buf_or_bytes : Payload.t
    ; port_id : portId option
    ; actions : Action.sequence }

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
  val to_string : t -> string

end

module StatsRequest : sig

  module IndividualFlowRequest : sig

    type t = { of_match : Match.t
             ; table_id : table_id
             ; port : PseudoPort.t option }

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  module AggregateFlowRequest : sig

    type t = { of_match : Match.t
             ; table_id : table_id
             ; port : PseudoPort.t option }

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  type t =
  | DescriptionReq
  | IndividualFlowReq of IndividualFlowRequest.t
  | AggregateFlowReq of AggregateFlowRequest.t
  | TableReq
  | PortReq of PseudoPort.t
  (* TODO(cole): queue and vendor stats requests. *)

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
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

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  module IndividualFlowStats : sig

    type t =
      { table_id : table_id
      ; of_match : Match.t
      ; duration_sec : int
      ; duration_nsec : int
      ; priority : int
      ; idle_timeout : int
      ; hard_timeout : int
      ; cookie : Int64.t
      ; packet_count : Int64.t
      ; byte_count : Int64.t
      ; actions : Action.sequence }

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string
    val sequence_to_string : t list -> string

  end

  module AggregateFlowStats : sig

    type t =
      { packet_count : Int64.t
      ; byte_count : Int64.t
      ; flow_count : int }

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  module TableStats : sig

    type t =
      { table_id : table_id
      ; name : string
      ; wildcards : int32
      ; max_entries : int
      ; active_count : int
      ; lookup_count : int
      ; matched_count : int }

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  module PortStats : sig

    type t =
      { port_no : PseudoPort.t
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
      ; collisions : int }

    val size_of : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

  type t =
    | DescriptionRep of DescriptionStats.t
    | IndividualFlowRep of IndividualFlowStats.t list
    | AggregateFlowRep of AggregateFlowStats.t
    | TableRep of TableStats.t
    | PortRep of PortStats.t

  val size_of : t -> int
  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int
  val to_string : t -> string

end

module Error : sig

  module HelloFailed : sig

    type t =
      | Incompatible
      | Eperm

    val of_int : int -> t

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

    val of_int : int -> t

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

    val of_int : int -> t

  end

  module FlowModFailed : sig

    type t =
      | AllTablesFull
      | Overlap
      | Eperm
      | BadEmergTimeout
      | BadCommand
      | Unsupported

    val of_int : int -> t

  end

  module PortModFailed : sig

    type t =
      | BadPort
      | BadHwAddr

    val of_int : int -> t

  end

  module QueueOpFailed : sig

    type t =
      | BadPort
      | BadQueue
      | Eperm

    val of_int : int -> t

  end

  (* Each error is composed of a pair (error_code, data) *)
  type t =
    | HelloFailed of HelloFailed.t * Cstruct.t
    | BadRequest of BadRequest.t * Cstruct.t
    | BadAction of BadAction.t * Cstruct.t
    | FlowModFailed of FlowModFailed.t * Cstruct.t
    | PortModFailed of PortModFailed.t * Cstruct.t
    | QueueOpFailed of QueueOpFailed.t  * Cstruct.t

  val parse : Cstruct.t -> t

end


module Message : sig
(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

  module Header : sig

    type t

    val size : int
    val size_of : t -> int
    val len : t -> int
    val parse : Cstruct.t -> t
    val marshal : t -> Cstruct.t -> int
    val to_string : t -> string

  end

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

  val size_of : t -> int
  val parse : Header.t -> Cstruct.t -> (xid * t)
  val marshal : xid -> t -> string
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
