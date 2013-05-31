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

(* [string_of_switchId sw] pretty-prints [sw]. *)
val string_of_switchId : switchId -> string

module Match : sig
(** Flow match data structure.  See Section 5.2.3 of the OpenFlow 1.0
specification. *)

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

  (** [to_string m] pretty-prints [m]. *)
  val to_string : t -> string

end

module PseudoPort : sig
(** A pseudo-port, as described by the [ofp_port] enumeration in Section 5.2.1
of the OpenFlow 1.0 specification. *)

  type t =
    | PhysicalPort of portId
    | InPort
    | Flood
    | AllPorts
    | Controller of int

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module Action : sig
(** Flow action data structure.  See Section 5.2.4 of the OpenFlow 1.0
specification. *)

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

  (** [move_controller_last seq] produces a semantically-equivalent list of
  actions with actions that send packets to the controller moved to the end.
  This works around a known bug in the OpenFlow reference switch where actions
  in an action sequence after a "send to controller" ([Output (Controller n)])
  action are ignored. *)
  val move_controller_last : sequence -> sequence

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

  (** [sequence_to_string v] pretty-prints an action sequence. *)
  val sequence_to_string : sequence -> string

end

module PortDescription : sig
(** Port data structure.  See section 5.2.1 of the OpenFlow 1.0 specification. *)

  module PortConfig : sig
  (** See the [ofp_port_config] enumeration in Section 5.2.1 of the OpenFlow 
  1.0 specification. *)

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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module PortState : sig
  (** See the [ofp_port_state] enumeration in Section 5.2.1 of the OpenFlow 
  1.0 specification.
  
  The [stp_X] fields have no effect on switch operation.  The controller must
  adjust [PortConfig.no_recv], [PortConfig.no_fwd], and
  [PortConfig.no_packet_in] to fully implement an 802.1D tree. *)

    type t =
      { down : bool  (** No physical link present. *)
      ; stp_listen : bool (** Not learning or relaying frames. *)
      ; stp_forward : bool (** Learning but not relaying frames. *)
      ; stp_block : bool (** Not part of spanning tree. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module PortFeatures : sig
  (** See the [ofp_port_features] enumeration in Section 5.2.1 of the OpenFlow
  1.0 specification. *)

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

    (** [to_string v] pretty-prints [v]. *)
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

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module PortStatus : sig
(** Port status message.  See Section 5.4.3 of the OpenFlow 1.0 specification. *)

  module ChangeReason : sig
  (** See the [ofp_port_reason] enumeration in Section 5.4.3 of the OpenFlow
  1.0 specification. *)

    type t =
      | Add (** The port was added. *)
      | Delete (** The port was removed. *)
      | Modify (** Some attribute of the port has changed. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
      { reason : ChangeReason.t
      ; desc : PortDescription.t }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module SwitchFeatures : sig
(** Switch features data structure.  See Section 5.3.1 of the OpenFlow 1.0
specification. *)

  (** Fields that support wildcard patterns on this switch. *)
  type supported_wildcards =
    { dlSrc : bool
    ; dlDst : bool
    ; dlTyp : bool
    ; dlVlan : bool
    ; dlVlanPcp : bool
    ; nwSrc : bool
    ; nwDst : bool
    ; nwProto : bool
    ; nwTos : bool
    ; tpSrc : bool
    ; tpDst : bool
    ; inPort : bool }

  module Capabilities : sig
  (** See the [ofp_capabilities] enumeration in Section 5.3.1 of the OpenFlow
  1.0 specification. *)


    type t =
      { flow_stats : bool (** Flow statistics. *)
      ; table_stats : bool (** Table statistics. *)
      ; port_stats : bool (** Port statistics. *)
      ; stp : bool (** 802.1D spanning tree. *)
      ; ip_reasm : bool (** Can reassemble IP fragments. *)
      ; queue_stats : bool (** Queue statistics. *)
      ; arp_match_ip : bool (** Match IP addresses in ARP packets. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module SupportedActions : sig
  (** Describes which actions ([Action.t]) this switch supports. *)

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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { switch_id : switchId
    ; num_buffers : int32
    ; num_tables : int8
    ; supported_capabilities : Capabilities.t
    ; supported_actions : SupportedActions.t
    ; ports : PortDescription.t list }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module FlowMod : sig
(** A flow modification data structure.  See Section 5.3.3 of the OpenFlow 1.0
specification. *)

  module Command : sig
  (** See the [ofp_flow_mod_command] enumeration in Section 5.3.3 of the 
  OpenFlow 1.0 specification. *)

    type t =
      | AddFlow (** New flow. *)
      | ModFlow (** Modify all matching flows. *)
      | ModStrictFlow (** Modify entry strictly matching wildcards. *)
      | DeleteFlow (** Delete all matching flows. *)
      | DeleteStrictFlow (** Delete entry strictly matching wildcards. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module Timeout : sig

    type t =
      | Permanent
      | ExpiresAfter of int16

    (** [to_string v] pretty-prints [v]. *)
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
    ; apply_to_packet : int32 option
    ; out_port : PseudoPort.t option
    ; check_overlap : bool }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module PacketIn : sig
(** A Packet-In message.  See Section 5.4.1 of the OpenFlow 1.0 specification.
*)

  module Reason : sig

    type t =
      | NoMatch
      | ExplicitSend

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { buffer_id : int32 option (** ID assigned by datapath. *)
    ; total_len : int16 (** Full length of frame. *)
    ; port : portId (** Port on which frame was received. *)
    ; reason : Reason.t (** Reason packet is being sent. *)
    ; packet : bytes (** Ethernet frame, halfway through 32-bit word, so the
                     IP header is 32-bit aligned.  The amount of data is
                     inferred from the length field in the header. *) 
    }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module PacketOut : sig
(** A send packet message.  See Section 5.3.6 of the OpenFlow 1.0 
specification. *)

  module Payload : sig

    type t =
      | Buffer of int32
      | Packet of bytes

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { buf_or_bytes : Payload.t
    ; port_id : portId option (** Packet's input port. *)
    ; actions : Action.sequence (** Actions. *)
    }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module StatsRequest : sig
(** A statistics request message.  See Section 5.3.5 of the OpenFlow 1.0 
specification. *)

  module IndividualFlowRequest : sig

    type t = { of_match : Match.t (** Fields to match. *)
             ; table_id : int8 (** ID of tabel to read from. *)
             ; port : PseudoPort.t option (** Require matching entries to
                                          include this as an output port.  A
                                          value of [None] indicates no
                                          restriction. *)
             }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module AggregateFlowRequest : sig

    type t = { of_match : Match.t (** Fields to match. *)
             ; table_id : int8 (** ID of tabel to read from. *)
             ; port : PseudoPort.t option (** Require matching entries to
                                          include this as an output port.  A
                                          value of [None] indicates no
                                          restriction. *)
             }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
  
    (** Description of this OpenFlow switch. *)
    | DescriptionReq
  
    (** Individual flow statistics. *)
    | IndividualFlowReq of IndividualFlowRequest.t
  
    (** Aggregate flow statistics. *)
    | AggregateFlowReq of AggregateFlowRequest.t
  
    (** Flow table statistics. *)
    | TableReq
  
    (** Physical port statistics. *)
    | PortReq of PseudoPort.t
  
    (* TODO(cole): queue and vendor stats requests. *)

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module StatsReply : sig
(** A statistics reply message.  See Section 5.3.5 of the OpenFlow 1.0 
specification. *)

  module DescriptionStats : sig

    type t =
      { manufacturer : string (** Manufacturer description. *)
      ; hardware : string (** Hardware description. *)
      ; software : string (** Software description. *)
      ; serial_number : string (** Serial number. *)
      ; datapath : string (** Human readable description of datapath. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module IndividualFlowStats : sig

    type t =
      { table_id : int8 (** ID of table flow came from. *)
      ; of_match : Match.t (** Description of fields. *)
      ; duration_sec : int32 (** Time flow has been alive in seconds. *)
      ; duration_nsec : int32 (** Time flow has been alive in nanoseconds 
                              beyond [duration_sec]. *)
      ; priority : int16 (** Priority of the entry.  Only meaningful when this
                         is not an exact-match entry. *)
      ; idle_timeout : int16 (** Number of seconds idle before expiration. *)
      ; hard_timeout : int16 (** Number of seconds before expiration. *)
      ; cookie : int64 (** Opaque controller-issued identifier. *)
      ; packet_count : int64 (** Number of packets in flow. *)
      ; byte_count : int64 (** Number of bytes in flow. *)
      ; actions : Action.sequence (** Actions. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module AggregateFlowStats : sig

    type t =
      { packet_count : int64 (** Number of packets in flows. *)
      ; byte_count : int64 (** Number of bytes in flows. *)
      ; flow_count : int16 (** Number of flows. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module TableStats : sig

    type t =
      { table_id : int8 (** Identifier of table.  Lower numbered tables are 
                        consulted first. *)
      ; name : string
      ; wildcards : SwitchFeatures.supported_wildcards (** Wildcards supported
                                                       by this table. *)
      ; max_entries : int32 (** Max number of entries supported. *)
      ; active_count : int32 (** Number of active entries. *)
      ; lookup_count : int64 (** Number of packets looked up in table. *)
      ; matched_count : int64 (** Number of packets that hit table. *)
      }

    (** [to_string v] pretty-prints [v]. *)
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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    | DescriptionRep of DescriptionStats.t
    | IndividualFlowRep of IndividualFlowStats.t list
    | AggregateFlowRep of AggregateFlowStats.t
    | TableRep of TableStats.t
    | PortRep of PortStats.t

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module Error : sig

  module HelloFailed : sig

    type t =
      | Incompatible
      | Eperm

    (** [to_string v] pretty-prints [v]. *)
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

    (** [to_string v] pretty-prints [v]. *)
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

    (** [to_string v] pretty-prints [v]. *)
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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module PortModFailed : sig

    type t =
      | BadPort
      | BadHwAddr

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module QueueOpFailed : sig

    type t =
      | BadPort
      | BadQueue
      | Eperm

    (** [to_string v] pretty-prints [v]. *)
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

  (** [to_string v] pretty-prints [v]. *)
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
