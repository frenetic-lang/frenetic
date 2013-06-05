open Packet
open OpenFlow0x01_Core

type switchId = OpenFlow0x01_Core.switchId

type portId = OpenFlow0x01_Core.portId

type xid = OpenFlow0x01_Core.xid

module Match : sig

  type t = pattern

  val to_string : t -> string

end

module PseudoPort : sig

  type t = pseudoPort
  val to_string : t -> string

end

module Action : sig

  type t = action

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

(** A flow modification data structure.  See Section 5.3.3 of the OpenFlow 1.0
specification. *)
module FlowMod : sig

  (** See the [ofp_flow_mod_command] enumeration in Section 5.3.3 of the 
  OpenFlow 1.0 specification. *)
  module Command : sig

    type t = flowModCommand

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** The type of flow rule timeouts.  See Section 5.3.3 of the OpenFlow 1.0
  specification. *)
  module Timeout : sig

    type t = timeout

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t = flowMod


  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end


module Payload : sig

  type t = payload

  val to_string : t -> string
end

module PacketIn : sig

  module Reason : sig

    type t = packetInReason

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t = packetIn

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module PacketOut : sig

  type t = packetOut

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** Port data structure.  See section 5.2.1 of the OpenFlow 1.0 specification. *)
module PortDescription : sig

  (** See the [ofp_port_config] enumeration in Section 5.2.1 of the OpenFlow 
  1.0 specification. *)
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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** See the [ofp_port_state] enumeration in Section 5.2.1 of the OpenFlow 
  1.0 specification.
  
  The [stp_X] fields have no effect on switch operation.  The controller must
  adjust [PortConfig.no_recv], [PortConfig.no_fwd], and
  [PortConfig.no_packet_in] to fully implement an 802.1D tree. *)
  module PortState : sig

    type t =
      { down : bool  (** No physical link present. *)
      ; stp_listen : bool (** Not learning or relaying frames. *)
      ; stp_forward : bool (** Learning but not relaying frames. *)
      ; stp_block : bool (** Not part of spanning tree. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** See the [ofp_port_features] enumeration in Section 5.2.1 of the OpenFlow
  1.0 specification. *)
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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { port_no : portId
    ; hw_addr : dlAddr
    ; name : string
    ; config : PortConfig.t
    ; state : PortState.t
    ; curr : PortFeatures.t (** Current features. *)
    ; advertised : PortFeatures.t (** Features being advertised by the port. *)
    ; supported : PortFeatures.t (** Features supported by the port. *)
    ; peer : PortFeatures.t (** Features advertised by peer. *)
    }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** Port status message.  See Section 5.4.3 of the OpenFlow 1.0 specification. *)
module PortStatus : sig

  (** See the [ofp_port_reason] enumeration in Section 5.4.3 of the OpenFlow
  1.0 specification. *)
  module ChangeReason : sig

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

(** Switch features data structure.  See Section 5.3.1 of the OpenFlow 1.0
specification. *)
module SwitchFeatures : sig

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

  (** See the [ofp_capabilities] enumeration in Section 5.3.1 of the OpenFlow
  1.0 specification. *)
  module Capabilities : sig


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

  (** Describes which actions ([Action.t]) this switch supports. *)
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

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { switch_id : switchId (** Datapath unique ID.  The lower 48 bits are for 
                           a MAC address, while the upper 16 bits are 
                           implementer-defined. *)
    ; num_buffers : int32 (** Max packets buffered at once. *)
    ; num_tables : int8 (** Number of tables supported by datapath. *)
    ; supported_capabilities : Capabilities.t
    ; supported_actions : SupportedActions.t
    ; ports : PortDescription.t list (** Port definitions. *)
    }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** A statistics request message.  See Section 5.3.5 of the OpenFlow 1.0 
specification. *)
module StatsRequest : sig

  (** Both [IndividualRequest] and [AggregateRequest] take as
      paramters, a [pattern] that specifies the fields to match, the
      [table_if] to read from, and an optional port, which requires
      matching entries to have this as an output port. *)
  type t =
    | DescriptionRequest     (** Description of this OpenFlow switch. *)
    | IndividualRequest of pattern * int8 * pseudoPort option
    | AggregateRequest of pattern * int8 * pseudoPort option
      (** Flow table statistics. *)
    | TableRequest
      (** Physical port statistics. *)
    | PortRequest of pseudoPort
  
    (* TODO(cole): queue and vendor stats requests. *)

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** A statistics reply message.  See Section 5.3.5 of the OpenFlow 1.0 
specification. *)
module StatsReply : sig

  (** The body of a reply to a description request. *)
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

  (** The body of a reply to an individual flow statistics request. *)
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

  (** The body of a reply to an aggregate flow statistics request. *)
  module AggregateFlowStats : sig

    type t =
      { packet_count : int64 (** Number of packets in flows. *)
      ; byte_count : int64 (** Number of bytes in flows. *)
      ; flow_count : int32 (** Number of flows. *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** The body of a reply to a table statistics request. *)
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

  (** The body of a reply to a port statistics request. *)
  module PortStats : sig

    type t =
      { port_no : PseudoPort.t
      ; rx_packets : int64 (** Number of received packets. *)
      ; tx_packets : int64 (** Number of transmitted packets *)
      ; rx_bytes : int64 (** Number of received bytes. *)
      ; tx_bytes : int64 (** Number of transmitted bytes. *)
      ; rx_dropped : int64 (** Number of packets dropped by RX. *)
      ; tx_dropped : int64 (** Number of packets dropped by TX. *)
      ; rx_errors : int64 (** Number of receive errors.  This is a super-set
                              of more specific receive errors and should be
                              greater than or equal to the sum of all 
                              [rx_X_err] values. *)
      ; tx_errors : int64 (** Number of transmit errors.  This is a super-set
                              of more specific transmit errors and should be
                              greater than or equal to the sum of all 
                              [tx_X_err] values. *)
      ; rx_frame_err : int64 (** Number of frame alignment errors. *)
      ; rx_over_err : int64 (** Number of of packets with RX overrun. *)
      ; rx_crc_err : int64 (** Number of CRC errors. *)
      ; collisions : int64 (** Number of collisions. *)
      }

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

(** An error message.  See Section 5.4.4 of the OpenFlow 1.0 specification. *)
module Error : sig

  module HelloFailed : sig

    type t =
      | Incompatible (** No compatible version. *)
      | Eperm (** Permissions error. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module BadRequest : sig

    type t =
      | BadVersion (** [Header] version not supported. *)
      | BadType (** [Message] type not supported. *)
      | BadStat (** StatsRequest type not supported. *)
      | BadVendor (** Vendor not supported. *)
      | BadSubType (** Vendor subtype not supported. *)
      | Eperm (** Permissions error. *)
      | BadLen (** Wrong request length for type. *)
      | BufferEmpty (** Specified buffer has already been used. *)
      | BufferUnknown (** Specified buffer does not exist. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module BadAction : sig

    type t =
      | BadType (** Unknown action type. *)
      | BadLen (** Length problem in actions. *)
      | BadVendor (** Unknown vendor id specified. *)
      | BadVendorType (** Unknown action type for vendor id. *)
      | BadOutPort (** Problem validating output action. *)
      | BadArgument (** Bad action argument. *)
      | Eperm (** Permissions error. *)
      | TooMany (** Can't handle this many actions. *)
      | BadQueue (** Problem validating output queue. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module FlowModFailed : sig

    type t =
      | AllTablesFull (** Flow not added because of full tables. *)
      | Overlap (** Attepted to add overlapping flow with 
                [FlowMod.check_overlap] set. *)
      | Eperm (** Permissions error. *)
      | BadEmergTimeout (** Flow not added because of non-zero idle/hard timeout. *)
      | BadCommand (** Unknown command. *)
      | Unsupported (** Unsupported action list - cannot process in the order
                    specified. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module PortModFailed : sig

    type t =
      | BadPort (** Specified port does not exist. *)
      | BadHwAddr (** Specified hardware address is wrong. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module QueueOpFailed : sig

    type t =
      | BadPort (** Invalid port (or port does not exist). *)
      | BadQueue (** Queue does not exist. *)
      | Eperm (** Permissions error. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** Each error is composed of a pair (error_code, data) *)
  type t =

    (** Hello protocol failed. *)
    | HelloFailed of HelloFailed.t * Cstruct.t

    (** Request was not understood. *)
    | BadRequest of BadRequest.t * Cstruct.t

    (** Error in action description *)
    | BadAction of BadAction.t * Cstruct.t

    (** Problem modifying flow entry. *)
    | FlowModFailed of FlowModFailed.t * Cstruct.t

    (** Port mod request failed. *)
    | PortModFailed of PortModFailed.t * Cstruct.t

    (** Queue operation failed. *)
    | QueueOpFailed of QueueOpFailed.t  * Cstruct.t

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end


(** A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the 
specification. *)
module Message : sig

  (** A message header. *)
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

end


(** {9 Pretty printing}

    In general, each submodule contains pretty-printing functions for the types
    defined therein.  This section defines pretty printers for top-level types.
*)

(** [string_of_switchId sw] pretty-prints [sw]. *)
val string_of_switchId : switchId -> string

(** [string_of_switchId p] pretty-prints [p]. *)
val string_of_portId : portId -> string

(** {9 Parsing exceptions}

    These exceptions may occur when parsing OpenFlow messages.
*)

(** [Unparsable msg] signals an error in parsing, such as when a bit sequence
has been corrupted. *)
exception Unparsable of string

(** [Ignored msg] signals the arrival of a valid OpenFlow message that the
parser is not yet equipped to handle. *)
exception Ignored of string
