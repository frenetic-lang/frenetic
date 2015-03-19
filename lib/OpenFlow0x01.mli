open Core.Std
open Packet

type 'a mask = { m_value : 'a; m_mask : 'a option } with sexp

(** [switchId] is the type of switch identifiers received as part of
[SwitchFeature] replies. *)
type switchId = int64 with sexp

(** [portId] is the type of physical port identifiers (port numbers). *)
type portId = int16 with sexp

(** [queueId] identifies a specific queue for QoS. *)
type queueId = int32 with sexp

(** Transaction ID of OpenFlow messages. *)
type xid = OpenFlow_Header.xid

(** A pattern that matches a packet headers.

    For each field, write [Some x] indicates that the headers must be
    [x], where [None] is a wildcard. *)
type pattern =
    { dlSrc : dlAddr option (** Ethernet source address. *)
    ; dlDst : dlAddr option (** Etherent destination address. *)
    ; dlTyp : dlTyp option (** Ethernet frame type. *)
    ; dlVlan : dlVlan option (** Input VLAN id. *)
    ; dlVlanPcp : dlVlanPcp option (** Input VLAN priority. *)
    ; nwSrc : nwAddr mask option (** IP source address. *)
    ; nwDst : nwAddr mask option (** IP destination address. *)
    ; nwProto : nwProto option (** IP protocol. *)
    ; nwTos : nwTos option (** IP ToS. *)
    ; tpSrc : tpPort option (** TCP/UDP source port. *)
    ; tpDst : tpPort option (** TCP/UDP destination port. *)
    ; inPort : portId option (** Input switch port. *)
    } with sexp

(** A pseudo-port, as described by the [ofp_port] enumeration in
    Section 5.2.1 of the OpenFlow 1.0 specification. *)
type pseudoPort =
  | PhysicalPort of portId
  | InPort            (** Send the packet out the input port. This virtual port
                          must be explicitly used in order to send back out of
                          the input port. *)
  | Table             (** Perform actions in flow table.  NB: This can only be
                          the destination port for packet-out messages. *)
  | Normal            (** Process with normal L2/L3 switching. *)
  | Flood             (** All physical ports except input port and those
                          disabled by STP. *)
  | AllPorts          (** All physical ports except input port. *)
  | Controller of int (** Send to controller along with [n] (max 1024) bytes
                          of the packet. *)
  | Local             (** Local openflow "port". *)

(** Flow action data structure.  See Section 5.2.4 of the OpenFlow 1.0
    specification. *)
type action =
  | Output of pseudoPort (** Output to switch port. *)
  | SetDlVlan of dlVlan (** Set the 802.1Q VLAN ID.  A value of None strips
                        the 802.1Q header. *)
  | SetDlVlanPcp of dlVlanPcp (** Set the 802.1Q priority. *)
  | SetDlSrc of dlAddr (** Set ethernet source address. *)
  | SetDlDst of dlAddr (** Set ethernet destination address. *)
  | SetNwSrc of nwAddr (** Set IP source address. *)
  | SetNwDst of nwAddr (** Set IP destination address. *)
  | SetNwTos of nwTos (** Set IP ToS. *)
  | SetTpSrc of tpPort (** Set TCP/UDP source port. *)
  | SetTpDst of tpPort (** Set TCP/UDP destination port. *)
  | Enqueue of pseudoPort * queueId (** Enqueue to a switch queue *)

(** The type of flow rule timeouts.  See Section 5.3.3 of the OpenFlow 1.0
    specification. *)
type timeout =
  | Permanent (** No timeout. *)
  | ExpiresAfter of int16 (** Time out after [n] seconds. *)

(** See the [ofp_flow_mod_command] enumeration in Section 5.3.3 of the
    OpenFlow 1.0 specification. *)
type flowModCommand =
  | AddFlow (** New flow. *)
  | ModFlow (** Modify all matching flows. *)
  | ModStrictFlow (** Modify entry strictly matching wildcards. *)
  | DeleteFlow (** Delete all matching flows. *)
  | DeleteStrictFlow (** Delete entry strictly matching wildcards. *)

(** A flow modification data structure.  See Section 5.3.3 of the OpenFlow 1.0
specification. *)
type flowMod =
    { command : flowModCommand
    ; pattern: pattern (** Fields to match. *)
    ; priority : int16 (** Priority level of flow entry. *)
    ; actions : action list (** Actions. *)
    ; cookie : int64 (** Opaque controller-issued identifier. *)
    ; idle_timeout : timeout (** Idle time before discarding (seconds). *)
    ; hard_timeout : timeout (** Max time before discarding (seconds). *)
    ; notify_when_removed : bool (** Send flow removed message when flow
                                 expires or is deleted. *)
    ; apply_to_packet : int32 option (** Optional buffered packet to apply
                                     to. *)
    ; out_port : pseudoPort option (** For [DeleteFlow] and
                                     [DeleteStrictFlow] modifications, require
                                     matching entries to include this as an
                                     output port.  A value of [None] indicates
                                     no restriction. *)
    ; check_overlap : bool (** Check for overlapping entries first. *)
    }

(** The payload for [packetIn] and [packetOut] messages. *)
type payload =
  | Buffered of int32 * bytes
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

(** A packet-in message.  See Section 5.4.1 of the OpenFlow 1.0
    specification. *)
type packetIn =
    { input_payload : payload
    (** The packet contents, which may truncated, in which case,
        the full packet is buffered on the switch. *)
    ; total_len : int16
      (** The length of the full packet, which may exceed the length
          of [payload] if the packet is buffered. *)
    ; port : portId (** Port on which frame was received. *)
    ; reason : packetInReason (** Reason packet is being sent. *)
    }

type flowRemovedReason =
  | IdleTimeout
  | HardTimeout
  | Delete

(** A flow-removed message.  See Section 5.4.2 of the OpenFlow 1.0
    specification. *)
type flowRemoved =
    { pattern : pattern;
      cookie : int64;
      priority : int16;
      reason : flowRemovedReason;
      duration_sec : int32;
      duration_nsec : int32;
      idle_timeout : timeout;
      packet_count : int64;
      byte_count : int64
    }

(** A send-packet message.  See Section 5.3.6 of the OpenFlow 1.0
    specification. *)
type packetOut =
    { output_payload : payload
    ; port_id : portId option (** Packet's input port. *)
    ; apply_actions : action list (** Actions. *)
    }

(** {2 Convenient Functions} *)

val parse_payload : payload -> Packet.packet

(** [marshal_payload buf pkt] serializes pkt, where [buf] is an optional
buffer ID. *)
val marshal_payload : int32 option -> Packet.packet -> payload

(** A pattern that matches all packets. (All fields wildcarded.) *)
val match_all : pattern

(** [add_flow priority pattern action_sequence] creates a
    [FlowMod.t] instruction that adds a new flow table entry with
    the specified [priority], [pattern], and [action_sequence].

    The entry is permanent (i.e., does not timeout), its cookie is
    zero, etc. *)
val add_flow : int16 -> pattern -> ?idle_to:timeout -> ?notify_removed:bool -> action list -> flowMod

val delete_flow_strict : int16 -> pattern -> pseudoPort option -> flowMod

val delete_all_flows : flowMod

(** {2 Printing and Debugging} *)

val packetIn_to_string : packetIn -> string

(** Both [IndividualRequest] and [AggregateRequest] take as paramters,
    a [pattern] that specifies the fields to match, the [table_id]
    to read from, and an optional port, which requires matching
    entries to have this as an output port.  Use table ID [0xFF] to
    read from all tables. *)

(** The body of an individual or aggregate flow stat request. *)
type statsReq =
  { sr_of_match : pattern
  ; sr_table_id : int8
  ; sr_out_port : pseudoPort option
  }

type request =
  | DescriptionRequest
  | FlowTableStatsRequest
  | IndividualRequest of statsReq
  | AggregateRequest of statsReq
  | PortRequest of pseudoPort option

  (** The body of a reply to a description request. *)
type descriptionStats =
    { manufacturer : string (** Manufacturer description. *)
    ; hardware : string (** Hardware description. *)
    ; software : string (** Software description. *)
    ; serial_number : string (** Serial number. *)
    ; datapath : string (** Human readable description of datapath. *)
    }

  (** The body of a reply to an individual flow statistics request. *)
type individualStats =
    { table_id : int8 (** ID of table flow came from. *)
    ; of_match : pattern (** Description of fields. *)
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
    ; actions : action list (** Actions. *)
    }

type aggregateStats =
    { total_packet_count : int64 (** Number of packets in flows. *)
    ; total_byte_count : int64 (** Number of bytes in flows. *)
    ; flow_count : int32 (** Number of flows. *)
    }

type portStats =
    { port_no : int16
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
    ; collisions : int64
    }

  (** A statistics reply message.  See Section 5.3.5 of the OpenFlow 1.0
      specification. *)

type reply =
  | DescriptionRep of descriptionStats
  | IndividualFlowRep of individualStats list
  | AggregateFlowRep of aggregateStats
  | PortRep of portStats

val reply_to_string : reply -> string

module Wildcards : sig

    type t = {
      in_port: bool;
      dl_vlan: bool;
      dl_src: bool;
      dl_dst: bool;
      dl_type: bool;
      nw_proto: bool;
      tp_src: bool;
      tp_dst: bool;
      nw_src: int; (* XXX: unsigned *)
      nw_dst: int; (* XXX: unsigned *)
      dl_vlan_pcp: bool;
      nw_tos: bool;
    }

    val to_string : t -> string

    val marshal : t -> int32
    val parse : int32 -> t

end

module Match : sig

  type t = pattern

  val to_string : t -> string

  val marshal : t -> Cstruct.t -> int
  val parse : Cstruct.t -> t

  val size_of : t -> int

end

module PseudoPort : sig

  type t = pseudoPort

  val to_string : t -> string

  val marshal : t -> int
  val make : int -> int -> t

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

  val marshal : t -> Cstruct.t -> int
  val parse : Cstruct.t -> t

  val size_of : t -> int

end

(** The type of flow rule timeouts.  See Section 5.3.3 of the OpenFlow 1.0
specification. *)
module Timeout : sig

  type t = timeout

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

  val to_int : t -> int16
  val of_int : int16 -> t
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

    val to_int : t -> int16
    val of_int : int16 -> t

  end

  type t = flowMod


  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

  val marshal : t -> Cstruct.t -> int
  val parse : Cstruct.t -> t

  val size_of : t -> int

end


module Payload : sig

  type t = payload

end

module PacketIn : sig

  module Reason : sig

    type t = packetInReason

  end

  type t = packetIn

end

(** Flow removed data structure. See section 5.4.3 of the OpenFlow 1.0 specification. *)
module FlowRemoved : sig

  module Reason : sig

    type t = flowRemovedReason

    val to_string : t -> string

    val to_int : t -> int16
    val of_int : int16 -> t

  end

  type t = flowRemoved

  val to_string : t -> string

  val marshal : t -> Cstruct.t -> int
  val parse : Cstruct.t -> t

  val size_of : t -> int

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


    val to_int : t -> Int32.t
    val of_int : Int32.t -> t
  end

  (** See the [ofp_port_state] enumeration in Section 5.2.1 of the OpenFlow
  1.0 specification.

  The [stp_X] fields have no effect on switch operation.  The controller must
  adjust [PortConfig.no_recv], [PortConfig.no_fwd], and
  [PortConfig.no_packet_in] to fully implement an 802.1D tree. *)
  module PortState : sig

    module StpState : sig
      type t =
        | Listen (** Not learning or relaying frames *)
        | Learn (** Learning but not relaying frames *)
        | Forward (** Learning and relaying frames *)
        | Block (** Not part of the spanning tree *)

      val of_int : Int32.t -> t
      val to_int : t -> Int32.t

      val to_string : t -> string
    end

    type t =
      { down : bool  (** No physical link present. *)
      ; stp_state : StpState.t (** The state of the port wrt the spanning tree
                                   algorithm *)
      }

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

    val of_int : Int32.t -> t
    val to_int : t -> Int32.t

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

    val of_int : Int32.t -> t
    val to_int : t -> Int32.t

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

  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int

  val size_of : t -> int

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

  val parse : Cstruct.t -> t
  val marshal : t -> Cstruct.t -> int

  val size_of : t -> int

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

module SwitchConfig : sig

  module FragFlags : sig

    type t =
      | FragNormal
      | FragDrop
      | FragReassemble

    val to_string : t -> string
  end

  type t = { frag_flags : FragFlags.t;
	     miss_send_len : int }

  val to_string : t -> string
end

module StatsRequest : sig
  type t = request
  val to_string : t -> string
end

module StatsReply : sig

  type t = reply

  val parse : Cstruct.t -> t

  val marshal : t -> Cstruct.t -> int

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
  type c =

    (** Hello protocol failed. *)
    | HelloFailed of HelloFailed.t

    (** Request was not understood. *)
    | BadRequest of BadRequest.t

    (** Error in action description *)
    | BadAction of BadAction.t

    (** Problem modifying flow entry. *)
    | FlowModFailed of FlowModFailed.t

    (** Port mod request failed. *)
    | PortModFailed of PortModFailed.t

    (** Queue operation failed. *)
    | QueueOpFailed of QueueOpFailed.t

  type t =

    | Error of c * Cstruct.t

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** A VENDOR message.  See Section 5.5.4 of the OpenFlow 1.0 specification. *)
module Vendor : sig

  type t = int32 * Cstruct.t

  val parse : Cstruct.t -> t

  val marshal : t -> Cstruct.t  -> int

end

(** A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the
specification. *)
module Message : sig

  type t =
    | Hello of bytes
    | ErrorMsg of Error.t
    | EchoRequest of bytes
    | EchoReply of bytes
    | VendorMsg of Vendor.t
    | SwitchFeaturesRequest
    | SwitchFeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | PacketInMsg of PacketIn.t
    | FlowRemovedMsg of FlowRemoved.t
    | PortStatusMsg of PortStatus.t
    | PacketOutMsg of PacketOut.t
    | BarrierRequest
    | BarrierReply
    | StatsRequestMsg of StatsRequest.t
    | StatsReplyMsg of StatsReply.t
    | SetConfig of SwitchConfig.t
    | ConfigRequestMsg
    | ConfigReplyMsg of SwitchConfig.t

  (** [size_of msg] returns the size of [msg] in bytes when serialized. *)
  val size_of : t -> int

  val header_of : xid -> t -> OpenFlow_Header.t

  (** [parse hdr bits] parses the body of a message with header [hdr] from
      buffer [bits].
      @param hdr Header of the message to be parsed from [bits].
      @param bits string containing a serialized message body.
      @return [(xid, message)] where [xid] is the transaction ID.
      @raise Unparsable if [bits] cannot be parsed.
      @raise Ignored if [bits] contains a valid OpenFlow message that the
             parser does not yet handle. *)
  val parse : OpenFlow_Header.t -> string -> (xid * t)

  val marshal_body : t -> Cstruct.t -> unit

  (** [marshal xid msg] serializes [msg], giving it a transaction ID [xid]. *)
  val marshal : xid -> t -> string

  (** [to_string msg] pretty-prints [msg]. *)
  val to_string : t -> string

end


(** {9 Pretty printing}

    In general, each submodule contains pretty-printing functions for the types
    defined therein.  This section defines pretty printers for top-level types.
*)

(** [string_of_switchId sw] pretty-prints [sw] in hex. *)
val string_of_switchId : switchId -> string

(** [string_of_portId p] pretty-prints [p]. *)
val string_of_portId : portId -> string

(** [string_of_queueId q] pretty-prints [q]. *)
val string_of_queueId : queueId -> string

(** {9 Parsing exceptions}

    These exceptions may occur when parsing OpenFlow messages.
*)

(** [Unparsable msg] signals an error in parsing, such as when a bit sequence
has been corrupted. *)
exception Unparsable of string

(** [Ignored msg] signals the arrival of a valid OpenFlow message that the
parser is not yet equipped to handle. *)
exception Ignored of string
