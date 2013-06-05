(** Library for constructing, marshalling, and parsing OpenFlow 1.0 messages.
It is largely drawn from the OpenFlow 1.0 specification:

{{:http://www.openflow.org/documents/openflow-spec-v1.0.0.pdf}
http://www.openflow.org/documents/openflow-spec-v1.0.0.pdf}

Most data structures are documented with a pointer to relevent section in the
OpenFlow 1.0 specification, rather than reproducing the specification here. *)

open Packet

(** {9 OpenFlow types}

    These types are primarily drawn from Section 5 of the OpenFlow 1.0
    specification.
*)

(** [switchId] is the type of switch identifiers received as part of
[SwitchFeature] replies. *)
type switchId = int64

(** [portId] is the type of physical port identifiers (port numbers). *)
type portId = int16

(** Transaction ID of OpenFlow messages. *)
type xid = int32

(** Flow match data structure.  See Section 5.2.3 of the OpenFlow 1.0
specification. *)
module Match : sig

  (** Each field describes criteria for matching one packet header field.  A
  value of [None] indicates a wildcard match (i.e. match all values). *)
  type t =
    { dlSrc : dlAddr option (** Ethernet source address. *)
    ; dlDst : dlAddr option (** Etherent destination address. *)
    ; dlTyp : dlTyp option (** Ethernet frame type. *)
    ; dlVlan : dlVlan option (** Input VLAN id. *)
    ; dlVlanPcp : dlVlanPcp option (** Input VLAN priority. *)
    ; nwSrc : nwAddr option (** IP source address. *)
    ; nwDst : nwAddr option (** IP destination address. *)
    ; nwProto : nwProto option (** IP protocol. *)
    ; nwTos : nwTos option (** IP ToS. *)
    ; tpSrc : tpPort option (** TCP/UDP source port. *)
    ; tpDst : tpPort option (** TCP/UDP destination port. *)
    ; inPort : portId option (** Input switch port. *)
    }

  (** A pattern that matches all packets. (All fields wildcarded.) *)
  val all : t

  (** [to_string m] pretty-prints [m]. *)
  val to_string : t -> string

end

(** A pseudo-port, as described by the [ofp_port] enumeration in Section 5.2.1
of the OpenFlow 1.0 specification. *)
module PseudoPort : sig

  type t =
    | PhysicalPort of portId
    | InPort (** Send the packet out the input port.  This virtual port must
             be explicitly used in order to send back out of the input port. *)
    | Flood (** All physical ports except input port and those disabled by 
            STP. *)
    | AllPorts (** All physical ports except input port. *)
    | Controller of int (** Send to controller along with [n] (max 1024) bytes
                        of the packet. *)

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** Flow action data structure.  See Section 5.2.4 of the OpenFlow 1.0
specification. *)
module Action : sig

  type t =
    | Output of PseudoPort.t (** Output to switch port. *)
    | SetDlVlan of dlVlan (** Set the 802.1Q VLAN ID. *)
    | SetDlVlanPcp of dlVlanPcp (** Set the 802.1Q priority. *)
    | StripVlan (** Strip the 802.1Q header. *)
    | SetDlSrc of dlAddr (** Set ethernet source address. *)
    | SetDlDst of dlAddr (** Set ethernet destination address. *)
    | SetNwSrc of nwAddr (** Set IP source address. *)
    | SetNwDst of nwAddr (** Set IP destination address. *)
    | SetNwTos of nwTos (** Set IP ToS. *)
    | SetTpSrc of tpPort (** Set TCP/UDP source port. *)
    | SetTpDst of tpPort (** Set TCP/UDP destination port. *)

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

    type t =
      | AddFlow (** New flow. *)
      | ModFlow (** Modify all matching flows. *)
      | ModStrictFlow (** Modify entry strictly matching wildcards. *)
      | DeleteFlow (** Delete all matching flows. *)
      | DeleteStrictFlow (** Delete entry strictly matching wildcards. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** The type of flow rule timeouts.  See Section 5.3.3 of the OpenFlow 1.0
  specification. *)
  module Timeout : sig

    type t =
      | Permanent (** No timeout. *)
      | ExpiresAfter of int16 (** Time out after [n] seconds. *)

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { mod_cmd : Command.t
    ; match_ : Match.t (** Fields to match. *)
    ; priority : int16 (** Priority level of flow entry. *)
    ; actions : Action.sequence (** Actions. *)
    ; cookie : int64 (** Opaque controller-issued identifier. *)
    ; idle_timeout : Timeout.t (** Idle time before discarding (seconds). *)
    ; hard_timeout : Timeout.t (** Max time before discarding (seconds). *)
    ; notify_when_removed : bool (** Send flow removed message when flow
                                 expires or is deleted. *)
    ; apply_to_packet : int32 option (** Optional buffered packet to apply 
                                     to. *)
    ; out_port : PseudoPort.t option (** For [DeleteFlow] and 
                                     [DeleteStrictFlow] modifications, require
                                     matching entries to include this as an
                                     output port.  A value of [None] indicates
                                     no restriction. *)
    ; check_overlap : bool (** Check for overlapping entries first. *)
    }

  (** [add_flow priority pattern action_sequence] creates a
      [FlowMod.t] instruction that adds a new flow table entry with
      the specified [priority], [pattern], and [action_sequence].

      The entry is permanent (i.e., does not timeout), its cookie is
      zero, etc. *)
  val add_flow : int16 -> Match.t -> Action.sequence -> t

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** The payload for [PacketIn.t] and [PacketOut.t] messages. *)
module Payload : sig

  type t =
    | Buffered of int32 * bytes 
    (** [Buffered (id, buf)] is a packet buffered on a switch. *)
    | NotBuffered of bytes

  (** [parse pk] signals an exception if the packet cannot be parsed.
      TODO(arjun): Which exception? *)
  val parse : t -> Packet.packet

  val to_string : t -> string
end

(** A packet-in message.  See Section 5.4.1 of the OpenFlow 1.0
    specification.
*)
module PacketIn : sig

  module Reason : sig

    type t =
      | NoMatch
      | ExplicitSend

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { payload : Payload.t
      (** The packet contents, which may truncated, in which case, 
          the full packet is buffered on the switch. *)
    ; total_len : int16
      (** The length of the full packet, which may exceed the length
          of [payload] if the packet is buffered. *)
    ; port : portId (** Port on which frame was received. *)
    ; reason : Reason.t (** Reason packet is being sent. *)
    }

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** A send-packet message.  See Section 5.3.6 of the OpenFlow 1.0
    specification. *)
module PacketOut : sig

  type t =
      { payload : Payload.t
      ; port_id : portId option (** Packet's input port. *)
      ; actions : Action.sequence (** Actions. *)
    }

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

  (** The body of an individual flow statistics request. *)
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

  (** The body of an aggregate flow statistics request. *)
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

  (** A message ([FlowModMsg]) that deletes all flows. *)
  val delete_all_flows : t

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
