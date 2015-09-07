open Core.Std
open Frenetic_Packet

(** [Unparsable msg] signals an error in parsing, such as when a bit sequence
has been corrupted. *)
exception Unparsable of string

(** [Ignored msg] signals the arrival of a valid OpenFlow message that the
parser is not yet equipped to handle. *)
exception Ignored of string

type 'a mask = { m_value : 'a; m_mask : 'a option } with sexp

type switchId = int64 with sexp

type portId = int16 with sexp

type queueId = int32 with sexp

type xid = Frenetic_OpenFlow_Header.xid

type pattern =
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan option
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr mask option
    ; nwDst : nwAddr mask option
    ; nwProto : nwProto option
    ; nwTos : nwTos option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option
    } with sexp

type pseudoPort =
  | PhysicalPort of portId
  | InPort
  | Table
  | Normal
  | Flood
  | AllPorts
  | Controller of int
  | Local
  with sexp

type action =
  | Output of pseudoPort
  | SetDlVlan of dlVlan
  | SetDlVlanPcp of dlVlanPcp
  | SetDlSrc of dlAddr
  | SetDlDst of dlAddr
  | SetNwSrc of nwAddr
  | SetNwDst of nwAddr
  | SetNwTos of nwTos
  | SetTpSrc of tpPort
  | SetTpDst of tpPort
  | Enqueue of pseudoPort * queueId
with sexp

type timeout =
  | Permanent
  | ExpiresAfter of int16
with sexp

type flowModCommand =
  | AddFlow
  | ModFlow
  | ModStrictFlow
  | DeleteFlow
  | DeleteStrictFlow
with sexp

type flowMod =
    { command : flowModCommand
    ; pattern: pattern
    ; priority : int16
    ; actions : action list
    ; cookie : int64
    ; idle_timeout : timeout
    ; hard_timeout : timeout
    ; notify_when_removed : bool
    ; apply_to_packet : int32 option
    ; out_port : pseudoPort option
    ; check_overlap : bool
    }
with sexp

type payload =
  | Buffered of int32 * Cstruct.t sexp_opaque
  | NotBuffered of Cstruct.t sexp_opaque
with sexp

type packetInReason =
  | NoMatch
  | ExplicitSend
with sexp

type packetIn =
    { input_payload : payload
    ; total_len : int16
    ; port : portId
    ; reason : packetInReason
    } with sexp

type flowRemovedReason =
  | IdleTimeout
  | HardTimeout
  | Delete
with sexp

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
with sexp

type packetOut =
    { output_payload : payload
    ; port_id : portId option
    ; apply_actions : action list
    }
with sexp

type statsReq =
  { sr_of_match : pattern
  ; sr_table_id : int8
  ; sr_out_port : pseudoPort option
  }
with sexp

type request =
  | DescriptionRequest
  | FlowTableStatsRequest
  | IndividualRequest of statsReq
  | AggregateRequest of statsReq
  | PortRequest of pseudoPort option
with sexp

type descriptionStats =
    { manufacturer : string
    ; hardware : string
    ; software : string
    ; serial_number : string
    ; datapath : string
    }
with sexp

type individualStats =
    { table_id : int8
    ; of_match : pattern
    ; duration_sec : int32
    ; duration_nsec : int32
    ; priority : int16
    ; idle_timeout : int16
    ; hard_timeout : int16
    ; cookie : int64
    ; packet_count : int64
    ; byte_count : int64
    ; actions : action list
    }
with sexp

type aggregateStats =
    { total_packet_count : int64
    ; total_byte_count : int64
    ; flow_count : int32
    }
with sexp

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
with sexp

type reply =
  | DescriptionRep of descriptionStats
  | IndividualFlowRep of individualStats list
  | AggregateFlowRep of aggregateStats
  | PortRep of portStats list
with sexp

type wildcards = 
  { in_port: bool;
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
  } with sexp

type portConfig =
  { down : bool;
    no_stp : bool;
    no_recv : bool;
    no_recv_stp : bool;
    no_flood : bool;
    no_fwd : bool;
    no_packet_in : bool;
  } with sexp

type stpState =
  | Listen
  | Learn
  | Forward
  | Block
with sexp
      
type portState = 
  { down : bool;
    stp_state : stpState
  } with sexp

type portFeatures =
  { f_10MBHD : bool
  ; f_10MBFD : bool
  ; f_100MBHD : bool
  ; f_100MBFD : bool
  ; f_1GBHD : bool
  ; f_1GBFD : bool
  ; f_10GBFD : bool
  ; copper : bool
  ; fiber : bool
  ; autoneg : bool
  ; pause : bool
  ; pause_asym : bool
  } with sexp

type portDescription =
  { port_no : portId
  ; hw_addr : dlAddr
  ; name : string
  ; config : portConfig
  ; state : portState
  ; curr : portFeatures
  ; advertised : portFeatures
  ; supported : portFeatures
  ; peer : portFeatures
  } with sexp

module Wildcards : sig
  val to_string : wildcards -> string
  val marshal : wildcards -> int32
  val parse : int32 -> wildcards
end

module Match : sig
  val to_string : pattern -> string
  val marshal : pattern -> Cstruct.t -> int
  val parse : Cstruct.t -> pattern
  val size_of : pattern -> int
end

module PseudoPort : sig
  val to_string : pseudoPort -> string
  val marshal : pseudoPort -> int
  val make : int -> int -> pseudoPort
end

module Timeout : sig
  val to_string : timeout -> string
  val to_int : timeout -> int
  val of_int : int -> timeout
end

module Action : sig
  type sequence = action list

  (** [move_controller_last seq] produces a semantically-equivalent list of
  actions with actions that send packets to the controller moved to the end.
  This works around a known bug in the OpenFlow reference switch where actions
  in an action sequence after a "send to controller" ([Output (Controller n)])
  action are ignored. *)
  val move_controller_last : sequence -> sequence
  val to_string : action -> string
  val sequence_to_string : sequence -> string
  val marshal : action -> Cstruct.t -> int
  val parse : Cstruct.t -> action
  val size_of : action -> int
end

module FlowMod : sig
  module Command : sig
    val to_string : flowModCommand -> string
    val to_int : flowModCommand -> int16
    val of_int : int16 -> flowModCommand
  end
  type t = flowMod with sexp
  val to_string : flowMod -> string
  val marshal : flowMod -> Cstruct.t -> int
  val parse : Cstruct.t -> flowMod
  val size_of : flowMod -> int
end

module FlowRemoved : sig
  module Reason : sig
    val to_string : flowRemovedReason -> string
    val to_int : flowRemovedReason -> int16
    val of_int : int16 -> flowRemovedReason

  end
  val to_string : flowRemoved -> string
  val marshal : flowRemoved -> Cstruct.t -> int
  val parse : Cstruct.t -> flowRemoved
  val size_of : flowRemoved -> int
end

module PacketOut : sig
  val to_string : packetOut -> string
end

module PortDescription : sig

  module PortConfig : sig
    val to_string : portConfig -> string
    val to_int : portConfig -> Int32.t
    val of_int : Int32.t -> portConfig
  end
  module PortState : sig
    module StpState : sig
      val of_int : Int32.t -> stpState
      val to_int : stpState -> Int32.t
      val to_string : stpState -> string
    end
    val to_string : portState -> string
    val of_int : Int32.t -> portState
    val to_int : portState -> Int32.t
  end

  module PortFeatures : sig
    val to_string : portFeatures -> string
    val of_int : Int32.t -> portFeatures
    val to_int : portFeatures -> Int32.t
  end

  val to_string : portDescription -> string
  val parse : Cstruct.t -> portDescription
  val marshal : portDescription -> Cstruct.t -> int
  val size_of : portDescription -> int
end

module PortStatus : sig

  module ChangeReason : sig

    type t =
      | Add
      | Delete
      | Modify
    with sexp

    val to_string : t -> string
  end

  type t = 
    { reason : ChangeReason.t; 
      desc : portDescription } with sexp

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
  with sexp

  (** See the [ofp_capabilities] enumeration in Section 5.3.1 of the OpenFlow
  1.0 specification. *)
  module Capabilities : sig

    type t =
      { flow_stats : bool
      ; table_stats : bool
      ; port_stats : bool
      ; stp : bool
      ; ip_reasm : bool
      ; queue_stats : bool
      ; arp_match_ip : bool
      } with sexp

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** Describes which actions ([Action.t]) this switch supports. *)
  module SupportedActions : sig

    type t =
      { output : bool;
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
        vendor : bool; 
      } with sexp

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  type t =
    { switch_id : switchId;
      num_buffers : int32;
      num_tables : int8;
      supported_capabilities : Capabilities.t;
      supported_actions : SupportedActions.t;
      ports : portDescription list
    } with sexp

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

module SwitchConfig : sig

  module FragFlags : sig

    type t =
      | FragNormal
      | FragDrop
      | FragReassemble
    with sexp

    val to_string : t -> string
  end

  type t = 
    { frag_flags : FragFlags.t;
      miss_send_len : int 
    } with sexp

  val to_string : t -> string
end

module StatsRequest : sig
  type t = request with sexp
  val to_string : t -> string
end

module StatsReply : sig

  type t = reply with sexp

  val parse : Cstruct.t -> t

  val marshal : t -> Cstruct.t -> int

  val to_string : t -> string

end

(** An error message.  See Section 5.4.4 of the OpenFlow 1.0 specification. *)
module Error : sig

  module HelloFailed : sig

    type t =
      | Incompatible
      | Eperm
    with sexp

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
    with sexp

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
    with sexp

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
    with sexp

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module PortModFailed : sig

    type t =
      | BadPort
      | BadHwAddr
    with sexp

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  module QueueOpFailed : sig

    type t =
      | BadPort
      | BadQueue
      | Eperm
    with sexp

    (** [to_string v] pretty-prints [v]. *)
    val to_string : t -> string

  end

  (** Each error is composed of a pair (error_code, data) *)
  type c =
    | HelloFailed of HelloFailed.t
    | BadRequest of BadRequest.t
    | BadAction of BadAction.t
    | FlowModFailed of FlowModFailed.t
    | PortModFailed of PortModFailed.t
    | QueueOpFailed of QueueOpFailed.t
  with sexp

  type t =
    | Error of c * Cstruct.t sexp_opaque
  with sexp

  (** [to_string v] pretty-prints [v]. *)
  val to_string : t -> string

end

(** A VENDOR message.  See Section 5.5.4 of the OpenFlow 1.0 specification. *)
module Vendor : sig

  type t = int32 * Cstruct.t sexp_opaque
  with sexp

  val parse : Cstruct.t -> t

  val marshal : t -> Cstruct.t  -> int

end

(** A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the
specification. *)
module Message : sig

  type t =
    | Hello of Cstruct.t
    | ErrorMsg of Error.t
    | EchoRequest of Cstruct.t
    | EchoReply of Cstruct.t
    | VendorMsg of Vendor.t
    | SwitchFeaturesRequest
    | SwitchFeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | PacketInMsg of packetIn
    | FlowRemovedMsg of flowRemoved
    | PortStatusMsg of PortStatus.t
    | PacketOutMsg of packetOut
    | BarrierRequest
    | BarrierReply
    | StatsRequestMsg of StatsRequest.t
    | StatsReplyMsg of StatsReply.t
    | SetConfig of SwitchConfig.t
    | ConfigRequestMsg
    | ConfigReplyMsg of SwitchConfig.t
  with sexp

  (** [size_of msg] returns the size of [msg] in bytes when serialized. *)
  val size_of : t -> int

  val header_of : xid -> t -> Frenetic_OpenFlow_Header.t

  (** [parse hdr bits] parses the body of a message with header [hdr] from
      buffer [bits].
      @param hdr Header of the message to be parsed from [bits].
      @param bits string containing a serialized message body.
      @return [(xid, message)] where [xid] is the transaction ID.
      @raise Unparsable if [bits] cannot be parsed.
      @raise Ignored if [bits] contains a valid OpenFlow message that the
             parser does not yet handle. *)
  val parse : Frenetic_OpenFlow_Header.t -> string -> (xid * t)

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


(** {2 Convenient Functions} *)

val parse_payload : payload -> Frenetic_Packet.packet

(** [marshal_payload buf pkt] serializes pkt, where [buf] is an optional
buffer ID. *)
val marshal_payload : int32 option -> Frenetic_Packet.packet -> payload

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

val reply_to_string : reply -> string
