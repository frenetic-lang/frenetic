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

  val marshal : t -> Cstruct.t -> int
  val parse : Cstruct.t -> t
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
  val marshal : t -> int

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
  val marshal : t -> Cstruct.t -> int
  val parse : Cstruct.t -> t
  val parse_sequence : Cstruct.t -> t list

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
  val marshal : t -> Cstruct.t -> int
  val size_of : t -> int
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
  val parse : Cstruct.t -> t
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
  val marshal : t -> Cstruct.t -> int
  val size_of : t -> int
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
