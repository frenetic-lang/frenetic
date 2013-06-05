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
    specification.  Each field describes criteria for matching one packet
    header field.  A value of [None] indicates a wildcard match
    (i.e. match all values). *)
type pattern =  
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

(** A pseudo-port, as described by the [ofp_port] enumeration in
    Section 5.2.1 of the OpenFlow 1.0 specification. *)
type pseudoPort =
  | PhysicalPort of portId
  | AllPorts (** All physical ports except input port. *)

  | InPort (** Send the packet out the input port.  This virtual port
               must be explicitly used in order to send back out of
               the input port. *)
  | Flood (** All physical ports except input port and those disabled by 
              STP. *)
  | Controller of int (** Send to controller along with [n] (max 1024)
                          bytes of the packet. *)

(** Flow action data structure.  See Section 5.2.4 of the OpenFlow 1.0
    specification. *)
type action =
  | Output of pseudoPort (** Output to switch port. *)
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

(** A send-packet message.  See Section 5.3.6 of the OpenFlow 1.0
    specification. *)
type packetOut =
    { output_payload : payload
    ; port_id : portId option (** Packet's input port. *)
    ; apply_actions : action list (** Actions. *)
    }

(** {9 Convenient Functions} *)


val parse_payload : payload -> Packet.packet

(** A pattern that matches all packets. (All fields wildcarded.) *)
val match_all : pattern

(** [add_flow priority pattern action_sequence] creates a
    [FlowMod.t] instruction that adds a new flow table entry with
    the specified [priority], [pattern], and [action_sequence].

    The entry is permanent (i.e., does not timeout), its cookie is
    zero, etc. *)
val add_flow : int16 -> pattern -> action list -> flowMod

val delete_all_flows : flowMod

