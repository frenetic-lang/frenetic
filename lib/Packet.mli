(** Library for constructing, marshaling and parsing ethernet data packets. *)

(** {9 Packet types}

    Based on {{:
    https://openflow.stanford.edu/display/ONL/POX+Wiki#POXWiki-Workingwithpacketspoxlibpacket}
    the packet library from POX}.

    You can navigate a packet's structure directly from here. But, using
    {!accs} may be more convenient.

*)

(** [bytes] is the type of arbitrary byte arrays, accessible via the Cstruct
library. *)
(* TODO(cole): Do we really need to expose Cstruct, or could this be opaque? *)
type bytes = Cstruct.t

(** [int8] is the type of 8-bit integers. *)
type int8 = int

(** [int16] is the type of 16-bit integers. *)
type int16 = int

(** [int48] is the type of 48-bit integers. *)
type int48 = int64

(** [dlAddr] is the type of Ethernet addresses. *)
type dlAddr = int48

(** [dlTyp] is the type of Ethernet frame types. *)
type dlTyp = int16

(** [dlVlan] is the type of VLAN identifiers.  A value of [None] indicates that
no 802.1Q (VLAN) header is set, which is distinct from setting the VLAN to 0.
*)
type dlVlan = int16 option

(** [dlVlanPcp] is the type of 802.1Q (VLAN) priorities. *)
type dlVlanPcp = int8

(** [nwAddr] is the type of IPv4 addresses. *)
type nwAddr = int32

(** [nwProto] is the type of IPv4 protocol numbers. *)
type nwProto = int8

(** [nwTos] is the type of IPv4 types of service. *)
type nwTos = int8

(** [tpPort] is the type of TCP ports. *)
type tpPort = int16

(** TCP frame of a packet. *)
module Tcp : sig

  (** TCP header flags. *)
  module Flags : sig

    type t =
      { ns : bool (** ECN-nonce concealment protection. *)
      ; cwr : bool (** Congestion window reduced. *)
      ; ece : bool (** ECN-Echo. *)
      ; urg : bool (** Indicates the Urgent pointer field is significant. *)
      ; ack : bool (** Indicates that the Acknowledgment field is 
                   significant. *)
      ; psh : bool (** Asks to push the buffered data to the receiving 
                   application. *)
      ; rst : bool (** Reset the connection. *)
      ; syn : bool (** Synchronize sequence numbers. *)
      ; fin : bool (** No more data from sender. *)
      }

  end

  type t = 
    { src : tpPort (** Source port. *)
    ; dst : tpPort  (** Destination port. *)
    ; seq : int32 (** Sequence number. *)
    ; ack : int32 (** Acknowledgement number. *)
    ; offset : int8 (** Data offset. *)
    ; flags : Flags.t (** TCP header flags. *)
    ; window : int16 (** Window size. *)
    ; chksum : int8  (** Checksum. *)
    ; urgent : int8 (** Urgent pointer. *)
    ; payload : bytes (** TCP payload. *)
    }

end

(** ICMP frame of a packet. *)
module Icmp : sig

  type t = 
    { typ : int8 (** ICMP type. *)
    ; code : int8 (** ICMP subtype. *)
    ; chksum : int16 (** Checksum. *)
    ; payload : bytes (** ICMP payload. *)
    }

end

(** IPv4 frame of a packet. *)
module Ip : sig

  (** [tp] is the type of protocol, which indicates which protocol is
  encapsulated in the payload of the IPv4 frame.  At present, only TCP and ICMP
  are explicitly supported; otherwise, the raw bytes and IPv4 protocol number
  are provided. *)
  type tp =
    | Tcp of Tcp.t
    | Icmp of Icmp.t
    | Unparsable of (nwProto * bytes)

  module Flags : sig
  (** [Flags] is the type of IPv4 flags. *)

    type t =
      { df : bool (** Don't fragment. *)
      ; mf : bool (** More fragments. *)
      }

  end

  type t = 
    { tos : nwTos (** Type of service. *)
    ; ident : int16 (** Identification. *)
    ; flags : Flags.t (** IPv4 header flags. *)
    ; frag : int16 (** Fragment offset. *)
    ; ttl : int8 (** Time to live. *)
    ; chksum : int16 (** Header checksum. *)
    ; src : nwAddr (** IP source address. *)
    ; dst : nwAddr (** IP destination address. *)
    ; tp : tp (** Packet payload. *)
    }

end

(** Address resolution protocol (ARP) packet payload. *)
module Arp : sig

  type t =
    | Query of dlAddr * nwAddr * nwAddr
    | Reply of dlAddr * nwAddr * dlAddr * nwAddr

end

(** [nw] is the type of an ethernet frame payload. *)
type nw =
  | Ip of Ip.t (** Internet Protocol version 4 (IPv4). *)
  | Arp of Arp.t (** Address Resolution Protocol (ARP). *)
  | Unparsable of (dlTyp * bytes) (** The EtherType code accompanied by the 
                                  uninterpreted ethernet payload. *)

(** [packet] is the type of ethernet data packets. *)
type packet = 
  { dlSrc : dlAddr (** Ethernet source address. *)
  ; dlDst : dlAddr (** Ethernet destination address. *)
  ; dlVlan : dlVlan (** 802.1Q VLAN identifier, if any. *)
  ; dlVlanPcp : dlVlanPcp (** 802.1Q VLAN priority.  Ignored if [dlVlan] is 
                          [None]. *)
  ; nw : nw (** Ethernet payload. *)
  }

(** {9:accs Accessors} *)

(** [dlTyp pkt] returns the ethernet frame type of [pkt] *)
val dlTyp : packet -> dlTyp

(** [nwSrc pkt] returns the source IP address of [pkt].
@raise Invalid_argument if the packet is not carrying an IP payload. *)
val nwSrc : packet -> nwAddr

(** [nwDst pkt] returns the destination IP address of [pkt].
@raise Invalid_argument if the packet is not carrying an IP payload. *)
val nwDst : packet -> nwAddr

(** [nwTos pkt] returns the IPv4 type of service of [pkt].
@raise Invalid_argument if the packet is not carrying an IP payload. *)
val nwTos : packet -> nwTos

(** [nwProto pkt] returns the IP protocol number of [pkt].
@raise Invalid_argument if the packet is not carrying an IP payload. *)
val nwProto : packet -> nwProto

(** [tpSrc pkt] returns the TCP source port of [pkt].
@raise Invalid_argument if the packet is not carrying a TCP payload. *)
val tpSrc : packet -> tpPort

(** [tpDst pkt] returns the TCP destination port of [pkt].
@raise Invalid_argument if the packet is not carrying a TCP payload. *)
val tpDst : packet -> tpPort

(** {9 Mutators} *)

(** [setDlSrc pkt addr] sets the ethernet source address of [pkt] to [addr]. *)
val setDlSrc : packet -> dlAddr -> packet

(** [setDlSrc pkt addr] sets the ethernet destination address of [pkt] to
[addr]. *)
val setDlDst : packet -> dlAddr -> packet

(** [setDlSrc pkt vlan] sets the VLAN identifier of [pkt] to [vlan]. *)
val setDlVlan : packet -> dlVlan -> packet

(** [setDlVlanPcp pkt pri] sets the VLAN priority of [pkt] to [pri]. *)
val setDlVlanPcp : packet -> dlVlanPcp -> packet

(** [setNwSrc pkt] sets the source IP address of [pkt] if the packet carries an
IP payload.  Otherwise, it returns the packet unchanged. *)
val setNwSrc : packet -> nwAddr -> packet

(** [setNwDst pkt] sets the destination IP address of [pkt] if the packet
carries an IP payload.  Otherwise, it returns the packet unchanged. *)
val setNwDst : packet -> nwAddr -> packet

(** [setNwTos pkt] sets the IPv4 type of service of [pkt] if the packet
carries an IP payload.  Otherwise, it returns the packet unchanged. *)
val setNwTos : packet -> nwTos -> packet

(** [setTpSrc pkt] sets the TCP source port of [pkt] if the packet
carries an TCP payload.  Otherwise, it returns the packet unchanged. *)
val setTpSrc : packet -> tpPort -> packet

(** [setTpDst pkt] sets the TCP destination port of [pkt] if the packet
carries a TCP payload.  Otherwise, it returns the packet unchanged. *)
val setTpDst : packet -> tpPort -> packet

(** {9 Pretty Printing} *)

(** [string_of_mac mac] pretty-prints an ethernet address. *)
val string_of_mac : dlAddr -> string

(* TODO(arjun): IMO it is silly to expose *all* these functions. *)

(** [string_of_dlAddr addr] is identical to [string_of_mac]. *)
val string_of_dlAddr : dlAddr -> string

(** [string_of_dlTyp typ] pretty-prints an ethernet frame type. *)
val string_of_dlTyp : dlTyp -> string

(** [string_of_dlVlan vlan] pretty-prints an 802.1Q VLAN identifier. *)
val string_of_dlVlan : dlVlan -> string

(** [string_of_dlVlanPcp p] pretty-prints an 802.1Q VLAN priority. *)
val string_of_dlVlanPcp : dlVlanPcp -> string

(** [string_of_ip ip] pretty-prints an IPv4 address. *)
val string_of_ip : nwAddr -> string

(** [string_of_nwAddr addr] is identical to [string_of_ip]. *)
val string_of_nwAddr : nwAddr -> string

(** [string_of_nwProto p] pretty-prints an IPv4 protocol. *)
val string_of_nwProto : nwProto -> string

(** [string_of_nwTos t] pretty-prints an IPv4 type of service. *)
val string_of_nwTos : nwTos -> string

(** [string_of_tpPort p] pretty-prints a TCP port number. *)
val string_of_tpPort : tpPort -> string

(** [bytes_of_mac mac] returns a bit-string representation of [mac]. *)
val bytes_of_mac : dlAddr -> string

(** [mac_of_bytes str] constructs a [dlAddr] from a bit-string representation.
@raise Invalid_argument if the length of the string is not six characters. *)
val mac_of_bytes : string -> int48

(** {9:marshal Serialization} *)

(** [parse bits] parses a bit sequence into a packet. *)
val parse : Cstruct.t -> packet

(** [len pkt] length of packet in bytes. *)
val len : packet -> int

(** [marshal pkt] marshals [pkt] into a bit sequence. *)
val marshal : packet -> Cstruct.t

val to_string : packet -> string

val format_packet : Format.formatter -> packet -> unit
