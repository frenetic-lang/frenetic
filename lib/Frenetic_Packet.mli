(** Library for constructing, marshaling and parsing data packets. *)

(** {9 Packet types}

    It is possible to navigate the structure of a packet directly
    using the types defined here. However, using {!accs} may be more
    convenient.
*)

(** [int8] is the type of 8-bit integers. *)
type int8 = int with sexp

(** [int16] is the type of 16-bit integers. *)
type int16 = int with sexp

(** [int48] is the type of 48-bit integers. *)
type int48 = int64 with sexp

(** [dlAddr] is the type of Ethernet addresses. *)
type dlAddr = int48 with sexp

(** [dlTyp] is the type of Ethernet frame types. *)
type dlTyp = int16 with sexp

(** [dlVlan] is the type of VLAN identifiers.  A value of [None]
    indicates that no 802.1Q (VLAN) header is set, which is distinct from
    setting the VLAN to 0.
*)
type dlVlan = int16 option with sexp

(** [dlVlanPcp] is the type of 802.1Q (VLAN) priorities. *)
type dlVlanPcp = int8 with sexp

(** [dlVlanDei] is the type of 802.1Q (VLAN) drop eligible indicator. *)
type dlVlanDei = bool with sexp

(** [nwAddr] is the type of IPv4 addresses. *)
type nwAddr = int32 with sexp

(** [nwProto] is the type of IPv4 protocol numbers. *)
type nwProto = int8 with sexp

(** [nwTos] is the type of IPv4 types of service. *)
type nwTos = int8 with sexp

(** [ipv6Addr] is the type of IPv6 addresses. *)
type ipv6Addr = int64*int64 with sexp

(** [tpPort] is the type of transport protocol ports. *)
type tpPort = int16 with sexp

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
      } with sexp
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
    ; payload : Cstruct.t (** TCP payload. *)
    } with sexp
end

(** UDP frame of a packet. *)
module Udp : sig

  type t =
    { src : tpPort (** Source port. *)
    ; dst : tpPort  (** Destination port. *)
    ; chksum : int16  (** Checksum. *)
    ; payload : Cstruct.t (** UDP payload. *)
    } with sexp
end

(** ICMP frame of a packet. *)
module Icmp : sig

  type t = 
    { typ : int8 (** ICMP type. *)
    ; code : int8 (** ICMP subtype. *)
    ; chksum : int16 (** Checksum. *)
    ; payload : Cstruct.t (** ICMP payload. *)
    } with sexp
end

(** Basic DNS packet type. *)
module Dns : sig

  (* DNS Question Description Records *)
  module Qd : sig
    type t =
      { name : string
      ; typ : int16
      ; class_ : int16
      } with sexp
  end

  (* DNS Resource Records *)
  module Rr : sig
    type t =
      { name : string
      ; typ : int16
      ; class_ : int16
      ; ttl : int (* TTL is a signed 32-bit int *)
      ; rdata : Cstruct.t
      } with sexp
  end

  type t =
    { id : int16
    ; flags : int16
    ; questions : Qd.t list
    ; answers : Rr.t list
    ; authority : Rr.t list
    ; additional : Rr.t list
    } with sexp

  (** [serialize dns_pkt] serializes [dns_pkt] into a bit sequence,
      suitable for placing in a UDP or TCP payload. *)
  val serialize : t -> Cstruct.t
end

(** IGMP v1 and v2 message type. *)
module Igmp1and2 : sig

  type t = {
    mrt: int8; (** Maximum response time. *)
    chksum : int16; (** Checksum. *)
    addr : nwAddr; (** IGMP group address. *)
  } with sexp

end

(** IGMP v3 message type. *)
module Igmp3 : sig

  module GroupRec : sig
    type t = {
      typ : int8; (** Group Record type. *)
      addr : nwAddr; (** Multicast Group. *)
      sources : nwAddr list; (** List of sources addresses. *)
    } with sexp
  end

  type t = {
    chksum : int16; (** Checksum. *)
    grs : GroupRec.t list; (** Group records. *)
  } with sexp
end

(** IGMP frame of a packet. *)
module Igmp : sig

  type msg =
    | Igmp1and2 of Igmp1and2.t
    | Igmp3 of Igmp3.t
    | Unparsable of (int8 * Cstruct.t)
  with sexp

  type t = {
    ver_and_typ : int8; (** IGMP version/type. *)
    msg : msg (** enclosed IGMP message *)
  } with sexp
end

(** IPv4 frame of a packet. *)
module Ip : sig

  (** The type [tp] represents packets at the transport protocol
      level, which are encapsulated within the IPv4 payload. At
      present, we only support TCP, UDP, ICMP and IGMP explicitly;
      otherwise, the raw bytes and IPv4 protocol number are
      provided. *)
  type tp =
    | Tcp of Tcp.t
    | Udp of Udp.t
    | Icmp of Icmp.t
    | Igmp of Igmp.t
    | Unparsable of (nwProto * Cstruct.t)
  with sexp

  module Flags : sig
    (** [Flags] is the type of IPv4 flags. *)
    type t =
      { df : bool (** Don't fragment. *)
      ; mf : bool (** More fragments. *)
      } with sexp
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
    ; options : Cstruct.t (** Uninterpreted IP options. *)
    ; tp : tp (** Transport payload. *)
    } with sexp
end

(** Address resolution protocol (ARP) packet payload. *)
module Arp : sig
  type t =
    | Query of dlAddr * nwAddr * nwAddr
    | Reply of dlAddr * nwAddr * dlAddr * nwAddr
  with sexp
end

(** The type [nw] represents a packet at the network protocol level. *)
type nw =
  | Ip of Ip.t (** Internet Protocol version 4 (IPv4). *)
  | Arp of Arp.t (** Address Resolution Protocol (ARP). *)
  | Unparsable of (dlTyp * Cstruct.t) (** The EtherType code accompanied by the 
                                  uninterpreted ethernet payload. *)
  with sexp

(** The type [packet] represents a packet at the ethernet protocol level. *)
type packet = 
  { dlSrc : dlAddr (** Ethernet source address. *)
  ; dlDst : dlAddr (** Ethernet destination address. *)
  ; dlVlan : dlVlan (** 802.1Q VLAN identifier, if any. *)
  ; dlVlanDei : dlVlanDei (** 802.1Q VLAN Drop Eligible Indciator.  Ignored if
                           [dlVlan] is [None] *)
  ; dlVlanPcp : dlVlanPcp (** 802.1Q VLAN priority.  Ignored if [dlVlan] is 
                          [None]. *)
  ; nw : nw (** Network payload. *)
  } with sexp

(** {9:accs Accessors} *)

(** [dlTyp pkt] returns the ethernet frame type of [pkt] *)
val dlTyp : packet -> dlTyp

(** [nwSrc pkt] returns the source IP address of [pkt]. @raise
    Invalid_argument if the packet is not carrying an IP payload. *)
val nwSrc : packet -> nwAddr

(** [nwDst pkt] returns the destination IP address of [pkt]. @raise
    Invalid_argument if the packet is not carrying an IP payload. *)
val nwDst : packet -> nwAddr

(** [nwTos pkt] returns the IPv4 type of service of [pkt]. @raise
    Invalid_argument if the packet is not carrying an IP payload. *)
val nwTos : packet -> nwTos

(** [nwProto pkt] returns the IP protocol number of [pkt]. @raise
    Invalid_argument if the packet is not carrying an IP payload. *)
val nwProto : packet -> nwProto

(** [tpSrc pkt] returns the transport protocol source port of [pkt].
    @raise Invalid_argument if the packet is not carrying a TCP or UDP
    payload. *)
val tpSrc : packet -> tpPort

(** [tpDst pkt] returns the transport protocol destination port of
    [pkt].  @raise Invalid_argument if the packet is not carrying a
    TCP or UDP payload. *)
val tpDst : packet -> tpPort

(** [arpOperation pkt] returns the ARP operation code of [pkt].
    @raise Invalid_argument if the packet is not carrying a ARP
    payload. *)
val arpOperation : packet -> int

(** {9 Mutators} *)

(** [setDlSrc pkt addr] sets the ethernet source address of [pkt] to
    [addr]. *)
val setDlSrc : packet -> dlAddr -> packet

(** [setDlDst pkt addr] sets the ethernet destination address of [pkt]
    to [addr]. *)
val setDlDst : packet -> dlAddr -> packet

(** [setDlVlan pkt vlan] sets the VLAN identifier of [pkt] to
    [vlan]. *)
val setDlVlan : packet -> dlVlan -> packet

(** [setDlVlanPcp pkt pri] sets the VLAN priority of [pkt] to
    [pri]. *)
val setDlVlanPcp : packet -> dlVlanPcp -> packet

(** [setNwSrc pkt] sets the source IP address of [pkt] if the packet
    carries an IP payload.  Otherwise, it returns the packet
    unchanged. *)
val setNwSrc : packet -> nwAddr -> packet

(** [setNwDst pkt] sets the destination IP address of [pkt] if the
    packet carries an IP payload.  Otherwise, it returns the packet
    unchanged. *)
val setNwDst : packet -> nwAddr -> packet

(** [setNwTos pkt] sets the IPv4 type of service of [pkt] if the
    packet carries an IP payload.  Otherwise, it returns the packet
    unchanged. *)
val setNwTos : packet -> nwTos -> packet

(** [setTpSrc pkt] sets the transport protocol source port of [pkt] if
    the packet carries a TCP or UDP payload.  Otherwise, it returns
    the packet unchanged. *)
val setTpSrc : packet -> tpPort -> packet

(** [setTpDst pkt] sets the transport protocol destination port of
    [pkt] if the packet carries a TCP or UDP payload.  Otherwise, it
    returns the packet unchanged. *)
val setTpDst : packet -> tpPort -> packet

(** {9 Pretty Printing} *)

(** [string_of_mac mac] pretty-prints an ethernet address. *)
val string_of_mac : dlAddr -> string

(** [mac_of_string string] converts an colon-separated ethernet
    address to a [dlAddr] **)
val mac_of_string : string -> dlAddr
(* TODO(arjun): IMO it is silly to expose *all* these functions. *)

(** [string_of_dlAddr addr] is identical to [string_of_mac]. *)
val string_of_dlAddr : dlAddr -> string

(** [string_of_dlTyp typ] pretty-prints an ethernet frame type. *)
val string_of_dlTyp : dlTyp -> string

(** [string_of_dlVlan vlan] pretty-prints an 802.1Q VLAN
    identifier. *)
val string_of_dlVlan : dlVlan -> string

(** [string_of_dlVlanPcp p] pretty-prints an 802.1Q VLAN priority. *)
val string_of_dlVlanPcp : dlVlanPcp -> string

(** [string_of_dlVlanDei p] pretty-prints an 802.1Q VLAN Drop Eligible
    Indicator. *)
val string_of_dlVlanDei : dlVlanDei -> string

(** [string_of_ip ip] pretty-prints an IPv4 address. *)
val string_of_ip : nwAddr -> string

(** [ip_of_string string] converts an dot-separated IPv4 address to a
    [nwAddr] **)
val ip_of_string : string -> nwAddr

(** [string_of_nwAddr addr] is identical to [string_of_ip]. *)
val string_of_nwAddr : nwAddr -> string

(** [string_of_nwProto p] pretty-prints an IPv4 protocol. *)
val string_of_nwProto : nwProto -> string

(** [string_of_nwTos t] pretty-prints an IPv4 type of service. *)
val string_of_nwTos : nwTos -> string

(** [string_of_ipv6 t] pretty-prints an IPv6 address. **)
val string_of_ipv6 : ipv6Addr -> string

(** [string_of_ipv6 t] Converts a colon-separated IPv6 address to
    ipv6Addr. **)
val ipv6_of_string : string -> ipv6Addr

(** [string_of_tpPort p] pretty-prints a transport protocol port
    number. *)
val string_of_tpPort : tpPort -> string

(** [bytes_of_mac mac] returns a bit-string representation of
    [mac]. *)
val bytes_of_mac : dlAddr -> string

(** [mac_of_bytes str] constructs a [dlAddr] from a bit-string
    representation.  @raise Invalid_argument if the length of the
    string is not six characters. *)
val mac_of_bytes : string -> int48

(** {9:marshal Serialization} *)

(** [parse bits] parses a bit sequence into a packet. *)
val parse : Cstruct.t -> packet

(** [len pkt] length of packet in bytes. *)
val len : packet -> int

(** [marshal pkt] marshals [pkt] into a bit sequence. *)
val marshal : packet -> Cstruct.t

(** [to_string pkt] prints [pkt] as a string. *)
val to_string : packet -> string

(** [format_packet fmt pkt] uses formatter [fmt] to format [pkt]. *)
val format_packet : Format.formatter -> packet -> unit
