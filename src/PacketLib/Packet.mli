(** Packet serialization library. *)

(** {9 Packet types}

    Based on {{:
    https://openflow.stanford.edu/display/ONL/POX+Wiki#POXWiki-Workingwithpacketspoxlibpacket}
    the packet library from POX}.

    You can navigate a packet's structure directly from here. But, using
    {!accs} may be more convenient.

*)

type bytes = Cstruct.t

type int8 = int

type int16 = int

type int48 = int64

(* [dlAddr] is the type of Ethernet addresses. *)
type dlAddr = int48

(* [dlTyp] is the type of Ethernet frame types. *)
type dlTyp = int16

(* [dlVlan] is the type of VLAN identifiers.  A value of [None] indicates that
no 802.1Q (VLAN) header is set, which is distinct from setting the VLAN to 0.
*)
type dlVlan = int16 option

(* [dlVlanPcp] is the type of 802.1Q (VLAN) priorities. *)
type dlVlanPcp = int8

(* [nwAddr] is the type of IPv4 addresses. *)
type nwAddr = int32

(* [nwProto] is the type of IPv4 protocol numbers. *)
type nwProto = int8

(* [nwTos] is the type of IPv4 types of service. *)
type nwTos = int8

(* [tpPort] is the type of TCP/UDP ports. *)
type tpPort = int16

module Tcp : sig

  type t = 
    { src : tpPort 
    ; dst : tpPort 
    ; seq : int32
    ; ack : int32 
    ; offset : int8 
    ; flags : int16
    ; window : int16 
    ; chksum : int8 
    ; urgent : int8
    ; payload : bytes }

  (** [to_string v] prett-prints [v]. *)
  val to_string : t -> string

end

module Icmp : sig

  type t = 
    { typ : int8
    ; code : int8
    ; chksum : int16
    ; payload : bytes }

  (** [to_string v] prett-prints [v]. *)
  val to_string : t -> string

end

module Ip : sig
(** IPv4 packet payload. *)

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

    val to_string : t -> string

  end

  type t = 
    { tos : nwTos (** Type of service. *)
    ; ident : int16 (** Identification. *)
    ; flags : Flags.t
    ; frag : int16 (** Fragment offset. *)
    ; ttl : int8 (** Time to live. *)
    ; chksum : int16 (** Header checksum. *)
    ; src : nwAddr (** IP source address. *)
    ; dst : nwAddr (** IP destination address. *)
    ; tp : tp (** Packet payload. *)
    }

  (** [to_string v] prett-prints [v]. *)
  val to_string : t -> string

end

module Arp : sig
(** Address resolution protocol (ARP) packet payload. *)

  type t =
    | Query of dlAddr * nwAddr * nwAddr
    | Reply of dlAddr * nwAddr * dlAddr * nwAddr

  (** [to_string v] prett-prints [v]. *)
  val to_string : t -> string

end

(** [nw] is the type of EtherTypes, which indicate which protocol is
encapsulated in the payload of the ethernet frame. *)
type nw =
  | Ip of Ip.t (** Internet Protocol version 4 (IPv4). *)
  | Arp of Arp.t (** Address Resolution Protocol (ARP). *)
  | Unparsable of bytes (** An uninterpreted sequence of bytes. *)

type packet = 
  { dlSrc : dlAddr
  ; dlDst : dlAddr 
  ; dlTyp : dlTyp
  ; dlVlan : dlVlan
  ; dlVlanPcp : dlVlanPcp
  ; nw : nw }

(** {9:accs Accessors} *)

val nwSrc : packet -> nwAddr

val nwDst : packet -> nwAddr

val nwTos : packet -> nwTos

val nwProto : packet -> nwProto

val tpSrc : packet -> tpPort

val tpDst : packet -> tpPort

(** {9 Mutators} *)

val setDlSrc : packet -> dlAddr -> packet

val setDlDst : packet -> dlAddr -> packet

val setDlVlan : packet -> dlVlan -> packet

val setDlVlanPcp : packet -> dlVlanPcp -> packet

val setNwSrc : packet -> nwAddr -> packet

val setNwDst : packet -> nwAddr -> packet

val setNwTos : packet -> nwTos -> packet

val setTpSrc : packet -> tpPort -> packet

val setTpDst : packet -> tpPort -> packet

(** {9 Pretty Printing} *)

val string_of_mac : int48 -> string

(* TODO(arjun): IMO it is silly to expose *all* these functions. *)
val dlAddr_to_string : int48 -> string

val dlTyp_to_string : int16 -> string

val dlVlan_to_string : int16 option -> string

val dlVlanPcp_to_string : int8 -> string

val nwAddr_to_string : int32 -> string

val nwProto_to_string : int8 -> string

val nwTos_to_string : int8 -> string

val tpPort_to_string : int16 -> string

val nw_to_string : nw -> string

val packet_to_string : packet -> string

val string_of_mac : int48 -> string

val bytes_of_mac : int48 -> string

val mac_of_bytes : string -> int48

val string_of_ip : int32 -> string

(** {9:serialize Serialization} *)

val vlan_none : int
val parse : Cstruct.t -> packet option
val serialize : packet -> Cstruct.t
