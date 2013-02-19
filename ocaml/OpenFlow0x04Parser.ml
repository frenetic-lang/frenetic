open Cstruct
open Cstruct.BE
open OpenFlow0x04Types

cenum ofp_oxm_class {
  OFPXMC_NXM_0 = 0x0000; (* Backward compatibility with NXM *)
  OFPXMC_NXM_1 = 0x0001; (* Backward compatibility with NXM *)
  OFPXMC_OPENFLOW_BASIC = 0x8000; (* Basic class for OpenFlow *)
  OFPXMC_EXPERIMENTER = 0xFFFF (* Experimenter class *)
} as uint32_t (* 16 bits *)

(* OXM Flow match field types for OpenFlow basic class. *)
cenum oxm_ofb_match_fields {
    OFPXMT_OFB_IN_PORT = 0;         (* Switch input port. *)
    OFPXMT_OFB_IN_PHY_PORT = 1;     (* Switch physical input port. *)
    OFPXMT_OFB_METADATA = 2;        (* Metadata passed between tables. *)
    OFPXMT_OFB_ETH_DST = 3;         (* Ethernet destination address. *)
    OFPXMT_OFB_ETH_SRC = 4;         (* Ethernet source address. *)
    OFPXMT_OFB_ETH_TYPE = 5;        (* Ethernet frame type. *)
    OFPXMT_OFB_VLAN_VID = 6;        (* VLAN id. *)
    OFPXMT_OFB_VLAN_PCP = 7;        (* VLAN priority. *)
    OFPXMT_OFB_IP_DSCP = 8;         (* IP DSCP (6 bits in ToS field). *)
    OFPXMT_OFB_IP_ECN = 9;          (* IP ECN (2 bits in ToS field). *)
    OFPXMT_OFB_IP_PROTO = 10;       (* IP protocol. *)
    OFPXMT_OFB_IPV4_SRC = 11;       (* IPv4 source address. *)
    OFPXMT_OFB_IPV4_DST = 12;       (* IPv4 destination address. *)
    OFPXMT_OFB_TCP_SRC = 13;        (* TCP source port. *)
    OFPXMT_OFB_TCP_DST = 14;        (* TCP destination port. *)
    OFPXMT_OFB_UDP_SRC = 15;        (* UDP source port. *)
    OFPXMT_OFB_UDP_DST = 16;        (* UDP destination port. *)
    OFPXMT_OFB_SCTP_SRC = 17;       (* SCTP source port. *)
    OFPXMT_OFB_SCTP_DST = 18;       (* SCTP destination port. *)
    OFPXMT_OFB_ICMPV4_TYPE = 19;    (* ICMP type. *)
    OFPXMT_OFB_ICMPV4_CODE = 20;    (* ICMP code. *)
    OFPXMT_OFB_ARP_OP = 21;         (* ARP opcode. *)
    OFPXMT_OFB_ARP_SPA = 22;        (* ARP source IPv4 address. *)
    OFPXMT_OFB_ARP_TPA = 23;        (* ARP target IPv4 address. *)
    OFPXMT_OFB_ARP_SHA = 24;        (* ARP source hardware address. *)
    OFPXMT_OFB_ARP_THA = 25;        (* ARP target hardware address. *)
    OFPXMT_OFB_IPV6_SRC = 26;       (* IPv6 source address. *)
    OFPXMT_OFB_IPV6_DST = 27;       (* IPv6 destination address. *)
    OFPXMT_OFB_IPV6_FLABEL = 28;    (* IPv6 Flow Label *)
    OFPXMT_OFB_ICMPV6_TYPE = 29;    (* ICMPv6 type. *)
    OFPXMT_OFB_ICMPV6_CODE = 30;    (* ICMPv6 code. *)
    OFPXMT_OFB_IPV6_ND_TARGET = 31; (* Target address for ND. *)
    OFPXMT_OFB_IPV6_ND_SLL = 32;    (* Source link-layer for ND. *)
    OFPXMT_OFB_IPV6_ND_TLL = 33;    (* Target link-layer for ND. *)
    OFPXMT_OFB_MPLS_LABEL = 34;     (* MPLS label. *)
    OFPXMT_OFB_MPLS_TC = 35;        (* MPLS TC. *)
    OFPXMT_OFB_MPLS_BOS = 36;       (* MPLS BoS bit. *)
    OFPXMT_OFB_PBB_ISID = 37;       (* PBB I-SID. *)
    OFPXMT_OFB_TUNNEL_ID = 38;      (* Logical Port Metadata. *)
    OFPXMT_OFB_IPV6_EXTHDR = 39     (* IPv6 Extension Header pseudo-field *)
} as uint32 (* seven bits, actually *)

module type PARSER = sig

  type t

  val parse : buf -> t

  (** [marshal t buf] writes [t] to the head of  [buf] and returns the number o
      bytes written. [marshal] throws an exception if [buf] does not have
      enough space.*)
  val marshal : t -> buf -> int

  (** [sizeof t] returns the number bytes [t] consumes when marshalled. *)
  val sizeof : t -> int

end



module Oxm : PARSER with type t = oxm = struct

  type t = oxm

  let parse buf =
    let hdr = get_uint32 buf 0 in
    let oxmClass = Int32.shift_right hdr 48 in
    let oxmField = Int32.logand (Int32.shift_right hdr 9) 0xEFl in
    failwith "NYI"
    

  let marshal t buf = failwith "NYI"

  let sizeof t = failwith "NYI"
end 
