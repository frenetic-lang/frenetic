open Packet
open Word

(* ----- Data Link Layer Structures ----- *)

cstruct eth {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t typ
} as big_endian

cstruct vlan {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t tag; (* tag and pcp *)
  uint16_t typ
} as big_endian

cenum eth_typ {
  ETHTYP_IP = 0x0800;
  ETHTYP_ARP = 0x0806;
  ETHTYP_VLAN = 0x8100
} as uint16_t

let vlan_none = 0xffff

let eth_cutoff = 0x0600

let vlan_mask = 0xfff

let vlan_pcp_mask = 0x7 lsl 9

(* ----- Network Layer Structures ----- *)

cstruct arp {
  uint16_t htype;
  uint16_t ptype;
  uint8_t hlen;
  uint8_t plen;
  uint16_t oper;
  uint8_t sha[6];
  uint32_t spa;
  uint8_t tha[6];
  uint32_t tpa
} as big_endian

cenum arp_oper {
  ARP_REQUEST = 0x0001;
  ARP_REPLY = 0x0002
} as uint16_t

cenum ip_proto { 
  IP_ICMP = 0x01;
  IP_TCP = 0x06;
  IP_UDP = 0x11
} as uint8_t

cstruct ip { 
  uint8_t vhl; (* version and ihl *)
  uint8_t tos; 
  uint16_t len;
  uint16_t ident;
  uint16_t frag; (* flags and frag *)
  uint8_t ttl;
  uint8_t proto;
  uint16_t chksum;
  uint32_t src;
  uint32_t dst;
  uint32_t options (* options and padding *)
} as big_endian

(* ----- Transport Layer Structures ----- *)

cstruct tcp { 
  uint16_t src;
  uint16_t dst;
  uint32_t seq;
  uint32_t ack;
  uint8_t offset; (* offset and reserved *)
  uint8_t flags; 
  uint16_t window;
  uint16_t chksum;
  uint16_t urgent;
  uint32_t options (* options and padding *)
} as big_endian

cstruct udp { 
  uint16_t src;
  uint16_t dst;
  uint16_t len;
  uint16_t chksum
} as big_endian

cstruct icmp { 
  uint8_t typ;
  uint8_t code;
  uint16_t chksum
} as big_endian

(* ----- Parsers ----- *)

let parse_tcp (bits:Cstruct.buf) : tcp option =
  let src = get_tcp_src bits in 
  let dst = get_tcp_dst bits in 
  let seq = get_tcp_seq bits in 
  let ack = get_tcp_ack bits in 
  let offset = get_tcp_offset bits in 
  let offset = offset lsr 4 in 
  let _ = offset land 0x0f in 
  let flags = get_tcp_flags bits in 
  let window = get_tcp_window bits in 
  let payload = Cstruct.shift bits sizeof_tcp in 
  Some { tcpSrc = Word16.from_int src;
	 tcpDst = Word16.from_int dst;
	 tcpSeq = Word32.from_int32 seq;
	 tcpAck = Word32.from_int32 ack;
	 tcpOffset = Word8.from_int offset;
	 tcpFlags = Word16.from_int flags;
	 tcpWindow = Word16.from_int window;
	 tcpPayload = payload }

(* let parse_udp (bits:Cstruct.buf) : udp option =  *)
(*   let src = get_ucp_src bits in  *)
(*   let dst = get_ucp_dst bits in  *)
(*   let chksum = get_udp_chksum bits in  *)
(*   let payload = Cstruct.shift bits sizeof_udp in  *)
(*   Some { udpSrc = Word16.from_int src; *)
(* 	 udpDst = Word16.from_int dst; *)
(* 	 udpChksum = Word16.from_int seq; *)
(* 	 udpPayload = payload } *)

let parse_icmp (bits:Cstruct.buf) : icmp option = 
  let typ = get_icmp_typ bits in 
  let code = get_icmp_code bits in 
  let chksum = get_icmp_chksum bits in 
  let payload = Cstruct.shift bits sizeof_icmp in 
  Some { icmpType = Word8.from_int typ;
	 icmpCode = Word8.from_int code;
	 icmpChksum = Word16.from_int chksum;
	 icmpPayload = payload }

let parse_ip (bits:Cstruct.buf) : ip option = 
  let vhl = get_ip_vhl bits in 
  let _ = vhl lsr 4 in (* TODO(jnf): test for IPv4? *)
  let tos = get_ip_tos bits in 
  let len = get_ip_len bits in 
  let frag = get_ip_frag bits in 
  let flags = frag lsr 13 in 
  let frag = frag land 0x1fff in 
  let ttl = get_ip_ttl bits in 
  let ident = get_ip_ident bits in 
  let proto = get_ip_proto bits in 
  let chksum = get_ip_chksum bits in 
  let src = get_ip_src bits in 
  let dst = get_ip_dst bits in 
  let tp_header = match int_to_ip_proto proto with 
    | Some IP_ICMP -> 
      begin match parse_icmp (Cstruct.shift bits len) with 
      | Some icmp -> TpICMP icmp 
      | _ -> TpUnparsable (Word8.from_int proto) 
      end
    | Some IP_TCP -> 
      begin match parse_tcp (Cstruct.shift bits len) with 
      | Some tcp -> TpTCP tcp 
      | _ -> TpUnparsable (Word8.from_int proto) 
      end
    | _ -> 
      TpUnparsable (Word8.from_int proto) in 
  Some { pktIPTos = Word8.from_int tos;
	 pktIPIdent = Word16.from_int ident;
	 pktIPFlags = Word8.from_int flags ;
	 pktFrag = Word16.from_int frag;
	 pktIPTTL = Word8.from_int ttl;
	 pktIPProto = Word8.from_int proto;
	 pktChksum = Word16.from_int chksum;
	 pktIPSrc = Word32.from_int32 src;
	 pktIPDst = Word32.from_int32 dst;     
	 pktTPHeader = tp_header }

let parse_arp (bits:Cstruct.buf) : arp option = 
  let oper = get_arp_oper bits in 
  let sha = Word48.from_bytes (Cstruct.to_string (get_arp_sha bits)) in 
  let spa = Word32.from_int32 (get_arp_spa bits) in 
  let tpa = Word32.from_int32 (get_arp_tpa bits) in 
  match int_to_arp_oper oper with 
    | Some ARP_REQUEST -> 
       Some (ARPQuery(sha,spa,tpa))
    | Some ARP_REPLY -> 
       let tha = Word48.from_bytes (Cstruct.to_string (get_arp_tha bits)) in 
       Some (ARPReply(sha,spa,tha,tpa))
    | _ -> None
    
let parse_vlan (bits:Cstruct.buf) : int * int * int * int = 
  let typ = get_eth_typ bits in 
  match int_to_eth_typ typ with 
  | Some ETHTYP_VLAN -> 
    let tag_and_pcp = get_vlan_tag bits in 
    let vlan_tag = tag_and_pcp land 0xfff in 
    let vlan_pcp = (tag_and_pcp lsr 9) land 0x7 in 
    let typ = get_vlan_typ bits in 
    (vlan_tag, vlan_pcp, typ, sizeof_vlan)
  | _ -> 
    (vlan_none, 0x0, typ, sizeof_eth)

let parse_eth (bits:Cstruct.buf) : packet option = 
  let dst = Cstruct.to_string (get_eth_dst bits) in
  let src = Cstruct.to_string (get_eth_src bits) in
  let (vlan_tag, vlan_pcp, typ, offset) = parse_vlan bits in 
  let bits = Cstruct.shift bits offset in 
  let nw_header = 
    match int_to_eth_typ typ with 
    | Some ETHTYP_IP -> 
      begin match parse_ip bits with 
      | Some ip -> NwIP ip
      | _ -> NwUnparsable typ 
      end
    | Some ETHTYP_ARP -> 
      begin match parse_arp bits with 
      | Some arp -> NwARP arp 
      | _ -> NwUnparsable typ 
      end
    | _ -> 
      NwUnparsable typ in 
  Some { pktDlSrc = Word48.from_bytes src;
	 pktDlDst = Word48.from_bytes dst;
	 pktDlTyp = Word16.from_int typ;
	 pktDlVlan = Word16.from_int vlan_tag;
	 pktDlVlanPcp = Word8.from_int vlan_pcp;
	 pktNwHeader = nw_header }

let parse_packet = parse_eth
