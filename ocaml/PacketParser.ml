open Packet
open Util

(* Data Link *)
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
let vlan_mask = 0xfff
let vlan_pcp_mask = 0x7 lsl 9

(* Network *)
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

(* Transport *)
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

(* Parsers *)

let parse_tcp (bits:Cstruct.t) : tcp option =
  let src = get_tcp_src bits in 
  let dst = get_tcp_dst bits in 
  let seq = get_tcp_seq bits in 
  let ack = get_tcp_ack bits in 
  let offset = get_tcp_offset bits in 
  let offset = offset lsr 4 in 
  let _ = offset land 0x0f in 
  let flags = get_tcp_flags bits in 
  let window = get_tcp_window bits in 
  let payload = Cstruct.shift bits sizeof_tcp in (* JNF: FIXME *)
  Some { tcpSrc = src;
	 tcpDst = dst;
	 tcpSeq = seq;
	 tcpAck = ack;
	 tcpOffset =  offset;
	 tcpFlags = flags;
	 tcpWindow = window;
	 tcpPayload = payload }

(* let parse_udp (bits:Cstruct.t) : udp option =  *)
(*   let src = get_ucp_src bits in  *)
(*   let dst = get_ucp_dst bits in  *)
(*   let chksum = get_udp_chksum bits in  *)
(*   let payload = Cstruct.shift bits sizeof_udp in  *)
(*   Some { udpSrc = src; *)
(* 	 udpDst = dst; *)
(* 	 udpChksum = seq; *)
(* 	 udpPayload = payload } *)

let parse_icmp (bits:Cstruct.t) : icmp option = 
  let typ = get_icmp_typ bits in 
  let code = get_icmp_code bits in 
  let chksum = get_icmp_chksum bits in 
  let payload = Cstruct.shift bits sizeof_icmp in 
  Some { icmpType = typ;
	 icmpCode = code;
	 icmpChksum = chksum;
	 icmpPayload = payload }

let parse_ip (bits:Cstruct.t) : ip option = 
  let vhl = get_ip_vhl bits in 
  let _ = vhl lsr 4 in (* TODO(jnf): test for IPv4? *)
  let ihl = vhl land 0x0f in 
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
  let bits = Cstruct.shift bits ihl in 
  let tp_header = match int_to_ip_proto proto with 
    | Some IP_ICMP -> 
      begin match parse_icmp bits with 
      | Some icmp -> TpICMP icmp 
      | _ -> TpUnparsable (proto, bits) 
      end
    | Some IP_TCP ->       
      begin match parse_tcp bits with 
      | Some tcp -> TpTCP tcp 
      | _ -> TpUnparsable (proto, bits) 
      end
    | _ -> 
      TpUnparsable (proto, bits) in 
  Some { pktIPVhl = vhl;
	 pktIPTos = tos;
	 pktIPLen = len;
	 pktIPIdent = ident;
	 pktIPFlags = flags ;
	 pktIPFrag = frag;
	 pktIPTtl = ttl;
	 pktIPProto = proto;
	 pktIPChksum = chksum;
	 pktIPSrc = src;
	 pktIPDst = dst;     
	 pktTPHeader = tp_header }

let parse_arp (bits:Cstruct.t) : arp option = 
  let oper = get_arp_oper bits in 
  let sha = mac_of_bytes (Cstruct.to_string (get_arp_sha bits)) in 
  let spa = (get_arp_spa bits) in 
  let tpa = (get_arp_tpa bits) in 
  match int_to_arp_oper oper with 
    | Some ARP_REQUEST -> 
       Some (ARPQuery(sha,spa,tpa))
    | Some ARP_REPLY -> 
       let tha = mac_of_bytes (Cstruct.to_string (get_arp_tha bits)) in 
       Some (ARPReply(sha,spa,tha,tpa))
    | _ -> 
      None
    
let parse_vlan (bits:Cstruct.t) : int * int * int * int = 
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

let parse_eth (bits:Cstruct.t) : packet option = 
  let dst = Cstruct.to_string (get_eth_dst bits) in
  let src = Cstruct.to_string (get_eth_src bits) in
  let (vlan_tag, vlan_pcp, typ, offset) = parse_vlan bits in 
  let bits = Cstruct.shift bits offset in 
  let nw_header = 
    match int_to_eth_typ typ with 
    | Some ETHTYP_IP -> 
      begin match parse_ip bits with 
      | Some ip -> NwIP ip
      | _ -> NwUnparsable (typ,bits) 
      end
    | Some ETHTYP_ARP -> 
      begin match parse_arp bits with 
      | Some arp -> NwARP arp 
      | _ -> NwUnparsable (typ, bits) 
      end
    | _ -> 
      NwUnparsable (typ,bits) in 
  Some { pktDlSrc = mac_of_bytes src;
	 pktDlDst = mac_of_bytes dst;
	 pktDlTyp = typ;
	 pktDlVlan = vlan_tag;
	 pktDlVlanPcp = vlan_pcp;
	 pktNwHeader = nw_header }

let parse_packet = parse_eth

(* sizes *)

let size_tp (p:tpPkt) : int = 
  match p with 
  | TpTCP(tcp) -> 
    sizeof_tcp - 4 (* JNF: hack! *)
  | TpICMP(icmp) -> 
    sizeof_icmp
  | TpUnparsable (_,buf) -> 
    Cstruct.len buf    

let size_nw (p:nw) : int = 
  match p with 
  | NwIP(ip) -> 
    let n_nw = sizeof_ip - 4 in (* JNF: hack! *)
    let n_tp = size_tp ip.pktTPHeader in 
    n_nw + n_tp
  | NwARP(arp) -> 
    sizeof_arp
  | NwUnparsable (_,buf) -> 
    Cstruct.len buf
  
let size_eth (p:packet) : int = 
  let n_eth = 
    match p.pktDlVlan with
    | 0xffff -> sizeof_eth
    | _ -> sizeof_vlan in 
  let n_nw = size_nw p.pktNwHeader in 
  n_eth + n_nw

let size_packet = size_eth 

(* Marshalling *)

let marshal_ip (p:ip) (bits:Cstruct.t) : unit = 
  set_ip_vhl bits p.pktIPVhl;
  set_ip_tos bits p.pktIPTos;
  set_ip_ident bits p.pktIPTos;
  set_ip_frag bits (p.pktIPFlags lor (p.pktIPFrag lsl 13));
  set_ip_ttl bits p.pktIPTtl;
  set_ip_proto bits p.pktIPProto;
  set_ip_chksum bits p.pktIPChksum;
  set_ip_src bits p.pktIPSrc;
  set_ip_dst bits p.pktIPDst;
  (* JNF: TODO TCP header *)
  ()

let marshal_arp (p:arp) (bits:Cstruct.t) : unit = 
  set_arp_htype bits 1;      (* JNF: baked *)
  set_arp_ptype bits 0x800;  (* JNF: baked *)
  set_arp_hlen bits 6;       (* JNF: baked *)
  set_arp_plen bits 4;       (* JNF: baked *)
  match p with 
  | ARPQuery(sha,spa,tpa) -> 
    set_arp_oper bits (arp_oper_to_int ARP_REQUEST);
    set_arp_sha (bytes_of_mac sha) 0 bits;
    set_arp_spa bits spa;
    set_arp_tpa bits tpa;
  | ARPReply(sha,spa,tha,tpa) -> 
    set_arp_oper bits (arp_oper_to_int ARP_REPLY);
    set_arp_sha (bytes_of_mac sha) 0 bits;
    set_arp_spa bits spa;
    set_arp_tha (bytes_of_mac tha) 0 bits;
    set_arp_tpa bits tpa

let marshal_nw (p:nw) (bits:Cstruct.t) : unit = 
  match p with 
  | NwIP ip -> marshal_ip ip bits
  | NwARP arp -> marshal_arp arp bits     
  | NwUnparsable (_,data) -> 
    Cstruct.blit bits 0 data 0 (Cstruct.len data) 

let marshal_eth (p:packet) (bits:Cstruct.t) : unit = 
  set_eth_src (bytes_of_mac p.pktDlSrc) 0 bits;
  set_eth_dst (bytes_of_mac p.pktDlDst) 0 bits;
  let bits = 
    match p.pktDlVlan with 
    | 0xffff -> 
      set_eth_typ bits p.pktDlTyp;
      Cstruct.shift bits sizeof_eth
    | _ -> 
      let vlan_tag = p.pktDlVlan in 
      let vlan_pcp = p.pktDlVlanPcp in 
      let tag_and_pcp = 
	(vlan_pcp lsr (31 - 16) + 13) land (* PCP *)
	  vlan_tag lsl 4 land 0xffff in 
      set_vlan_tag bits tag_and_pcp;
      set_vlan_typ bits p.pktDlTyp;
      Cstruct.shift bits sizeof_vlan in 
  marshal_nw p.pktNwHeader bits

let marshal_packet (p:packet) : Cstruct.t = 
  let bits = Cstruct.create (size_packet p) in 
  let () = marshal_eth p bits in 
  bits
