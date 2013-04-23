open NetworkPacket
open Misc

(* Data Link *)
cstruct eth {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t typ
} as big_endian

cstruct vlan {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t hdr; (* 0x8100 *)
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

(* Types for parsers *)
type 'a desc = 
  { parse : Cstruct.t -> 'a option;
    len : 'a -> int;
    serialize : Cstruct.t -> 'a -> unit 
  }

(* Parsers *)
(* type format = FEth | FIp | FTcp | FArp | FEnd *)
(* let rec desc_of_format f = match f with  *)
(*   | FEther -> Some eth_desc *)
(*   | FIp -> Some ip_desc *)
(*   | FTcp -> Some tcp_desc *)
(*   | FArp -> Some arp_desc  *)
(*   | FEnd -> None *)

let rec eth_desc = 
  { parse = (fun (bits:Cstruct.t) -> 
    let dst = Cstruct.to_string (get_eth_dst bits) in
    let src = Cstruct.to_string (get_eth_src bits) in
    let typ = get_eth_typ bits in 
    let (vlan_tag, vlan_pcp, typ, offset) = 
      match int_to_eth_typ typ with 
      | Some ETHTYP_VLAN -> 
	let tag_and_pcp = get_vlan_tag bits in 
	let vlan_tag = tag_and_pcp land 0xfff in 
	let vlan_pcp = tag_and_pcp lsr 13 in 
	let typ = get_vlan_typ bits in 
	(vlan_tag, vlan_pcp, typ, sizeof_vlan)
      | _ -> 
	(vlan_none, 0x0, typ, sizeof_eth) in 
    let bits = Cstruct.shift bits offset in
    let nw_header = match int_to_eth_typ typ with 
      | Some ETHTYP_IP -> 
        begin match ip_desc.parse bits with 
	| Some ip -> NwIP ip
	| None -> NwUnparsable (typ,Cstruct.of_string (Cstruct.to_string bits))
	end
      | Some ETHTYP_ARP -> 
	begin match arp_desc.parse bits with 
	| Some arp -> NwARP arp 
	| _ -> NwUnparsable (typ, Cstruct.of_string (Cstruct.to_string bits))
	end
      | _ -> 
	NwUnparsable (typ,Cstruct.of_string (Cstruct.to_string bits)) in 
    Some { pktDlSrc = mac_of_bytes src;
	   pktDlDst = mac_of_bytes dst;
	   pktDlTyp = typ;
	   pktDlVlan = vlan_tag;
	   pktDlVlanPcp = vlan_pcp;
	   pktNwHeader = nw_header });
    len = (fun (pkt:packet) ->
      let eth_len = 
	if pkt.pktDlVlan <> vlan_none then sizeof_vlan 
	else sizeof_eth in 
      let nw_len = match pkt.pktNwHeader with 
	| NwIP ip -> 
	  ip_desc.len ip
	| NwARP arp -> 
	  arp_desc.len arp 
	| NwUnparsable(_,data) -> 
	  Cstruct.len data in 
      eth_len + nw_len);
    serialize = (fun (bits:Cstruct.t) (pkt:packet) -> 
      set_eth_src (bytes_of_mac pkt.pktDlSrc) 0 bits;
      set_eth_dst (bytes_of_mac pkt.pktDlDst) 0 bits;
      let bits =       
	if pkt.pktDlVlan <> vlan_none then 
	  begin 
	    set_vlan_hdr bits 0x8100;
	    let tag_and_pcp = (pkt.pktDlVlanPcp lsl 4) lor pkt.pktDlVlan in 
	    set_vlan_tag bits tag_and_pcp;
	    set_vlan_typ bits pkt.pktDlTyp;	    	    
	    Cstruct.shift bits sizeof_vlan 
	  end
	else
	  begin 
	    set_eth_typ bits pkt.pktDlTyp;
	    Cstruct.shift bits sizeof_eth 
	  end in 
      match pkt.pktNwHeader with 
      | NwIP ip -> 
	ip_desc.serialize bits ip
      | NwARP arp -> 
	arp_desc.serialize bits arp
      | NwUnparsable(_,data) -> 
	Cstruct.blit bits 0 data 0 (Cstruct.len data))
  }

and ip_desc = 
  { parse = (fun (bits:Cstruct.t) -> 
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
	begin match icmp_desc.parse bits with 
	| Some icmp -> TpICMP icmp 
	| _ -> TpUnparsable (proto, bits) 
	end
      | Some IP_TCP ->       
	begin match tcp_desc.parse bits with 
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
	   pktTpHeader = tp_header });
    len = (fun (pkt:ip) -> 
      let ip_len = sizeof_ip - 4 in (* JNF: hack! *)
      let tp_len = match pkt.pktTpHeader with 
      | TpTCP tcp -> 
	tcp_desc.len tcp
      | TpICMP icmp -> 
	icmp_desc.len icmp
      | TpUnparsable(_,data) -> 
	Cstruct.len data in 
      ip_len + tp_len);
    serialize = (fun (bits:Cstruct.t) (pkt:ip) -> 
      set_ip_vhl bits pkt.pktIPVhl;
      set_ip_tos bits pkt.pktIPTos;
      set_ip_ident bits pkt.pktIPTos;
      set_ip_frag bits (pkt.pktIPFlags lor (pkt.pktIPFrag lsl 13));
      set_ip_ttl bits pkt.pktIPTtl;
      set_ip_proto bits pkt.pktIPProto;
      set_ip_chksum bits pkt.pktIPChksum;
      set_ip_src bits pkt.pktIPSrc;
      set_ip_dst bits pkt.pktIPDst;
      let bits = Cstruct.shift bits sizeof_ip in 
      match pkt.pktTpHeader with 
      | TpTCP tcp -> 
	tcp_desc.serialize bits tcp
      | TpICMP icmp -> 
	icmp_desc.serialize bits icmp
      | TpUnparsable (_,data) -> 
	Cstruct.blit bits 0 data 0 (Cstruct.len data)) }

and arp_desc = 
  { parse = (fun (bits:Cstruct.t) -> 
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
	None);
    len = (fun (pkt:arp) -> 
      sizeof_arp);
    serialize = (fun (bits:Cstruct.t) (pkt:arp) -> 
      set_arp_htype bits 1;      (* JNF: baked *)
      set_arp_ptype bits 0x800;  (* JNF: baked *)
      set_arp_hlen bits 6;       (* JNF: baked *)
      set_arp_plen bits 4;       (* JNF: baked *)
      match pkt with 
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
	set_arp_tpa bits tpa) }

and tcp_desc = 
  { parse = (fun (bits:Cstruct.t) -> 
      let src = get_tcp_src bits in 
      let dst = get_tcp_dst bits in 
      let seq = get_tcp_seq bits in 
      let ack = get_tcp_ack bits in 
      let offset = get_tcp_offset bits in 
      let offset = offset lsr 4 in 
      let _ = offset land 0x0f in 
      let flags = get_tcp_flags bits in 
      let window = get_tcp_window bits in 
      let chksum = get_tcp_chksum bits in 
      let urgent = get_tcp_urgent bits in 
      let payload = Cstruct.shift bits sizeof_tcp in (* JNF: options fixme *)
      Some { tcpSrc = src;
	     tcpDst = dst;
	     tcpSeq = seq;
	     tcpAck = ack;
	     tcpOffset =  offset;
	     tcpFlags = flags;
	     tcpWindow = window;
	     tcpChksum = chksum;
	     tcpUrgent = urgent;
	     tcpPayload = payload });
    len = (fun (pkt:tcp) -> sizeof_tcp);
    serialize = (fun (bits:Cstruct.t) (pkt:tcp) -> 
      set_tcp_src bits pkt.tcpSrc;
      set_tcp_dst bits pkt.tcpDst;
      set_tcp_seq bits pkt.tcpSeq;
      set_tcp_ack bits pkt.tcpAck;
      set_tcp_offset bits pkt.tcpOffset;
      set_tcp_flags bits pkt.tcpFlags;
      set_tcp_window bits pkt.tcpWindow;
      set_tcp_window bits pkt.tcpWindow;
      let bits = Cstruct.shift bits sizeof_tcp in 
      Cstruct.blit bits 0 pkt.tcpPayload 0 (Cstruct.len pkt.tcpPayload)) }

and icmp_desc = 
  { parse = (fun (bits:Cstruct.t) ->
      let typ = get_icmp_typ bits in
      let code = get_icmp_code bits in
      let chksum = get_icmp_chksum bits in
      let payload = Cstruct.shift bits sizeof_icmp in
      Some { icmpType = typ;
  	     icmpCode = code;
  	     icmpChksum = chksum;
  	     icmpPayload = payload });
    len = (fun (pkt:icmp) -> sizeof_icmp);
    serialize = (fun (bits:Cstruct.t) (pkt:icmp) ->
      set_icmp_typ bits pkt.icmpType;
      set_icmp_code bits pkt.icmpCode;
      set_icmp_chksum bits pkt.icmpChksum;
      let bits = Cstruct.shift bits sizeof_icmp in
      Cstruct.blit bits 0 pkt.icmpPayload 0 (Cstruct.len pkt.icmpPayload))
  }

(* let parse_udp (bits:Cstruct.t) : udp option =  *)
(*   let src = get_ucp_src bits in  *)
(*   let dst = get_ucp_dst bits in  *)
(*   let chksum = get_udp_chksum bits in  *)
(*   let payload = Cstruct.shift bits sizeof_udp in  *)
(*   Some { udpSrc = src; *)
(* 	 udpDst = dst; *)
(* 	 udpChksum = seq; *)
(* 	 udpPayload = payload } *)

(* Pretty Printing *)
let string_of_nw pkt = 
  match pkt with 
    | NwUnparsable(typ,data) -> 
      Printf.sprintf "%d %s [%d:%d%d%d]\n\n" typ 
	(Cstruct.debug data) 
	(Cstruct.len data)
      	(Char.code (Cstruct.get_char data 0))
      	(Char.code (Cstruct.get_char data 1))
      	(Char.code (Cstruct.get_char data 2))
    | _ -> ""

let string_of_eth pkt = 
  Printf.sprintf 
    "\n{ pktDlSrc = %s; 
  pktDlDst = %s  
  pktDlTyp : %d  
  pktDlVlan : %d  
  pktDlVlanPcp : %d
  pktNwHeader: %s }" 
      (string_of_mac pkt.pktDlSrc)
      (string_of_mac pkt.pktDlDst)
      (pkt.pktDlTyp)
      (pkt.pktDlVlan)
      (pkt.pktDlVlanPcp)
      (string_of_nw pkt.pktNwHeader)

let parse_packet = eth_desc.parse

let sizeof_packet = eth_desc.len

let serialize_packet (pkt:packet) : Cstruct.t = 
  let bits = Cstruct.create (eth_desc.len pkt) in 
  let () = eth_desc.serialize bits pkt in 
  bits
