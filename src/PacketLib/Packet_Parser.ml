open Packet

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


(* Transport *)

cstruct udp { 
  uint16_t src;
  uint16_t dst;
  uint16_t len;
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
        (Some vlan_tag, vlan_pcp, typ, sizeof_vlan)
      | _ -> 
        (None, 0x0, typ, sizeof_eth) in 
    let bits = Cstruct.shift bits offset in
    let nw_header = match int_to_eth_typ typ with 
      | Some ETHTYP_IP -> 
        begin match Ip.parse bits with 
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
        if pkt.pktDlVlan != None then sizeof_vlan 
        else sizeof_eth in 
      let nw_len = match pkt.pktNwHeader with 
        | NwIP ip -> 
          Ip.len ip
        | NwARP arp -> 
          arp_desc.len arp 
        | NwUnparsable(_,data) -> 
          Cstruct.len data in 
      eth_len + nw_len);
    serialize = (fun (bits:Cstruct.t) (pkt:packet) -> 
      set_eth_src (bytes_of_mac pkt.pktDlSrc) 0 bits;
      set_eth_dst (bytes_of_mac pkt.pktDlDst) 0 bits;
      let bits =       
        if pkt.pktDlVlan != None then 
          begin 
            set_vlan_hdr bits 0x8100;
            let vlan_tag = 
              match pkt.pktDlVlan with 
              | Some v -> v 
              | None -> vlan_none in
            let tag_and_pcp = (pkt.pktDlVlanPcp lsl 4) lor vlan_tag in 
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
        Ip.serialize bits ip
      | NwARP arp -> 
        arp_desc.serialize bits arp
      | NwUnparsable(_,data) -> 
        Cstruct.blit bits 0 data 0 (Cstruct.len data))
  }

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


(* let parse_udp (bits:Cstruct.t) : udp option =  *)
(*   let src = get_ucp_src bits in  *)
(*   let dst = get_ucp_dst bits in  *)
(*   let chksum = get_udp_chksum bits in  *)
(*   let payload = Cstruct.shift bits sizeof_udp in  *)
(*   Some { udpSrc = src; *)
(*          udpDst = dst; *)
(*          udpChksum = seq; *)
(*          udpPayload = payload } *)

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
  let vlan_tag = 
    match pkt.pktDlVlan with
    | Some v -> v
    | None -> vlan_none in
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
      (vlan_tag)
      (pkt.pktDlVlanPcp)
      (string_of_nw pkt.pktNwHeader)

let parse_packet = eth_desc.parse

let sizeof_packet = eth_desc.len

let serialize_packet (pkt:packet) : Cstruct.t = 
  let bits = Cstruct.create (eth_desc.len pkt) in 
  let () = eth_desc.serialize bits pkt in 
  bits
