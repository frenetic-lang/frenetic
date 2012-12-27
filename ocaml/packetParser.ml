open Packet
open Word

(* JNF: what about the 7-octet preamble and 1 octet frame delimeter? *)
cstruct eth {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t typ
} as big_endian

cstruct vlan {
  uint8_t dst[6];
  uint8_t src[6];
  (* 3-bits (MSB) are the PCP, 12-bits (LSB) are the VLAN ID tag *)
  uint16_t tag; 
  uint16_t typ
} as big_endian

cenum eth_typ {
  ETHTYP_IP = 0x0800;
  ETHTYP_ARP = 0x0806;
  ETHTYP_VLAN = 0x8100
} as uint16_t

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
  ARP_REPLY = 0x002
} as uint16_t

let vlan_none = 0xffff

let eth_cutoff = 0x0600

let vlan_mask = 0xfff

let vlan_pcp_mask = 0x7 lsl 9

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

let parse_nw (eth_typ:eth_typ option) (typ_or_tag:int) (bits:Cstruct.buf) : nw = match eth_typ with 
  | Some ETHTYP_ARP -> 
    begin match parse_arp bits with 
      | Some arp -> NwARP arp
      | _ -> NwUnparsable typ_or_tag
    end 
  | _ -> 
    NwUnparsable typ_or_tag

let parse_dl bits = 
  let dst = Cstruct.to_string (get_eth_dst bits) in
  let src = Cstruct.to_string (get_eth_src bits) in
  let typ_or_tag = get_eth_typ bits in
  let eth_typ = int_to_eth_typ typ_or_tag in
  let (vlan, vlan_pcp, vlan_typ) = match eth_typ with
    | Some ETHTYP_VLAN -> 
      let tag_and_pcp = get_vlan_tag bits in
      (tag_and_pcp land 0xfff, 
       (tag_and_pcp lsr 9) land 0x7,
        get_vlan_typ bits)
    | _ -> (vlan_none, 0x0, typ_or_tag) in
 { pktDlSrc = Word48.from_bytes src;
   pktDlDst = Word48.from_bytes dst;
   pktDlTyp = vlan_typ;
   pktDlVlan = vlan;
   pktDlVlanPcp = Word8.from_int vlan_pcp;
   pktNwHeader = parse_nw eth_typ typ_or_tag (Cstruct.shift bits sizeof_eth) }

