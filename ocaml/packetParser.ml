open Packet
open Word

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
  ETHTYP_IP = 0x800;
  ETHTYP_ARP = 0x806;
  ETHTYP_VLAN = 0x8100
} as uint16_t

let vlan_none = 0xffff

let eth_cutoff = 0x0600

let vlan_mask = 0xfff

let vlan_pcp_mask = 0x7 lsl 9

let parse_dl bits = 
  let dst = Cstruct.to_string (get_eth_dst bits) in
  let src = Cstruct.to_string (get_eth_src bits) in
  let typ_or_tag = get_eth_typ bits in
  let (vlan, vlan_pcp, typ) = match int_to_eth_typ typ_or_tag with
    | Some ETHTYP_VLAN -> 
      let tag_and_pcp = get_vlan_tag bits in
      (tag_and_pcp land 0xfff, 
       (tag_and_pcp lsr 9) land 0x7,
       get_vlan_typ bits)
    | _ -> (vlan_none, 0x0, typ_or_tag) in
 { pktDlSrc = Word48.from_bytes src;
   pktDlDst = Word48.from_bytes dst;
   pktDlTyp = typ;
   pktDlVlan = vlan;
   pktDlVlanPcp = Word8.from_int vlan_pcp;
   (* TODO(arjun): parse other headers *)
   pktNwHeader = NwUnparsable typ }
