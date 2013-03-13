(** OpenFlow 1.3 (protocol version 0x04) *)

open Printf
open Cstruct
open Cstruct.BE
open OpenFlow0x04Types
open Util
open List
open PacketParser

exception Unparsable of string
let sym_num = ref 0

let sum (lst : int list) = List.fold_left (fun x y -> x + y) 0 lst

(* OKAY *)
cstruct ofp_header {
  uint8_t version;    
  uint8_t typ;   
  uint16_t length;    
  uint32_t xid
} as big_endian

(* OKAY *)
cenum msg_code {
  HELLO;
  ERROR;
  ECHO_REQ;
  ECHO_RESP;
  VENDOR;
  FEATURES_REQ;
  FEATURES_RESP;
  GET_CONFIG_REQ;
  GET_CONFIG_RESP;
  SET_CONFIG;
  PACKET_IN;
  FLOW_REMOVED;
  PORT_STATUS;
  PACKET_OUT;
  FLOW_MOD;
  GROUP_MOD;
  PORT_MOD;
  TABLE_MOD;
  MULTIPART_REQ;
  MULTIPART_RESP;
  BARRIER_REQ;
  BARRIER_RESP;
  QUEUE_GET_CONFIG_REQ;
  QUEUE_GET_CONFIG_RESP;
  ROLE_REQ;
  ROLE_RESP;
  GET_ASYNC_REQ;
  GET_ASYNC_REP;
  METER_MOD
} as uint8_t

cstruct ofp_match {
  uint16_t typ;          
  uint16_t length
} as big_endian

let ofpp_in_port = 0xfffffff8l
let ofpp_flood = 0xfffffffbl
let ofpp_all = 0xfffffffcl
let ofpp_controller = 0xfffffffdl
let ofpp_any = 0xffffffffl

(* Not in the spec, comes from C headers. :rolleyes: *)
let ofpg_all = 0xfffffffcl
let ofpg_any = 0xffffffffl

(* OKAY *)
cenum ofp_oxm_class {
  OFPXMC_NXM_0          = 0x0000;    (* Backward compatibility with NXM *)
  OFPXMC_NXM_1          = 0x0001;    (* Backward compatibility with NXM *)
  OFPXMC_OPENFLOW_BASIC = 0x8000;    (* Basic class for OpenFlow *)
  OFPXMC_EXPERIMENTER   = 0xFFFF     (* Experimenter class *)
} as uint16_t

(* OKAY *)
cenum oxm_ofb_match_fields {
  OFPXMT_OFB_IN_PORT        = 0;  (* Switch input port. *)
  OFPXMT_OFB_IN_PHY_PORT    = 1;  (* Switch physical input port. *)
  OFPXMT_OFB_METADATA       = 2;  (* Metadata passed between tables. *)
  OFPXMT_OFB_ETH_DST        = 3;  (* Ethernet destination address. *)
  OFPXMT_OFB_ETH_SRC        = 4;  (* Ethernet source address. *)
  OFPXMT_OFB_ETH_TYPE       = 5;  (* Ethernet frame type. *)
  OFPXMT_OFB_VLAN_VID       = 6;  (* VLAN id. *)
  OFPXMT_OFB_VLAN_PCP       = 7;  (* VLAN priority. *)
  OFPXMT_OFB_IP_DSCP        = 8;  (* IP DSCP (6 bits in ToS field). *)
  OFPXMT_OFB_IP_ECN         = 9;  (* IP ECN (2 bits in ToS field). *)
  OFPXMT_OFB_IP_PROTO       = 10; (* IP protocol. *)
  OFPXMT_OFB_IPV4_SRC       = 11; (* IPv4 source address. *)
  OFPXMT_OFB_IPV4_DST       = 12; (* IPv4 destination address. *)
  OFPXMT_OFB_TCP_SRC        = 13; (* TCP source port. *)
  OFPXMT_OFB_TCP_DST        = 14; (* TCP destination port. *)
  OFPXMT_OFB_UDP_SRC        = 15; (* UDP source port. *)
  OFPXMT_OFB_UDP_DST        = 16; (* UDP destination port. *)
  OFPXMT_OFB_SCTP_SRC       = 17; (* SCTP source port. *)
  OFPXMT_OFB_SCTP_DST       = 18; (* SCTP destination port. *)
  OFPXMT_OFB_ICMPV4_TYPE    = 19; (* ICMP type. *)
  OFPXMT_OFB_ICMPV4_CODE    = 20; (* ICMP code. *)
  OFPXMT_OFB_ARP_OP         = 21; (* ARP opcode. *)
  OFPXMT_OFB_ARP_SPA        = 22; (* ARP source IPv4 address. *)
  OFPXMT_OFB_ARP_TPA        = 23; (* ARP target IPv4 address. *)
  OFPXMT_OFB_ARP_SHA        = 24; (* ARP source hardware address. *)
  OFPXMT_OFB_ARP_THA        = 25; (* ARP target hardware address. *)
  OFPXMT_OFB_IPV6_SRC       = 26; (* IPv6 source address. *)
  OFPXMT_OFB_IPV6_DST       = 27; (* IPv6 destination address. *)
  OFPXMT_OFB_IPV6_FLABEL    = 28; (* IPv6 Flow Label *)
  OFPXMT_OFB_ICMPV6_TYPE    = 29; (* ICMPv6 type. *)
  OFPXMT_OFB_ICMPV6_CODE    = 30; (* ICMPv6 code. *)
  OFPXMT_OFB_IPV6_ND_TARGET = 31; (* Target address for ND. *)
  OFPXMT_OFB_IPV6_ND_SLL    = 32; (* Source link-layer for ND. *)
  OFPXMT_OFB_IPV6_ND_TLL    = 33; (* Target link-layer for ND. *)
  OFPXMT_OFB_MPLS_LABEL     = 34; (* MPLS label. *)
  OFPXMT_OFB_MPLS_TC        = 35; (* MPLS TC. *)
  OFPXMT_OFP_MPLS_BOS       = 36; (* MPLS BoS bit. *)
  OFPXMT_OFB_PBB_ISID       = 37; (* PBB I-SID. *)
  OFPXMT_OFB_TUNNEL_ID      = 38; (* Logical Port Metadata. *)
  OFPXMT_OFB_IPV6_EXTHDR    = 39  (* IPv6 Extension Header pseudo-field *)
} as uint8_t

cenum ofp_vlan_id {
  OFPVID_PRESENT = 0x1000; (* Bit that indicate that a VLAN id is set *)
  OFPVID_NONE    = 0x0000  (* No VLAN id was set. *)
} as uint16_t

cstruct ofp_switch_features {
  uint64_t datapath_id;
  uint32_t n_buffers;
  uint8_t n_tables;
  uint8_t auxiliary_id;
  uint8_t pad0;
  uint8_t pad1;
  uint8_t pad2;
  uint32_t capabilities; 
  uint32_t reserved
} as big_endian 


(* MISSING: ofp_port_config *)
(* MISSING: ofp_port_state *)
(* MISSING: ofp_port_features *)

(* MISSING: ofp_ queues *)

cstruct ofp_flow_mod {
  uint64_t cookie;             (* Opaque controller-issued identifier. *)
  uint64_t cookie_mask;        (* Mask used to restrict the cookie bits
                                  that must match when the command is
                                  OFPFC_MODIFY* or OFPFC_DELETE*. A value
                                  of 0 indicates no restriction. *)

  (* Flow actions. *)
  uint8_t table_id;             (* ID of the table to put the flow in.
                                   For OFPFC_DELETE_* commands, OFPTT_ALL
                                   can also be used to delete matching
                                   flows from all tables. *)
  uint8_t command;              (* One of OFPFC_*. *)
  uint16_t idle_timeout;        (* Idle time before discarding (seconds). *)
  uint16_t hard_timeout;        (* Max time before discarding (seconds). *)
  uint16_t priority;            (* Priority level of flow entry. *)
  uint32_t buffer_id;           (* Buffered packet to apply to, or
                                   OFP_NO_BUFFER.
                                   Not meaningful for OFPFC_DELETE*. *)
  uint32_t out_port;            (* For OFPFC_DELETE* commands, require
                                   matching entries to include this as an
                                   output port.  A value of OFPP_ANY
                                   indicates no restriction. *)
  uint32_t out_group;           (* For OFPFC_DELETE* commands, require
                                   matching entries to include this as an
                                   output group.  A value of OFPG_ANY
                                   indicates no restriction. *)
  uint16_t flags;               (* One of OFPFF_*. *)
  uint8_t pad0;
  uint8_t pad1
} as big_endian

(* OKAY *)
cenum ofp_action_type {
  OFPAT_OUTPUT       = 0;  (* Output to switch port. *)
  OFPAT_COPY_TTL_OUT = 11; (* Copy TTL "outwards" -- from next-to-outermost
                              to outermost *)
  OFPAT_COPY_TTL_IN  = 12; (* Copy TTL "inwards" -- from outermost to
                             next-to-outermost *)
  OFPAT_SET_MPLS_TTL = 15; (* MPLS TTL *)
  OFPAT_DEC_MPLS_TTL = 16; (* Decrement MPLS TTL *)

  OFPAT_PUSH_VLAN    = 17; (* Push a new VLAN tag *)
  OFPAT_POP_VLAN     = 18; (* Pop the outer VLAN tag *)
  OFPAT_PUSH_MPLS    = 19; (* Push a new MPLS tag *)
  OFPAT_POP_MPLS     = 20; (* Pop the outer MPLS tag *)
  OFPAT_SET_QUEUE    = 21; (* Set queue id when outputting to a port *)
  OFPAT_GROUP        = 22; (* Apply group. *)
  OFPAT_SET_NW_TTL   = 23; (* IP TTL. *)
  OFPAT_DEC_NW_TTL   = 24; (* Decrement IP TTL. *)
  OFPAT_SET_FIELD    = 25; (* Set a header field using OXM TLV format. *)
  OFPAT_PUSH_PBB     = 26; (* Push a new PBB service tag (I-TAG) *)
  OFPAT_POP_PBB      = 27; (* Pop the outer PBB service tag (I-TAG) *)
  OFPAT_EXPERIMENTER = 0xffff
} as uint16_t

(* Action structure for OFPAT_OUTPUT, which sends packets out 'port'.
 * When the 'port' is the OFPP_CONTROLLER, 'max_len' indicates the max
 * number of bytes to send.  A 'max_len' of zero means no bytes of the
 * packet should be sent. A 'max_len' of OFPCML_NO_BUFFER means that
 * the packet is not buffered and the complete packet is to be sent to
 * the controller. *)
cstruct ofp_action_output {
    uint16_t typ;                   (* OFPAT_OUTPUT. *)
    uint16_t len;                   (* Length is 16. *)
    uint32_t port;                  (* Output port. *)
    uint16_t max_len;               (* Max length to send to controller. *)
    uint8_t pad0;                   (* Pad to 64 bits. *)
    uint8_t pad1;                   (* Pad to 64 bits. *)
    uint8_t pad2;                   (* Pad to 64 bits. *)
    uint8_t pad3;                   (* Pad to 64 bits. *)
    uint8_t pad4;                   (* Pad to 64 bits. *)
    uint8_t pad5                    (* Pad to 64 bits. *)
} as big_endian

(* Action structure for OFPAT_GROUP. *)
cstruct ofp_action_group {
  uint16_t typ;                   (* OFPAT_GROUP. *)
  uint16_t len;                   (* Length is 8. *)
  uint32_t group_id               (* Group identifier. *)
} as big_endian

(* Action structure for OFPAT_SET_FIELD. *)
cstruct ofp_action_set_field {
    uint16_t typ;                  (* OFPAT_SET_FIELD. *)
    uint16_t len                   (* Length is padded to 64 bits. *)
    (* Followed by:
     *   - Exactly oxm_len bytes containing a single OXM TLV, then
     *   - Exactly ((oxm_len + 4) + 7)/8*8 - (oxm_len + 4) (between 0 and 7)
     *     bytes of all-zero bytes
     *)
} as big_endian

(* Instruction header that is common to all instructions.  The length includes
 * the header and any padding used to make the instruction 64-bit aligned.
 * NB: The length of an instruction *must* always be a multiple of eight. *)
cstruct ofp_instruction {
    uint16_t typ;                 (* Instruction type *)
    uint16_t len                  (* Length of this struct in bytes. *)
} as big_endian

(* Instruction structure for OFPIT_GOTO_TABLE *)
cstruct ofp_instruction_goto_table {
    uint16_t typ;                 (* OFPIT_GOTO_TABLE *)
    uint16_t len;                 (* Length of this struct in bytes. *)
    uint8_t table_id;             (* Set next table in the lookup pipeline *)
    uint8_t pad0;                 (* Pad to 64 bits. *)
    uint8_t pad1;
    uint8_t pad2
} as big_endian

(* Instruction structure for OFPIT_WRITE_METADATA *)
cstruct ofp_instruction_write_metadata {
    uint16_t typ;                 (* OFPIT_WRITE_METADATA *)
    uint16_t len;                 (* Length of this struct in bytes. *)
    uint8_t pad0;                 (* Align to 64-bits *)
    uint8_t pad1;
    uint8_t pad2;
    uint8_t pad3;
    uint64_t metadata;            (* Metadata value to write *)
    uint64_t metadata_mask        (* Metadata write bitmask *)
} as big_endian

(* Instruction structure for OFPIT_WRITE/APPLY/CLEAR_ACTIONS *)
cstruct ofp_instruction_actions {
    uint16_t typ;               (* One of OFPIT_*_ACTIONS *)
    uint16_t len;               (* Length of this struct in bytes. *)
    uint8_t pad0;               (* Align to 64-bits *)
    uint8_t pad1;
    uint8_t pad2;
    uint8_t pad3
} as big_endian

(* Instruction structure for OFPIT_METER *)
cstruct ofp_instruction_meter {
    uint16_t typ;                 (* OFPIT_METER *)
    uint16_t len;                 (* Length is 8. *)
    uint32_t meter_id             (* Meter instance. *)
} as big_endian

(* Instruction structure for experimental instructions *)
cstruct ofp_instruction_experimenter {
    uint16_t typ;               (* OFPIT_EXPERIMENTER *)
    uint16_t len;               (* Length of this struct in bytes *)
    uint32_t experimenter       (* Experimenter ID which takes the same form
                                   as in struct ofp_experimenter_header. *)
    (* Experimenter-defined arbitrary additional data. *)
} as big_endian

(* Group setup and teardown (controller -> datapath). *)
cstruct ofp_group_mod {
  uint16_t command;             (* One of OFPGC_*. *)
  uint8_t typ;                 (* One of OFPGT_*. *)
  uint8_t pad;                  (* Pad to 64 bits. *)
  uint32_t group_id            (* Group identifier. *)
} as big_endian

(* Bucket for use in groups. *)
cstruct ofp_bucket {
  uint16_t len;                   (* Length the bucket in bytes, including
                                     this header and any padding to make it
                                     64-bit aligned. *)
  uint16_t weight;                (* Relative weight of bucket.  Only
                                     defined for select groups. *)
  uint32_t watch_port;            (* Port whose state affects whether this
                                     bucket is live.  Only required for fast
                                     failover groups. *)
  uint32_t watch_group;           (* Group whose state affects whether this
                                     bucket is live.  Only required for fast
                                     failover groups. *)
  uint8_t pad0;
  uint8_t pad1;
  uint8_t pad2;
  uint8_t pad3
} as big_endian

cstruct ofp_oxm {
  uint16_t oxm_class;
  uint8_t oxm_field_and_hashmask;
  uint8_t oxm_length
} as big_endian

cstruct ofp_uint8 {
  uint8_t value
} as big_endian

cstruct ofp_uint16 {
  uint16_t value
} as big_endian

cstruct ofp_uint32 {
  uint32_t value
} as big_endian

cstruct ofp_uint48 {
  uint32_t high;
  uint16_t low
} as big_endian

cstruct ofp_uint64 {
  uint64_t value
} as big_endian

let set_ofp_uint48_value (buf : Cstruct.t) (value : uint48) =
  let high = Int32.of_int ((Int64.to_int value) lsr 16) in
    let low = ((Int64.to_int value) land 0xffff) in
      set_ofp_uint48_high buf high;
      set_ofp_uint48_low buf low


let rec marshal_fields (buf: Cstruct.t) (fields : 'a list) (marshal_func : Cstruct.t -> 'a -> int ): int =
  if (fields = []) then 0
  else let size = marshal_func buf (List.hd fields) in
    size + (marshal_fields (Cstruct.shift buf size) (List.tl fields) marshal_func)

let pad_to_64bits (n : int) : int =
  if n land 0x7 <> 0 then
    n + (8 - (n land 0x7))
  else
    n

let rec pad_with_zeros (buf : Cstruct.t) (pad : int) : int =
  if pad = 0 then 0
  else begin set_ofp_uint8_value buf 0;
    1 + pad_with_zeros (Cstruct.shift buf 1) (pad - 1) end

module Oxm = struct

  let field_length (oxm : oxm) : int = match oxm with
    | OxmInPort _ -> 4
    | OxmInPhyPort _ -> 4
    | OxmEthType  _ -> 2
    | OxmEthDst ethaddr ->
      (match ethaddr.mask with
        | None -> 6
        | Some _ -> 12)
    | OxmEthSrc ethaddr ->
      (match ethaddr.mask with
        | None -> 6
        | Some _ -> 12)
    | OxmVlanVId vid ->
      (match vid.mask with
        | None -> 2
        | Some _ -> 4)
    | OxmIP4Src ipaddr -> 
      (match ipaddr.mask with
        | None -> 4
        | Some _ -> 8)
    | OxmIP4Dst ipaddr ->       
      (match ipaddr.mask with
        | None -> 4
        | Some _ -> 8)
    | OxmTCPSrc _ -> failwith "Invalid field_length TCPSrc"
    | OxmTCPDst _ -> failwith "Invalid field_length TCPDst"
    | _ -> failwith "Invalid field_length"

  let sizeof (oxm : oxm) : int =
    sizeof_ofp_oxm + field_length oxm

  let set_ofp_oxm (buf : Cstruct.t) (c : ofp_oxm_class) (f : oxm_ofb_match_fields) (hm : int) (l : int) = 
    let value = (0x3f land (oxm_ofb_match_fields_to_int f)) lsl 1 in
      let value = value lor (0x1 land hm) in
        set_ofp_oxm_oxm_class buf (ofp_oxm_class_to_int c);
        set_ofp_oxm_oxm_field_and_hashmask buf value;
        set_ofp_oxm_oxm_length buf l


  let marshal (buf : Cstruct.t) (oxm : oxm) : int = 
    let l = field_length oxm in
      let ofc = OFPXMC_OPENFLOW_BASIC in
        let buf2 = Cstruct.shift buf sizeof_ofp_oxm in
          match oxm with
            | OxmInPort pid ->
              set_ofp_oxm buf ofc OFPXMT_OFB_IN_PORT 0 l;
              set_ofp_uint32_value buf2 pid;
              sizeof_ofp_oxm + l
            | OxmInPhyPort pid ->
              set_ofp_oxm buf ofc OFPXMT_OFB_IN_PHY_PORT 0 l;
              set_ofp_uint32_value buf2 pid;
              sizeof_ofp_oxm + l
            | OxmEthType ethtype ->
              set_ofp_oxm buf ofc OFPXMT_OFB_ETH_TYPE 0 l;
              set_ofp_uint16_value buf2 ethtype;
              sizeof_ofp_oxm + l
            | OxmEthDst ethaddr ->
              set_ofp_oxm buf ofc OFPXMT_OFB_ETH_DST (match ethaddr.mask with None -> 0 | _ -> 1) l;
              set_ofp_uint48_value buf2 ethaddr.value;
              begin match ethaddr.mask with
                | None ->
                  sizeof_ofp_oxm + l
                | Some mask ->
                  let buf3 = Cstruct.shift buf2 (l/2) in
                    set_ofp_uint48_value buf3 mask;
                    sizeof_ofp_oxm + l
              end
            | OxmEthSrc ethaddr ->
              set_ofp_oxm buf ofc OFPXMT_OFB_ETH_SRC (match ethaddr.mask with None -> 0 | _ -> 1) l;
              set_ofp_uint48_value buf2 ethaddr.value;
              begin match ethaddr.mask with
                | None ->
                  sizeof_ofp_oxm + l
                | Some mask ->
                  let buf3 = Cstruct.shift buf2 (l/2) in
                    set_ofp_uint48_value buf3 mask;
                    sizeof_ofp_oxm + l
              end
            | OxmIP4Src ipaddr ->
              set_ofp_oxm buf ofc OFPXMT_OFB_IPV4_SRC (match ipaddr.mask with None -> 0 | _ -> 1) l;
              set_ofp_uint32_value buf2 ipaddr.value;
              begin match ipaddr.mask with
                | None ->
                  sizeof_ofp_oxm + l
                | Some mask ->
                  let buf3 = Cstruct.shift buf2 (l/2) in
                    set_ofp_uint32_value buf3 mask;
                    sizeof_ofp_oxm + l
              end
            | OxmIP4Dst ipaddr ->
              set_ofp_oxm buf ofc OFPXMT_OFB_IPV4_DST (match ipaddr.mask with None -> 0 | _ -> 1) l;
              set_ofp_uint32_value buf2 ipaddr.value;
              begin match ipaddr.mask with
                | None ->
                  sizeof_ofp_oxm + l
                | Some mask ->
                  let buf3 = Cstruct.shift buf2 (l/2) in
                    set_ofp_uint32_value buf3 mask;
                    sizeof_ofp_oxm + l
              end
            | OxmVlanVId vid ->
              set_ofp_oxm buf ofc OFPXMT_OFB_VLAN_VID (match vid.mask with None -> 0 | _ -> 1) l;
              set_ofp_uint16_value buf2 vid.value;
              begin match vid.mask with
                | None ->
                  sizeof_ofp_oxm + l
                | Some mask ->
                  let buf3 = Cstruct.shift buf2 (l/2) in
                    set_ofp_uint16_value buf3 mask;
                    sizeof_ofp_oxm + l
              end
            | _ -> failwith "Invalid marshal of oxm"

  let parse (bits : Cstruct.t) : oxm * Cstruct.t =
    (* printf "class= %d\n" (get_ofp_oxm_oxm_class bits); *)
    let c = match int_to_ofp_oxm_class (get_ofp_oxm_oxm_class bits) with
      | Some n -> n
      | None -> 
        raise (Unparsable (sprintf "malformed class in oxm")) in
    (* TODO: assert c is OFPXMC_OPENFLOW_BASIC *)
    let value = get_ofp_oxm_oxm_field_and_hashmask bits in
    let f = match int_to_oxm_ofb_match_fields (value lsr 1) with
      | Some n -> n
      | None -> 
        raise (Unparsable (sprintf "malformed field in oxm %d" (value lsr 1))) in
    let hm = value land 0x1 in
    let oxm_length = get_ofp_oxm_oxm_length bits in
    let bits = Cstruct.shift bits sizeof_ofp_oxm in
    let bits2 = Cstruct.shift bits oxm_length in
    match f with
      | OFPXMT_OFB_IN_PORT ->
        let pid = get_ofp_uint32_value bits in
        (OxmInPort pid, bits2)
      | OFPXMT_OFB_IN_PHY_PORT ->
        let pid = get_ofp_uint32_value bits in
        (OxmInPhyPort pid, bits2)
      | OFPXMT_OFB_METADATA ->
        let value = get_ofp_uint64_value bits in
        if hm = 1 then
          let bits = Cstruct.shift bits 8 in
          let mask = get_ofp_uint64_value bits in
          (OxmMetadata {value = value; mask = (Some mask)}, bits2)
        else
          (OxmMetadata {value = value; mask = None}, bits2)
      | OFPXMT_OFB_TUNNEL_ID ->
        let value = get_ofp_uint64_value bits in
        if hm = 1 then
          let bits = Cstruct.shift bits 8 in
          let mask = get_ofp_uint64_value bits in
          (OxmTunnelId {value = value; mask = (Some mask)}, bits2)
        else
          (OxmTunnelId {value = value; mask = None}, bits2)
      (* Ethernet destination address. *)
      | OFPXMT_OFB_ETH_DST ->
	let value = get_ofp_uint64_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 6 in
	  let mask = get_ofp_uint64_value bits in
	  (OxmEthDst {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmEthDst {value = value; mask = None}, bits2)
      (* Ethernet source address. *)
      | OFPXMT_OFB_ETH_SRC ->
	let value = get_ofp_uint64_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 6 in
	  let mask = get_ofp_uint64_value bits in
	  (OxmEthSrc {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmEthSrc {value = value; mask = None}, bits2)
      (* Ethernet frame type. *)
      | OFPXMT_OFB_ETH_TYPE ->
	let value = get_ofp_uint16_value bits in
	  (OxmEthType value, bits2)
      (* IP protocol. *)
      | OFPXMT_OFB_IP_PROTO ->
	let value = get_ofp_uint8_value bits in
	  (OxmIPProto value, bits2)
      (* IP DSCP (6 bits in ToS field). *)
      | OFPXMT_OFB_IP_DSCP ->
	let value = get_ofp_uint8_value bits in
	  (OxmIPDscp (value land 252), bits2)
      (* IP ECN (2 bits in ToS field). *)
      |  OFPXMT_OFB_IP_ECN ->
	let value = get_ofp_uint8_value bits in
	  (OxmIPEcn (value land 3), bits2)
      (* IPv4 source address. *)
      | OFPXMT_OFB_IPV4_SRC ->
	let value = get_ofp_uint32_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 4 in
	  let mask = get_ofp_uint32_value bits in
	  (OxmIP4Src {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmIP4Src {value = value; mask = None}, bits2)
      (* IPv4 destination address. *)
      | OFPXMT_OFB_IPV4_DST ->
	let value = get_ofp_uint32_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 4 in
	  let mask = get_ofp_uint32_value bits in
	  (OxmIP4Dst {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmIP4Dst {value = value; mask = None}, bits2)
      (* ARP opcode. *)
      | OFPXMT_OFB_ARP_OP ->
	let value = get_ofp_uint16_value bits in
	  (OxmARPOp value, bits2)
      (* ARP source IPv4 address. *)
      | OFPXMT_OFB_ARP_SPA ->
	let value = get_ofp_uint32_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 4 in
	  let mask = get_ofp_uint32_value bits in
	  (OxmARPSpa {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmARPSpa {value = value; mask = None}, bits2)
      (* ARP target IPv4 address. *)
      | OFPXMT_OFB_ARP_TPA ->
	let value = get_ofp_uint32_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 4 in
	  let mask = get_ofp_uint32_value bits in
	  (OxmARPTpa {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmARPTpa {value = value; mask = None}, bits2)
      (* ARP source hardware address. *)
      | OFPXMT_OFB_ARP_SHA ->
	let value = get_ofp_uint64_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 6 in
	  let mask = get_ofp_uint64_value bits in
	  (OxmARPSha {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmARPSha {value = value; mask = None}, bits2)
      (* ARP target hardware address. *)
      | OFPXMT_OFB_ARP_THA ->
	let value = get_ofp_uint64_value bits in
	if hm = 1 then
	  let bits = Cstruct.shift bits 6 in
	  let mask = get_ofp_uint64_value bits in
	  (OxmARPTha {value = value; mask = (Some mask)}, bits2)
	else
	  (OxmARPTha {value = value; mask = None}, bits2)
      (* ICMP Type *)
      | OFPXMT_OFB_ICMPV4_TYPE ->
	let value = get_ofp_uint8_value bits in
	  (OxmICMPType value, bits2)
      (* ICMP code. *)
      |   OFPXMT_OFB_ICMPV4_CODE ->
	let value = get_ofp_uint8_value bits in
	  (OxmICMPCode value, bits2)
      | _ -> 
        raise (Unparsable (sprintf "malformed packet in oxm %d\n" (value lsr 1)))

end

module Action = struct
    
  let sizeof (act : action) : int = match act with
    | Output _ -> sizeof_ofp_action_output
    | Group _ -> sizeof_ofp_action_group
    | SetField oxm -> pad_to_64bits (sizeof_ofp_action_set_field + Oxm.sizeof oxm)


  let marshal (buf : Cstruct.t) (act : action) : int =
    let size = sizeof act in
    match act with
      | Output port ->
        set_ofp_action_output_typ buf 0; (* OFPAT_OUTPUT *)
        set_ofp_action_output_len buf size;
        set_ofp_action_output_port buf
          (match port with
            | PhysicalPort pid -> pid
            | InPort -> ofpp_in_port         (* OFPP_IN_PORT *)
            | Flood -> ofpp_flood          (* OFPP_FLOOD *)
            | AllPorts -> ofpp_all       (* OFPP_ALL *)
            | Controller _ -> ofpp_controller   (* OFPP_CONTROLLER *)
            | Any -> ofpp_any);          (* OFPP_ANY *)
        set_ofp_action_output_max_len buf
          (match port with
            | Controller max_len -> max_len
            | _ -> 0);
        set_ofp_action_output_pad0 buf 0;
        set_ofp_action_output_pad1 buf 0;
        set_ofp_action_output_pad2 buf 0;
        set_ofp_action_output_pad3 buf 0;
        set_ofp_action_output_pad4 buf 0;
        set_ofp_action_output_pad5 buf 0;
        size
      | Group gid ->
        set_ofp_action_group_typ buf 22; (* OFPAT_GROUP *)
        set_ofp_action_group_len buf size;
        set_ofp_action_group_group_id buf gid;
        size
      | SetField oxm ->
        set_ofp_action_set_field_typ buf 25; (* OFPAT_SET_FIELD *)
        set_ofp_action_set_field_len buf size;
        let buf = Cstruct.shift buf sizeof_ofp_action_set_field in
        let oxm_size = Oxm.marshal buf oxm in
        let pad = size - (sizeof_ofp_action_set_field + oxm_size) in
        (* printf "pad = %d\n" pad; *)
        if pad > 0 then
          let buf = Cstruct.shift buf oxm_size in
          let _ = pad_with_zeros buf pad in
          size
        else size

end

module Bucket = struct

  let sizeof (bucket : bucket) : int =
    let n = sizeof_ofp_bucket + sum (map Action.sizeof bucket.actions) in
    pad_to_64bits n

  let marshal (buf : Cstruct.t) (bucket : bucket) : int =
    let size = sizeof bucket in
      set_ofp_bucket_len buf size;
      set_ofp_bucket_weight buf bucket.weight;
      set_ofp_bucket_watch_port buf
        (match bucket.watch_port with
          | None -> ofpg_any
          | Some port -> port);
      set_ofp_bucket_watch_group buf
        (match bucket.watch_group with
          | None -> ofpg_any
          | Some group_id -> group_id);
      set_ofp_bucket_pad0 buf 0;
      set_ofp_bucket_pad1 buf 0;
      set_ofp_bucket_pad2 buf 0;
      set_ofp_bucket_pad3 buf 0;
      sizeof_ofp_bucket + (marshal_fields (Cstruct.shift buf sizeof_ofp_bucket) bucket.actions Action.marshal)

end

module GroupMod = struct

  let sizeof (gm: groupMod) : int =
    match gm with
      | AddGroup (typ, gid, buckets) -> 
        sizeof_ofp_group_mod + sum (map Bucket.sizeof buckets)
      | DeleteGroup (typ, gid) -> 
        sizeof_ofp_group_mod

  let marshal (buf : Cstruct.t) (gm : groupMod) : int =
    match gm with
      | AddGroup (typ, gid, buckets) -> 
        set_ofp_group_mod_command buf 0; (* OFPGC_ADD *)
        set_ofp_group_mod_typ buf (groupType_to_int typ);
        set_ofp_group_mod_pad buf 0;
        set_ofp_group_mod_group_id buf gid;
        sizeof_ofp_group_mod + (marshal_fields (Cstruct.shift buf sizeof_ofp_group_mod) buckets Bucket.marshal)
      | DeleteGroup (typ, gid) ->
        set_ofp_group_mod_command buf 2; (* OFPGC_DEL *)
        set_ofp_group_mod_typ buf (groupType_to_int typ);
        set_ofp_group_mod_pad buf 0;
        set_ofp_group_mod_group_id buf gid;
        sizeof_ofp_group_mod
end

module OfpMatch = struct

  let sizeof (om : oxmMatch) : int =
    let n = sizeof_ofp_match + sum (map Oxm.sizeof om) in
    pad_to_64bits n

  let marshal (buf : Cstruct.t) (om : oxmMatch) : int =
    let size = sizeof om in
    set_ofp_match_typ buf 1; (* OXPMT_OXM *)
    set_ofp_match_length buf (sizeof_ofp_match + sum (map Oxm.sizeof om)); (* Length of ofp_match (excluding padding) *)
    let buf = Cstruct.shift buf sizeof_ofp_match in
    let oxm_size = marshal_fields buf om Oxm.marshal in
    let pad = size - (sizeof_ofp_match + oxm_size) in
    if pad > 0 then
      let buf = Cstruct.shift buf oxm_size in
      let _ = pad_with_zeros buf pad in
      size
    else size

  let rec parse_fields (bits : Cstruct.t) : oxmMatch * Cstruct.t =
    if Cstruct.len bits <= sizeof_ofp_oxm then ([], bits)
    else let field, bits2 = Oxm.parse bits in
    let fields, bits3 = parse_fields bits2 in
    (List.append [field] fields, bits3)

  let parse (bits : Cstruct.t) : oxmMatch * Cstruct.t =
    let typ = get_ofp_match_typ bits in
    let length = get_ofp_match_length bits in
    let oxm_bits = Cstruct.sub bits sizeof_ofp_match (length - sizeof_ofp_match) in
    let fields, _ = parse_fields oxm_bits in
    let bits = Cstruct.shift bits (pad_to_64bits length) in
    (fields, bits)

end

module Instruction = struct

  let sizeof (ins : instruction) : int =
    match ins with
      | GotoTable _ ->
        sizeof_ofp_instruction_goto_table
      | WriteActions actions ->
        sizeof_ofp_instruction_actions + sum (map Action.sizeof actions)

  let marshal (buf : Cstruct.t) (ins : instruction) : int =
    let size = sizeof ins in
      match ins with
        | GotoTable table_id ->
          set_ofp_instruction_goto_table_typ buf 1; (* OFPIT_GOTO_TABLE *)
          set_ofp_instruction_goto_table_len buf size;
          set_ofp_instruction_goto_table_table_id buf table_id;
          set_ofp_instruction_goto_table_pad0 buf 0;
          set_ofp_instruction_goto_table_pad1 buf 0;
          set_ofp_instruction_goto_table_pad2 buf 0;
          size
        | WriteActions actions ->
          set_ofp_instruction_actions_typ buf 3; (* OFPIT_WRITE_ACTIONS *)
          set_ofp_instruction_actions_len buf size;
          set_ofp_instruction_actions_pad0 buf 0;
          set_ofp_instruction_actions_pad1 buf 0;
          set_ofp_instruction_actions_pad2 buf 0;
          set_ofp_instruction_actions_pad3 buf 0;
          sizeof_ofp_instruction_actions + (marshal_fields (Cstruct.shift buf sizeof_ofp_instruction_actions) actions Action.marshal)

end

module Instructions = struct

  let sizeof (inss : instruction list) : int =
    sum (map Instruction.sizeof inss)

  let marshal (buf : Cstruct.t) (inss : instruction list) : int =
    marshal_fields buf inss Instruction.marshal

end

module FlowMod = struct

  let sizeof (fm : flowMod) =
    sizeof_ofp_flow_mod + (OfpMatch.sizeof fm.ofp_match) + (Instructions.sizeof fm.instructions)

  let flags_to_int (f : flowModFlags) =
    (if f.send_flow_rem then 1 lsl 0 else 0) lor
      (if f.check_overlap then 1 lsl 1 else 0) lor
        (if f.reset_counts then 1 lsl 2 else 0) lor
          (if f.no_pkt_counts then 1 lsl 3 else 0) lor
            (if f.no_byt_counts then 1 lsl 4 else 0)

  let marshal (buf : Cstruct.t) (fm : flowMod) : int =
    set_ofp_flow_mod_cookie buf fm.cookie.value;
    set_ofp_flow_mod_cookie_mask buf (
      match fm.cookie.mask with
        | None -> 0L
        | Some mask -> mask);
    set_ofp_flow_mod_table_id buf fm.table_id;
    set_ofp_flow_mod_command buf (flowModCommand_to_int fm.command);
    set_ofp_flow_mod_idle_timeout buf
      (match fm.idle_timeout with
        | Permanent -> 0
        | ExpiresAfter value -> value);
    set_ofp_flow_mod_hard_timeout buf
      (match fm.idle_timeout with
        | Permanent -> 0
        | ExpiresAfter value -> value);
    set_ofp_flow_mod_priority buf fm.priority;
    set_ofp_flow_mod_buffer_id buf
      (match fm.buffer_id with
        | None -> 0xffffffffl
        | Some bid -> bid);
    set_ofp_flow_mod_out_port buf
      (match fm.out_port with
        | None -> Int32.of_int 0
        | Some port ->
          (match port with
            | PhysicalPort pid -> pid
            | InPort -> Int32.of_int (Int64.to_int 0xfffffff8L)         (* OFPP_IN_PORT *)
            | Flood -> Int32.of_int (Int64.to_int 0xfffffffbL)          (* OFPP_FLOOD *)
            | AllPorts -> Int32.of_int (Int64.to_int 0xfffffffcL)       (* OFPP_ALL *)
            | Controller _ -> Int32.of_int (Int64.to_int 0xfffffffdL);  (* OFPP_CONTROLLER *)
            | Any -> Int32.of_int (Int64.to_int 0xffffffffL)));         (* OFPP_ANY *)
    set_ofp_flow_mod_out_group buf
      (match fm.out_group with
        | None -> Int32.of_int 0
        | Some gid -> gid);
    set_ofp_flow_mod_flags buf (flags_to_int fm.flags);
    set_ofp_flow_mod_pad0 buf 0;
    set_ofp_flow_mod_pad1 buf 0;
    let size = sizeof_ofp_flow_mod +
        OfpMatch.marshal (Cstruct.shift buf sizeof_ofp_flow_mod) fm.ofp_match in
      size + Instructions.marshal (Cstruct.shift buf size) fm.instructions
end

module Capabilities = struct

  let parse (bits : int32) : capabilities =
    { port_blocked = test_bit 7 bits; 
      queue_stats = test_bit 6 bits; 
      ip_reasm = test_bit 5 bits; 
      group_stats = test_bit 3 bits; 
      port_stats = test_bit 2 bits; 
      table_stats = test_bit 1 bits; 
      flow_stats = test_bit 0 bits;
    }

end

module Features = struct

  let parse (bits : Cstruct.t) : features =
    let datapath_id = get_ofp_switch_features_datapath_id bits in 
    let num_buffers = get_ofp_switch_features_n_buffers bits in
    let num_tables = get_ofp_switch_features_n_tables bits in
    let aux_id = get_ofp_switch_features_auxiliary_id bits in
    let supported_capabilities = Capabilities.parse
      (get_ofp_switch_features_capabilities bits) in
    let bits = Cstruct.shift bits sizeof_ofp_switch_features in
    { datapath_id; 
      num_buffers; 
      num_tables;
      aux_id; 
      supported_capabilities }

end

module PacketIn = struct

 cstruct ofp_packet_in {
   uint32_t buffer_id;     
   uint16_t total_len;     
   uint8_t reason;         
   uint8_t table_id;
   uint64_t cookie
  } as big_endian

  let parse (bits : Cstruct.t) : packetIn =
    (* let oc = open_out "test-msg-1.3-msg3-bits" in *)
    (* let str = Cstruct.to_string bits in *)
    (* fprintf oc "%s" str; *)
    (* close_out oc; *)
    let bufId = match get_ofp_packet_in_buffer_id bits with
      | -1l -> None
      | n -> Some n in
    let total_len = get_ofp_packet_in_total_len bits in
    let reason = match int_to_reasonType (get_ofp_packet_in_reason bits) with
      | Some n -> n
      | None -> 
        raise (Unparsable (sprintf "malformed packet in packet_in")) in
    let table_id = get_ofp_packet_in_table_id bits in
    let cookie = get_ofp_packet_in_cookie bits in
    let ofp_match_bits = Cstruct.shift bits sizeof_ofp_packet_in in
    let ofp_match, pkt_bits = OfpMatch.parse ofp_match_bits in
    let pkt_bits = Cstruct.shift pkt_bits 2 in (* pad bytes *)
    (* printf "len = %d\n" (Cstruct.len pkt_bits); *)
    let pkt = match Cstruct.len pkt_bits with
      | 0 -> None
      | _ -> begin match PacketParser.parse_packet pkt_bits with 
        | Some pkt -> Some pkt 
        | None -> 
          raise (Unparsable (sprintf "malformed packet in packet_in")) end in
    let _ = eprintf "[PacketIn] okay \n%!" in 
    { pi_buffer_id = bufId;
      pi_total_len = total_len;
      pi_reason = reason;
      pi_table_id = table_id;
      pi_cookie = cookie;
      pi_ofp_match = ofp_match;
      pi_pkt = pkt
    }

end

module PacketOut = struct

  cstruct ofp_packet_out {
      uint32_t buffer_id;           (* ID assigned by datapath (OFP_NO_BUFFER
                                       if none). *)
      uint32_t in_port;             (* Packet's input port or OFPP_CONTROLLER. *)
      uint16_t actions_len;         (* Size of action array in bytes. *)
      uint8_t pad0;
      uint8_t pad1;
      uint8_t pad2;
      uint8_t pad3;
      uint8_t pad4;
      uint8_t pad5
      (* struct ofp_action_header actions[0]; *) (* Action list. *)
      (* uint8_t data[0]; *)        (* Packet data.  The length is inferred
                                       from the length field in the header.
                                       (Only meaningful if buffer_id == -1.) *)
  } as big_endian

  let sizeof (po : packetOut) =
    sizeof_ofp_packet_out + sum (map Action.sizeof po.po_actions) +
    (match po.po_pkt with
      | None -> 0
      | Some pkt -> sizeof_packet pkt)

  let marshal (buf : Cstruct.t) (po : packetOut) : int =
    let size = sizeof po in
    set_ofp_packet_out_buffer_id buf (
      match po.po_buffer_id with
        | None -> 0xffffffffl
        | Some buffer_id -> buffer_id);
    set_ofp_packet_out_in_port buf
      (match po.po_in_port with
        | PhysicalPort pid -> pid
        | Controller _ -> 0xfffffffdl   (* OFPP_CONTROLLER *)
        | _ -> failwith "Invalid marshal of packetOut");
    set_ofp_packet_out_actions_len buf (sum (map Action.sizeof po.po_actions));
    set_ofp_packet_out_pad0 buf 0;
    set_ofp_packet_out_pad1 buf 0;
    set_ofp_packet_out_pad2 buf 0;
    set_ofp_packet_out_pad3 buf 0;
    set_ofp_packet_out_pad4 buf 0;
    set_ofp_packet_out_pad5 buf 0;
    let buf = Cstruct.shift buf sizeof_ofp_packet_out in
    let act_size = marshal_fields buf po.po_actions Action.marshal in
    match po.po_pkt with
      | None -> size
      | Some pkt ->
        let pkt_buf = serialize_packet pkt in
        Cstruct.blit pkt_buf 0 buf act_size (Cstruct.len pkt_buf);
        size

end

module Message = struct

  let msg_code_of_message (msg : message) : msg_code = match msg with
    | Hello -> HELLO
    | EchoRequest _ -> ECHO_REQ
    | EchoReply _ -> ECHO_RESP
    | FeaturesRequest -> FEATURES_REQ
    | FeaturesReply _ -> FEATURES_RESP
    | FlowMod _ -> FLOW_MOD
    | GroupMod _ -> GROUP_MOD
    | PacketIn _ -> PACKET_IN
    | PacketOut _ -> PACKET_OUT

  let sizeof (msg : message) : int = match msg with
    | Hello -> sizeof_ofp_header
    | EchoRequest bytes -> sizeof_ofp_header + (String.length bytes)
    | EchoReply bytes -> sizeof_ofp_header + (String.length bytes)
    | FeaturesRequest -> sizeof_ofp_header
    | FeaturesReply _ -> sizeof_ofp_header + sizeof_ofp_switch_features
    | FlowMod fm -> sizeof_ofp_header + FlowMod.sizeof fm
    | GroupMod gm -> sizeof_ofp_header + GroupMod.sizeof gm
    | PacketOut po -> sizeof_ofp_header + PacketOut.sizeof po
    | _ -> failwith "unknowns"

  let marshal (buf : Cstruct.t) (msg : message) : int =
    let buf2 = (Cstruct.shift buf sizeof_ofp_header) in
    set_ofp_header_version buf 0x04;
    set_ofp_header_typ buf (msg_code_to_int (msg_code_of_message msg));
    set_ofp_header_length buf (sizeof msg);
    match msg with
      | Hello ->
        sizeof_ofp_header
      | EchoRequest bytes 
      | EchoReply bytes ->
        Cstruct.blit_from_string bytes 0 buf2 0 (String.length bytes);
        sizeof_ofp_header + String.length bytes
      | FeaturesRequest ->
        sizeof_ofp_header
      | FlowMod fm ->
	sizeof_ofp_header + FlowMod.marshal buf2 fm
      | GroupMod gm ->
        sizeof_ofp_header + GroupMod.marshal buf2 gm
      | PacketOut po ->
        sizeof_ofp_header + PacketOut.marshal buf2 po
      | _ -> failwith "unknowns"

  let serialize (xid : xid) (msg : message) : string = 
    let buf = Cstruct.create (sizeof msg) in
    let _ = set_ofp_header_xid buf xid in
    let _ = marshal buf msg in
    let str = Cstruct.to_string buf in
    str

  let parse (bits : Cstruct.t) =
    (* let oc = open_out "test-msg-1.3-msg3-prebodybits" in *)
    (* let str = Cstruct.to_string bits in *)
    (* fprintf oc "%s" str; *)
    (* close_out oc; *)
    let ver = get_ofp_header_version bits in
    let typ = get_ofp_header_typ bits in
    let len = get_ofp_header_length bits in
    let xid = get_ofp_header_xid bits in
    let body_bits = Cstruct.shift bits sizeof_ofp_header in
    (* let oc2 = open_out "test-msg-1.3-msg3-bodybits" in *)
    (* let str = Cstruct.to_string body_bits in *)
    (* fprintf oc2 "%s" str; *)
    (* close_out oc2; *)
    match int_to_msg_code typ with
        | Some HELLO -> Hello
        | Some ECHO_RESP -> EchoReply (Cstruct.to_string body_bits)
        | Some FEATURES_RESP -> FeaturesReply (Features.parse body_bits)
        | Some PACKET_IN -> PacketIn (PacketIn.parse body_bits)
	| Some ECHO_REQ -> EchoRequest (Cstruct.to_string body_bits)
        | _ -> raise (Unparsable "unrecognized message code")

end


(*

module PseudoPort = struct

  type t = pseudoPort

  let marshal (t : t) : int = match t with
    | PhysicalPort p -> p
    | InPort -> ofp_port_to_int OFPP_IN_PORT
    | Flood -> ofp_port_to_int OFPP_FLOOD
    | AllPorts -> ofp_port_to_int OFPP_ALL
    (* TODO(arjun): what happened to the byte count? *)
    | Controller _ -> ofp_port_to_int OFPP_CONTROLLER

  let marshal_optional (t : t option) : int = match t with
    | None -> ofp_port_to_int OFPP_NONE
    | Some x -> marshal x

end

module PacketIn = struct

  cenum reason {
    NO_MATCH = 0;
    ACTION = 1
  } as uint8_t

 cstruct ofp_packet_in {
   uint32_t buffer_id;     
   uint16_t total_len;     
   uint16_t in_port;       
   uint8_t reason;         
   uint8_t pad
  } as big_endian

  let parse bits =
    let bufId = match get_ofp_packet_in_buffer_id bits with
      | -1l -> None
      | n -> Some n in
  let total_len = get_ofp_packet_in_total_len bits in
  let in_port = get_ofp_packet_in_in_port bits in
  let reason_code = get_ofp_packet_in_reason bits in
  let reason = match int_to_reason reason_code with
    | Some NO_MATCH -> NoMatch
    | Some ACTION -> ExplicitSend
    | None ->
      raise (Unparsable (sprintf "bad reason in packet_in (%d)" reason_code)) in
  let pkt_bits = Cstruct.shift bits sizeof_ofp_packet_in in
  let pkt = match PacketParser.parse_packet pkt_bits with 
    | Some pkt -> pkt 
    | None -> 
      raise (Unparsable (sprintf "malformed packet in packet_in")) in
  let _ = eprintf "[PacketIn] okay \n%!" in 
  { packetInBufferId = bufId;
    packetInTotalLen = total_len;
    packetInPort = in_port;
    packetInReason_ = reason;
    packetInPacket = pkt }

end

module Action = struct

  type t = action

  let type_code (a : t) = match a with
    | Output _ -> OFPAT_OUTPUT
    | SetDlVlan _ -> OFPAT_SET_VLAN_VID
    | SetDlVlanPcp _ -> OFPAT_SET_VLAN_PCP
    | StripVlan -> OFPAT_STRIP_VLAN
    | SetDlSrc _ -> OFPAT_SET_DL_SRC
    | SetDlDst _ -> OFPAT_SET_DL_DST
    | SetNwSrc _ -> OFPAT_SET_NW_SRC
    | SetNwDst _ -> OFPAT_SET_NW_DST
    | SetNwTos _ -> OFPAT_SET_NW_TOS
    | SetTpSrc _ -> OFPAT_SET_TP_SRC
    | SetTpDst _ -> OFPAT_SET_TP_DST

  let sizeof (a : t) = match a with
    | Output _ -> sizeof_ofp_action_output
    | SetDlVlan _ -> sizeof_ofp_action_vlan_vid
    | SetDlVlanPcp _ -> sizeof_ofp_action_vlan_pcp
    | StripVlan -> sizeof_ofp_action_header
    | SetDlSrc _
    | SetDlDst _ -> sizeof_ofp_action_dl_addr
    | SetNwSrc _
    | SetNwDst _ -> sizeof_ofp_action_nw_addr
    | SetNwTos _ -> sizeof_ofp_action_nw_tos
    | SetTpSrc _
    | SetTpDst _ -> sizeof_ofp_action_tp_port

  let marshal a bits = 
    set_ofp_action_header_typ bits (ofp_action_type_to_int (type_code a));
    set_ofp_action_header_len bits (sizeof a);
    begin
      match a with
        | Output pp ->
          set_ofp_action_output_port bits (PseudoPort.marshal pp);
          set_ofp_action_output_max_len bits
            (match pp with
              | Controller w -> w
              | _ -> 0)
    end;
    sizeof a
      
        
end

module Timeout = struct

  type t = timeout

  let marshal (t : t) : int = match t with
    | Permanent -> 0
    | ExpiresAfter n -> n

end

module FlowModCommand = struct
    
  type t = flowModCommand

  let marshal (t : t) : int = match t with
    | AddFlow -> ofp_flow_mod_command_to_int OFPFC_ADD
    | ModFlow -> ofp_flow_mod_command_to_int OFPFC_MODIFY
    | ModStrictFlow -> ofp_flow_mod_command_to_int OFPFC_MODIFY_STRICT
    | DeleteFlow -> ofp_flow_mod_command_to_int OFPFC_DELETE
    | DeleteStrictFlow -> ofp_flow_mod_command_to_int OFPFC_DELETE_STRICT
     
end

module Capabilities = struct

  type t = capabilities

  let parse bits =
    { arp_match_ip = test_bit 7 bits; 
      queue_stats = test_bit 6 bits; 
      ip_reasm = test_bit 5 bits; 
      stp = test_bit 3 bits; 
      port_stats = test_bit 2 bits; 
      table_stats = test_bit 1 bits; 
      flow_stats = test_bit 0 bits;
    }

  let marshal (c : t) : int32 =
    let bits = Int32.zero in 
    let bits = bit bits 7 c.arp_match_ip in
    let bits = bit bits 6 c.queue_stats in
    let bits = bit bits 5 c.ip_reasm in
    let bits = bit bits 3 c.stp in 
    let bits = bit bits 2 c.port_stats in
    let bits = bit bits 1 c.table_stats in
    let bits = bit bits 0 c.flow_stats in
    bits

end

module Actions = struct

  type t = actions

 let parse bits = 
   { output = test_bit 0 bits; 
     set_vlan_id = test_bit 1 bits; 
     set_vlan_pcp = test_bit 2 bits; 
     strip_vlan = test_bit 3 bits;
     set_dl_src = test_bit 4 bits; 
     set_dl_dst = test_bit 5 bits; 
     set_nw_src = test_bit 6 bits; 
     set_nw_dst = test_bit 7 bits;
     set_nw_tos = test_bit 8 bits; 
     set_tp_src = test_bit 9 bits; 
     set_tp_dst = test_bit 10 bits; 
     enqueue = test_bit 11 bits; 
     vendor = test_bit 12 bits; }

  let marshal (a : actions) : int32 =
    let bits = Int32.zero in
    let bits = bit bits 0 a.output in  
    let bits = bit bits 1 a.set_vlan_id in  
    let bits = bit bits 2 a.set_vlan_pcp in  
    let bits = bit bits 3 a.strip_vlan in 
    let bits = bit bits 4 a.set_dl_src in  
    let bits = bit bits 5 a.set_dl_dst in  
    let bits = bit bits 6 a.set_nw_src in  
    let bits = bit bits 7 a.set_nw_dst in 
    let bits = bit bits 8 a.set_nw_tos in  
    let bits = bit bits 9 a.set_tp_src in  
    let bits = bit bits 10 a.set_tp_dst in  
    let bits = bit bits 11 a.enqueue in  
    let bits = bit bits 12 a.vendor in  
    bits

end

module Features = struct

  type t = features

  let parse (buf : Cstruct.buf) : t =
    let switch_id = get_ofp_switch_features_datapath_id buf in 
    let num_buffers = get_ofp_switch_features_n_buffers buf in
    let num_tables = get_ofp_switch_features_n_tables buf in 
    let supported_capabilities = Capabilities.parse
      (get_ofp_switch_features_capabilities buf) in
    let supported_actions = Actions.parse 
      (get_ofp_switch_features_action buf) in
    let buf = Cstruct.shift buf sizeof_ofp_switch_features in
    { switch_id; 
      num_buffers; 
      num_tables; 
      supported_capabilities; 
      supported_actions }
end

(** Internal module, only used to parse the wildcards bitfield *)
module Wildcards = struct

  type t = {
    in_port: bool; 
    dl_vlan: bool;
    dl_src: bool; 
    dl_dst: bool; 
    dl_type: bool; 
    nw_proto: bool; 
    tp_src: bool; 
    tp_dst: bool; 
    nw_src: int; (* XXX *)
    nw_dst: int; (* XXX *)
    dl_vlan_pcp: bool;
    nw_tos: bool;
  }

  let set_nw_mask (f:int32) (off : int) (v : int) : int32 = 
    let value = (0x3f land v) lsl off in
    (Int32.logor f (Int32.of_int value))

  (* TODO(arjun): this is different from mirage *)
  let get_nw_mask (f : int32) (off : int) : int = 
    (Int32.to_int (Int32.shift_right f off)) land 0x3f

  let marshal m = 
    let ret = Int32.zero in 
    let ret = bit ret 0 m.in_port in
    let ret = bit ret 1 m.dl_vlan in 
    let ret = bit ret 2 m.dl_src in
    let ret = bit ret 3 m.dl_dst in
    let ret = bit ret 4 m.dl_type in
    let ret = bit ret 5 m.nw_proto in
    let ret = bit ret 6 m.tp_src in
    let ret = bit ret 7 m.tp_dst in
    let ret = set_nw_mask ret 8 m.nw_src in
    let ret = set_nw_mask ret 14 m.nw_dst in
    let ret = bit ret 20 m.dl_vlan_pcp in 
    let ret = bit ret 21 m.nw_tos in
    ret

  let to_string h = 
    Format.sprintf
      "in_port:%s,dl_vlan:%s,dl_src:%s,dl_dst:%s,dl_type:%s,\
       nw_proto:%s,tp_src:%s,tp_dst:%s,nw_src:%d,nw_dst:%d,\
       dl_vlan_pcp:%s,nw_tos:%s" 
      (string_of_bool h.in_port) 
      (string_of_bool h.dl_vlan) (string_of_bool h.dl_src)
      (string_of_bool h.dl_dst) (string_of_bool h.dl_type)
      (string_of_bool h.nw_proto) (string_of_bool h.tp_src)
      (string_of_bool h.tp_dst) h.nw_src
      h.nw_dst (string_of_bool h.dl_vlan_pcp) 
      (string_of_bool h.nw_tos)
      
  let parse bits = 
    { nw_tos = test_bit 21 bits;
      dl_vlan_pcp = test_bit 20 bits;
      nw_dst = get_nw_mask bits 14; 
      nw_src = get_nw_mask bits 8; 
      tp_dst = test_bit 7 bits; 
      tp_src = test_bit 6 bits; 
      nw_proto = test_bit 5 bits; 
      dl_type = test_bit 4 bits; 
      dl_dst = test_bit 3 bits; 
      dl_src = test_bit 2 bits; 
      dl_vlan = test_bit 1 bits; 
      in_port = test_bit 0 bits;
    }
end

module Match = struct

  type t = of_match

  let is_none x = match x with
    | None -> true
    | Some _ -> false

  let wildcards_of_match (m : t) : Wildcards.t =
    { Wildcards.in_port = is_none m.matchInPort;
      Wildcards.dl_vlan = is_none m.matchDlVlan;
      Wildcards.dl_src = is_none m.matchDlSrc;
      Wildcards.dl_dst = is_none m.matchDlDst;
      Wildcards.dl_type = is_none m.matchDlTyp;
      Wildcards.nw_proto = is_none m.matchNwProto;
      Wildcards.tp_src = is_none m.matchTpSrc;
      Wildcards.tp_dst = is_none m.matchTpDst;
      (* TODO(arjun): support IP prefixes *)
      Wildcards.nw_src = if is_none m.matchNwSrc then 32 else 0x0;
      Wildcards.nw_dst = if is_none m.matchNwDst then 32 else 0x0;
      Wildcards.dl_vlan_pcp = is_none m.matchDlVlanPcp;
      Wildcards.nw_tos = is_none m.matchNwTos;
  }

  let if_some16 x = match x with
    | Some n -> n
    | None -> 0

  let if_some8 x = match x with
    | Some n -> n
    | None -> 0

  let if_some32 x = match x with
    | Some n -> n
    | None -> 0l

  let if_word48 x = match x with
    | Some n -> n
    | None -> Int64.zero

 let marshal m bits = 
   set_ofp_match_wildcards bits (Wildcards.marshal (wildcards_of_match m));
   set_ofp_match_in_port bits (if_some16 m.matchInPort); 
   set_ofp_match_dl_src (bytes_of_mac (if_word48 m.matchDlSrc)) 0 bits;
   set_ofp_match_dl_dst (bytes_of_mac (if_word48 m.matchDlDst)) 0 bits;
   set_ofp_match_dl_vlan bits (if_some16 m.matchDlVlan);
   set_ofp_match_dl_vlan_pcp bits (if_some8 m.matchDlVlanPcp);
   set_ofp_match_dl_type bits (if_some16 m.matchDlTyp);
   set_ofp_match_nw_tos bits (if_some8 m.matchNwTos);
   set_ofp_match_nw_proto bits (if_some8 m.matchNwProto);
   set_ofp_match_nw_src bits (if_some32 m.matchNwSrc);
   set_ofp_match_nw_dst bits (if_some32 m.matchNwDst);
   set_ofp_match_tp_src bits (if_some16 m.matchTpSrc);
   set_ofp_match_tp_dst bits (if_some16 m.matchTpDst); 
   sizeof_ofp_match 

  let parse bits = 
    let w = Wildcards.parse (get_ofp_match_wildcards bits) in
    { matchDlSrc = 
        if w.Wildcards.dl_src then 
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_src bits)));
      matchDlDst = 
        if w.Wildcards.dl_dst then 
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_dst bits)));
      matchDlVlan =
        if w.Wildcards.dl_vlan then
          None
        else
          Some (get_ofp_match_dl_vlan bits);
      matchDlVlanPcp = 
        if w.Wildcards.dl_vlan_pcp then
          None
        else
          Some (get_ofp_match_dl_vlan_pcp bits);
      matchDlTyp =
        if w.Wildcards.dl_type then
          None
        else
          Some (get_ofp_match_dl_type bits);
      matchNwSrc = 
        if w.Wildcards.nw_src = 0x3f then (* TODO(arjun): prefixes *)
          None
        else
          Some (get_ofp_match_nw_src bits);
      matchNwDst = 
        if w.Wildcards.nw_dst = 0x3f then (* TODO(arjun): prefixes *)
          None
        else
          Some (get_ofp_match_nw_dst bits);
      matchNwProto =
        if w.Wildcards.nw_proto then
          None
        else
          Some (get_ofp_match_nw_proto bits);
      matchNwTos = 
        if w.Wildcards.nw_tos then 
          None
        else 
          Some (get_ofp_match_nw_tos bits);
      matchTpSrc =
        if w.Wildcards.tp_src then
          None
        else
          Some (get_ofp_match_tp_src bits);
      matchTpDst =
        if w.Wildcards.tp_dst then
          None
        else
          Some (get_ofp_match_tp_dst bits);
      matchInPort =
        if w.Wildcards.in_port then
          None
        else
          Some (get_ofp_match_in_port bits);
    }

end

module TimeoutSer = struct

  let to_int (x : timeout) = match x with
    | Permanent -> 0
    | ExpiresAfter w -> w

end  

module Header = struct

  let ver : int = 0x01

  type t = {
    ver: int;
    typ: msg_code;
    len: int;
    xid: int32
  }
      
  (** [parse buf] assumes that [buf] has size [sizeof_ofp_header] *)
  let parse buf = 
    { ver = get_ofp_header_version buf;
      typ = begin match int_to_msg_code (get_ofp_header_typ buf) with
        | Some typ -> typ
        | None -> raise (Unparsable "unrecognized message code")
      end;
      len = get_ofp_header_length buf;
      xid = get_ofp_header_xid buf
    }

end

module Message = struct

  type t = message
  
  let parse (hdr : Header.t) (buf : Cstruct.buf) : (xid * t) option =
    let msg = match hdr.Header.typ with
      | HELLO -> Some (Hello buf)
      | ECHO_REQ -> Some (EchoRequest buf)
      | ECHO_RESP -> Some (EchoReply buf)
      | FEATURES_REQ -> Some (FeaturesRequest)
      | FEATURES_RESP -> Some (FeaturesReply (Features.parse buf))
      | PACKET_IN -> Some (PacketInMsg (PacketIn.parse buf))
      | code -> None
    in
    match msg with
      | Some v -> Some (hdr.Header.xid, v)
      | None -> None

  let msg_code_of_message (msg : t) : msg_code = match msg with
    | Hello _ -> HELLO
    | EchoRequest _ -> ECHO_REQ
    | EchoReply _ -> ECHO_RESP
    | FeaturesRequest -> FEATURES_REQ
    | FeaturesReply _ -> FEATURES_RESP
    | FlowModMsg _ -> FLOW_MOD

  open Bigarray

  (** Size of the message body, without the header *)
  let sizeof_body (msg : t) : int = match msg with
    | Hello buf -> Array1.dim buf
    | EchoRequest buf -> Array1.dim buf
    | EchoReply buf -> Array1.dim buf
    | FeaturesRequest -> 0
    | FeaturesReply _ -> sizeof_ofp_switch_features
    | FlowModMsg msg ->
      sizeof_ofp_match + sizeof_ofp_flow_mod + 
        sum (List.map Action.sizeof msg.mfActions)
    | _ -> failwith "unknowns"

  let blit_message (msg : t) (out : Cstruct.buf) = match msg with
    | Hello buf
    | EchoRequest buf
    | EchoReply buf ->
      Cstruct.blit_buffer buf 0 out 0 (Cstruct.len buf)
    | FeaturesRequest -> ()
    | FlowModMsg flow_mod -> FlowMod.marshal flow_mod out

  let marshal (xid : xid) (msg : t) : string = 
    let sizeof_buf = sizeof_ofp_header + sizeof_body msg in
    let buf = Array1.create char c_layout sizeof_buf in
    set_ofp_header_version buf 0x1;
    set_ofp_header_typ buf (msg_code_to_int (msg_code_of_message msg));
    set_ofp_header_length buf sizeof_buf;
    set_ofp_header_xid buf xid;
    blit_message msg (Cstruct.shift buf sizeof_ofp_header);
    let str = Cstruct.to_string buf in
    str
end
*)
