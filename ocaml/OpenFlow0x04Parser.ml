(** OpenFlow 1.3 (protocol version 0x04) *)

open Printf
open OpenFlow0x04Types
open Util

exception Unparsable of string

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
  uint16_t length;          
} as big_endian

(* OKAY *)
cenum ofp_match_type {
  OFPMT_STANDARD = 0;       (* Deprecated. *)
  OFPMT_OXM      = 1        (* OpenFlow Extensible Match *)
} as uint16_t

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
}

cstruct ofp_switch_features {
  uint64_t datapath_id; 
  uint32_t n_buffers; 
  uint8_t n_tables; 
  uint8_t pad[3]; 
  uint32_t capabilities; 
  uint32_t action
} as big_endian 

cenum ofp_flow_mod_command {
  OFPFC_ADD           = 0; (* New flow. *)
  OFPFC_MODIFY        = 1; (* Modify all matching flows. *)
  OFPFC_MODIFY_STRICT = 2; (* Modify entry strictly matching wildcards and
                              priority. *)
  OFPFC_DELETE        = 3; (* Delete all matching flows. *)
  OFPFC_DELETE_STRICT = 4  (* Delete entry strictly matching wildcards and
                              priority. *)
} as uint16_t

(* OKAY *)
cenum ofp_port_no {
  (* Maximum number of physical and logical switch ports. *)
  OFPP_MAX        = 0xffffff00;
  (* Reserved OpenFlow Port (fake output "ports"). *)
  OFPP_IN_PORT    = 0xfffffff8;  (* Send the packet out the input port.  This
                                    reserved port must be explicitly used
                                    in order to send back out of the input
                                    port. *)
  OFPP_TABLE      = 0xfffffff9;  (* Submit the packet to the first flow table
                                    NB: This destination port can only be
                                    used in packet-out messages. *)
  OFPP_NORMAL     = 0xfffffffa;  (* Process with normal L2/L3 switching. *)
  OFPP_FLOOD      = 0xfffffffb;  (* All physical ports in VLAN, except input
                                    port and those blocked or link down. *)
  OFPP_ALL        = 0xfffffffc;  (* All physical ports except input port. *)
  OFPP_CONTROLLER = 0xfffffffd;  (* Send to controller. *)
  OFPP_LOCAL      = 0xfffffffe;  (* Local openflow "port". *)
  OFPP_ANY        = 0xffffffff   (* Wildcard port used only for flow mod
                                    (delete) and flow stats requests. Selects
                                    all flows regardless of output port
                                    (including flows with no output port). *)
} as uint32_t

(* OKAY *)
cstruct ofp_port {
    uint32_t port_no;
    uint8_t pad[4];
    uint8_t hw_addr[OFP_ETH_ALEN];
    uint8_t pad2[2];                  (* Align to 64 bits. *)
    char name[OFP_MAX_PORT_NAME_LEN]; (* Null-terminated *)

    uint32_t config;        (* Bitmap of OFPPC_* flags. *)
    uint32_t state;         (* Bitmap of OFPPS_* flags. *)

    (* Bitmaps of OFPPF_* that describe features.  All bits zeroed if
     * unsupported or unavailable. *)
    uint32_t curr;          (* Current features. *)
    uint32_t advertised;    (* Features being advertised by the port. *)
    uint32_t supported;     (* Features supported by the port. *)
    uint32_t peer;          (* Features advertised by peer. *)

    uint32_t curr_speed;    (* Current port bitrate in kbps. *)
    uint32_t max_speed      (* Max port bitrate in kbps *)
} as big_endian

(* MISSING: ofp_port_config *)
(* MISSING: ofp_port_state *)
(* MISSING: ofp_port_features *)

(* MISSING: ofp_ queues *)

cstruct ofp_flow_mod {
  struct ofp_header header;
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
  uint8_t pad[2];
  struct ofp_match match_       (* Fields to match. Variable size. *)
  //struct ofp_instruction instructions[0]; (* Instruction set *)
} as big_endian

cstruct ofp_action_header {
   uint16_t typ;
   uint16_t len;
   uint8_t pad[4]
} as big_endian

cstruct ofp_action_output {
  uint16_t typ;
  uint16_t len;
  uint16_t port;
  uint16_t max_len
} as big_endian 

 cstruct ofp_action_vlan_vid {
   uint16_t typ;          
   uint16_t len;           
   uint16_t vlan_vid;      
   uint8_t pad[2]
 } as big_endian

 cstruct ofp_action_vlan_pcp {
   uint16_t typ;
   uint16_t len;           
   uint8_t vlan_pcp;       
   uint8_t pad[3]
 } as big_endian

 cstruct ofp_action_dl_addr {
   uint16_t typ; 
   uint16_t len;          
   uint8_t dl_addr[6];
   uint8_t pad[6]
 } as big_endian 

 cstruct ofp_action_nw_addr {
   uint16_t typ;
   uint16_t len; 
   uint32_t nw_addr
 } as big_endian

 cstruct ofp_action_tp_port {
   uint16_t typ;         
   uint16_t len;          
   uint16_t tp_port;      
   uint8_t pad[2]
 } as big_endian

 cstruct ofp_action_nw_tos {
   uint16_t typ;
   uint16_t len; 
   uint8_t nw_tos; 
   uint8_t pad[3]
 } as big_endian

 cstruct ofp_action_enqueue {
   uint16_t typ;
   uint16_t len;
   uint16_t port;
   uint8_t pad[6]; 
   uint32_t queue_id
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

(* OKAY *)
cstruct ofp_action_header {
    uint16_t typ;                   (* One of OFPAT_*. *)
    uint16_t len;                   (* Length of action, including this
                                       header.  This is the length of action,
                                       including any padding to make it
                                       64-bit aligned. *)
    uint8_t pad[4]
} as big_endian

cstruct ofp_action_output {
    uint16_t typ;                   (* OFPAT_OUTPUT. *)
    uint16_t len;                   (* Length is 16. *)
    uint32_t port;                  (* Output port. *)
    uint16_t max_len;               (* Max length to send to controller. *)
    uint8_t pad[6]                  (* Pad to 64 bits. *)
} as big_endian

cstruct ofp_action_mpls_ttl {
    uint16_t typ;                   (* OFPAT_SET_MPLS_TTL. *)
    uint16_t len;                   (* Length is 8. *)
    uint8_t mpls_ttl;               (* MPLS TTL *)
    uint8_t pad[3]
} as big_endian

cstruct ofp_action_push {
    uint16_t typ;                   (* OFPAT_PUSH_VLAN/MPLS/PBB. *)
    uint16_t len;                   (* Length is 8. *)
    uint16_t ethertype;             (* Ethertype *)
    uint8_t pad[2]
} as big_endian

cstruct ofp_action_pop_mpls {
    uint16_t typ;                   (* OFPAT_POP_MPLS. *)
    uint16_t len;                   (* Length is 8. *)
    uint16_t ethertype;             (* Ethertype *)
    uint8_t pad[2]
} as big_endian

cstruct ofp_action_group {
    uint16_t typ;                   (* OFPAT_GROUP. *)
    uint16_t len;                   (* Length is 8. *)
    uint32_t group_id               (* Group identifier. *)
} as big_endian

cstruct ofp_action_nw_ttl {
    uint16_t typ;                   (* OFPAT_SET_NW_TTL. *)
    uint16_t len;                   (* Length is 8. *)
    uint8_t nw_ttl;                 (* IP TTL *)
    uint8_t pad[3]
} as big_endian

cstruct ofp_action_set_field {
    uint16_t typ;                   (* OFPAT_SET_FIELD. *)
    uint16_t len;                   (* Length is padded to 64 bits. *)
    (* Followed by:
     *   - Exactly oxm_len bytes containing a single OXM TLV, then
     *   - Exactly ((oxm_len + 4) + 7)/8*8 - (oxm_len + 4) (between 0 and 7)
     *     bytes of all-zero bytes
     *)
    uint8_t field[4]               (* OXM TLV - Make compiler happy *)
} as big_endian

cstruct ofp_action_experimenter_header {
    uint16_t typ;                   (* OFPAT_EXPERIMENTER. *)
    uint16_t len;                   (* Length is a multiple of 8. *)
    uint32_t experimenter           (* Experimenter ID which takes the same
                                       form as in struct
                                       ofp_experimenter_header. *)
} as big_endian

cenum ofp_instruction_type {
    OFPIT_GOTO_TABLE = 1,       (* Setup the next table in the lookup
                                   pipeline *)
    OFPIT_WRITE_METADATA = 2,   (* Setup the metadata field for use later in
                                   pipeline *)
    OFPIT_WRITE_ACTIONS = 3,    (* Write the action(s) onto the datapath action
                                   set *)
    OFPIT_APPLY_ACTIONS = 4,    (* Applies the action(s) immediately *)
    OFPIT_CLEAR_ACTIONS = 5,    (* Clears all actions from the datapath
                                   action set *)
    OFPIT_METER = 6,            (* Apply meter (rate limiter) *)
    OFPIT_EXPERIMENTER = 0xFFFF (* Experimenter instruction *)
} as 

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
    uint8_t pad[3]                (* Pad to 64 bits. *)
} as big_endian

(* Instruction structure for OFPIT_WRITE_METADATA *)
cstruct ofp_instruction_write_metadata {
    uint16_t typ;                 (* OFPIT_WRITE_METADATA *)
    uint16_t len;                 (* Length of this struct in bytes. *)
    uint8_t pad[4];               (* Align to 64-bits *)
    uint64_t metadata;            (* Metadata value to write *)
    uint64_t metadata_mask        (* Metadata write bitmask *)
} as big_endian

(* Instruction structure for OFPIT_WRITE/APPLY/CLEAR_ACTIONS *)
cstruct ofp_instruction_actions {
    uint16_t typ;               (* One of OFPIT_*_ACTIONS *)
    uint16_t len;               (* Length of this struct in bytes. *)
    uint8_t pad[4];             (* Align to 64-bits *)
    struct ofp_action_header actions[0]   (* Actions associated with
                                             OFPIT_WRITE_ACTIONS and
                                             OFPIT_APPLY_ACTIONS *)
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

cstruct ofp_oxm {
  uint16_t oxm_class;
  uint8_t oxm_field_and_hashmask;
  uint8_t oxm_length
} as big_endian

module Oxm = struct

  open Bigarray

(*   let set_ofp_oxm (buf : int32) (c : int) (f : int) (hm : int) (l : int) : int32 = 
    let value = (0xffff land c) lsl 16 in
    let value = value lor ((0x3f land f) lsl 9) in
    let value = value lor ((0x1 land hm) lsl 8) in
    let value = value lor (0xff land l) in
    (Int32.logor buf (Int32.of_int value))
 *)

  let set_ofp_oxm buf (c : int) (f : int) (hm : int) (l : int) : int32 = 
    let value = (0x3f land f) lsl 1 in
    let value = value lor (0x1 land hm) in
    set_ofp_oxm_oxm_class buf c;
    set_ofp_oxm_oxm_field_and_hashmask buf value;
    set_ofp_oxm_oxm_length buf l;

end

module Oxm_Of_InPort = struct

  let marshal buf =
    let oxm_length = 4 in
    Oxm.set_ofp_oxm buf OFPXMC_OPENFLOW_BASIC OFPXMT_OFB_IN_PORT 0 oxm_length;
    (* HERE we need to write the port value into buf *)
    ()
end

module Oxm_Of_EthDst = struct

  let marshal buf =
    let oxm_length = 6 in
    Oxm.set_ofp_oxm buf OFPXMC_OPENFLOW_BASIC OFPXMT_OFB_IN_PORT 0 oxm_length;
    ()
end

#define OXM_OF_ETH_DST    OXM_HEADER  (0x8000, OFPXMT_OFB_ETH_DST, 6)
#define OXM_OF_ETH_DST_W  OXM_HEADER_W(0x8000, OFPXMT_OFB_ETH_DST, 6)
#define OXM_OF_ETH_SRC    OXM_HEADER  (0x8000, OFPXMT_OFB_ETH_SRC, 6)
#define OXM_OF_ETH_SRC_W  OXM_HEADER_W(0x8000, OFPXMT_OFB_ETH_SRC, 6)




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

module FlowMod = struct

  type t = flowMod

  let flags_to_int (check_overlap : bool) (notify_when_removed : bool) =
    (if check_overlap then 1 lsl 1 else 0) lor
      (if notify_when_removed then 1 lsl 0 else 0)

  let marshal m bits = 
    let bits = Cstruct.shift bits (Match.marshal m.mfMatch bits) in
    set_ofp_flow_mod_cookie bits (m.mfCookie);
    set_ofp_flow_mod_command bits (FlowModCommand.marshal m.mfModCmd);
    set_ofp_flow_mod_idle_timeout bits (TimeoutSer.to_int m.mfIdleTimeOut);
    set_ofp_flow_mod_hard_timeout bits (TimeoutSer.to_int m.mfHardTimeOut);
    set_ofp_flow_mod_priority bits (m.mfPriority);
    set_ofp_flow_mod_buffer_id bits
      (match m.mfApplyToPacket with
        | None -> -1l
        | Some bufId -> bufId);
    set_ofp_flow_mod_out_port bits (PseudoPort.marshal_optional m.mfOutPort);
    set_ofp_flow_mod_flags bits
      (flags_to_int m.mfCheckOverlap m.mfNotifyWhenRemoved);
    let bits = Cstruct.shift bits sizeof_ofp_flow_mod in
    let _ = List.fold_left
      (fun bits act -> 
        Cstruct.shift bits (Action.marshal act bits))
      bits
      m.mfActions
    in
    ()

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
