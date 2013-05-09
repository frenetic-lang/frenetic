(** OpenFlow 1.0 (protocol version 0x01) *)

open Printf
open OpenFlow0x01Types
open Misc

exception Unparsable of string

let sum (lst : int list) = List.fold_left (fun x y -> x + y) 0 lst

cstruct ofp_header {
  uint8_t version;    
  uint8_t typ;   
  uint16_t length;    
  uint32_t xid
} as big_endian

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
  PORT_MOD;
  STATS_REQ;
  STATS_RESP;
  BARRIER_REQ;
  BARRIER_RESP;
  QUEUE_GET_CONFIG_REQ;
  QUEUE_GET_CONFIG_RESP
} as uint8_t

cstruct ofp_match {
  uint32_t wildcards;        
  uint16_t in_port;          
  uint8_t dl_src[6];
  uint8_t dl_dst[6];
  uint16_t dl_vlan;          
  uint8_t dl_vlan_pcp;       
  uint8_t pad1[1];           
  uint16_t dl_type;          
  uint8_t nw_tos;            
  uint8_t nw_proto;          
  uint8_t pad2[2];           
  uint32_t nw_src;           
  uint32_t nw_dst;           
  uint16_t tp_src;           
  uint16_t tp_dst
} as big_endian

cstruct ofp_switch_features {
  uint64_t datapath_id; 
  uint32_t n_buffers; 
  uint8_t n_tables; 
  uint8_t pad[3]; 
  uint32_t capabilities; 
  uint32_t action
} as big_endian 

cenum ofp_flow_mod_command {
  OFPFC_ADD;
  OFPFC_MODIFY;
  OFPFC_MODIFY_STRICT;
  OFPFC_DELETE;
  OFPFC_DELETE_STRICT
} as uint16_t

cenum ofp_port {
  (* Maximum number of physical switch ports. *)
  OFPP_MAX = 0xff00;

  (*Fake output "ports". *)
  OFPP_IN_PORT = 0xfff8;  (* Send the packet out the input port.  This
                             virtual port must be explicitly used
                             in order to send back out of the input
                             port. *)

  OFPP_TABLE = 0xfff9; (* Perform actions in flow table.
                          NB: This can only be the destination
                          port for packet-out messages. *)
  OFPP_NORMAL = 0xfffa; (* Process with normal L2/L3 switching. *)
  OFPP_FLOOD = 0xfffb; (* All physical porbts except input port and
                          those disabled by STP. *)
  OFPP_ALL = 0xfffc; (* All physical ports except input port. *)
  OFPP_CONTROLLER = 0xfffd; (* Send to controller. *)
  OFPP_LOCAL = 0xfffe; (* Local openflow "port". *)
  OFPP_NONE = 0xffff  (* Not associated with a physical port. *)
} as uint16_t

cstruct ofp_flow_mod {
  uint64_t cookie;         
  uint16_t command;        
  uint16_t idle_timeout;   
  uint16_t hard_timeout;   
  uint16_t priority;       
  uint32_t buffer_id;      
  uint16_t out_port;       
  uint16_t flags
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

cenum ofp_action_type {
  OFPAT_OUTPUT;
  OFPAT_SET_VLAN_VID;
  OFPAT_SET_VLAN_PCP;
  OFPAT_STRIP_VLAN;
  OFPAT_SET_DL_SRC;
  OFPAT_SET_DL_DST;
  OFPAT_SET_NW_SRC;
  OFPAT_SET_NW_DST;
  OFPAT_SET_NW_TOS;
  OFPAT_SET_TP_SRC;
  OFPAT_SET_TP_DST;
  OFPAT_ENQUEUE
} as uint16_t

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
  let pkt = match Packet.Parser.parse_packet pkt_bits with 
    | Some pkt -> pkt 
    | None -> 
      raise (Unparsable (sprintf "malformed packet in packet_in")) in
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
	| _ -> 
	  failwith "unimplemented" 
    end;
    sizeof a

  let is_to_controller (act : action) : bool = match act with
    | Output (Controller _) -> true
    | _ -> false

  let move_controller_last (lst : action list) : action list = 
    let (to_ctrl, not_to_ctrl) = List.partition is_to_controller lst in
    not_to_ctrl @ to_ctrl
        
end

module PacketOut = struct

  type t = packetOut

  cstruct ofp_packet_out {
    uint32_t buffer_id;
    uint16_t in_port;
    uint16_t actions_len
  } as big_endian

  let sizeof (pktOut : t) : int = 
    sizeof_ofp_packet_out + 
      (sum (List.map Action.sizeof pktOut.pktOutActions)) +
      (match pktOut.pktOutBufOrBytes with
        | Datatypes.Coq_inl _ -> 0
        | Datatypes.Coq_inr bytes -> Cstruct.len bytes)

  let marshal (pktOut : t) (buf : Cstruct.t) : int =
    set_ofp_packet_out_buffer_id buf
      (match pktOut.pktOutBufOrBytes with
        | Datatypes.Coq_inl n -> n
        | _ -> -1l);
    set_ofp_packet_out_in_port buf
      (match pktOut.pktOutPortId with
        | None -> ofp_port_to_int OFPP_NONE
        | Some n -> n);
    set_ofp_packet_out_actions_len buf
      (sum (List.map Action.sizeof pktOut.pktOutActions));
    let _ = List.fold_left
      (fun buf act -> Cstruct.shift buf (Action.marshal act buf))
      (Cstruct.shift buf sizeof_ofp_packet_out)
      (Action.move_controller_last pktOut.pktOutActions) in
    begin match pktOut.pktOutBufOrBytes with
    | Datatypes.Coq_inl n -> ()
    | Datatypes.Coq_inr _ -> ()
    end;
    sizeof pktOut
end
 


module Timeout = struct

  type t = timeout

  let marshal (t : t) : int = match t with
    | Permanent -> 0
    | ExpiresAfter n -> n

end

module FlowModCommand = struct
    
  type t = flowModCommand

  let n = ref 0L

  let marshal (t : t) : int = match t with
    | AddFlow -> n := Int64.succ !n; Misc.Log.printf "created %Ld flow table entries.\n%!" !n;  ofp_flow_mod_command_to_int OFPFC_ADD
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

  let parse (buf : Cstruct.t) : t =
    let switch_id = get_ofp_switch_features_datapath_id buf in 
    let num_buffers = get_ofp_switch_features_n_buffers buf in
    let num_tables = get_ofp_switch_features_n_tables buf in 
    let supported_capabilities = Capabilities.parse
      (get_ofp_switch_features_capabilities buf) in
    let supported_actions = Actions.parse 
      (get_ofp_switch_features_action buf) in
    let _ = Cstruct.shift buf sizeof_ofp_switch_features in
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

module FlowMod = struct

  type t = flowMod

  let flags_to_int (check_overlap : bool) (notify_when_removed : bool) =
    (if check_overlap then 1 lsl 1 else 0) lor
      (if notify_when_removed then 1 lsl 0 else 0)

  let marshal m bits = 
    try 
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
      (Action.move_controller_last m.mfActions)
    in
    ()
    with exn -> 
      begin
	      Misc.Log.printf "@@@ GOT IT @@@\n%s\n%!" (Printexc.get_backtrace ());
	raise exn
      end
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
  
  let parse (hdr : Header.t) (buf : Cstruct.t) : (xid * t) option =
    let msg = match hdr.Header.typ with
      | HELLO -> Some (Hello buf)
      | ECHO_REQ -> Some (EchoRequest buf)
      | ECHO_RESP -> Some (EchoReply buf)
      | FEATURES_REQ -> Some (FeaturesRequest)
      | FEATURES_RESP -> Some (FeaturesReply (Features.parse buf))
      | PACKET_IN -> Some (PacketInMsg (PacketIn.parse buf))
      | BARRIER_REQ -> Some BarrierRequest
      | BARRIER_RESP -> Some BarrierReply
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
    | PacketOutMsg _ -> PACKET_OUT
    | PacketInMsg _ -> PACKET_IN
    | BarrierRequest -> BARRIER_REQ
    | BarrierReply -> BARRIER_RESP

  let to_string (msg : t) : string = match msg with 
    | Hello _ -> "Hello"
    | EchoRequest _ -> "EchoRequest"
    | EchoReply _ -> "EchoReply"
    | FeaturesRequest -> "FeaturesRequest"
    | FeaturesReply _ -> "FeaturesReply"
    | FlowModMsg _ -> "FlowMod"
    | PacketOutMsg _ -> "PacketOut"
    | PacketInMsg _ -> "PacketIn"
    | BarrierRequest -> "BarrierRequest"
    | BarrierReply -> "BarrierReply"

  open Bigarray

  (** Size of the message body, without the header *)
  let sizeof_body (msg : t) : int = match msg with
    | Hello buf -> Cstruct.len buf
    | EchoRequest buf -> Cstruct.len buf
    | EchoReply buf -> Cstruct.len buf
    | FeaturesRequest -> 0
    | FeaturesReply _ -> sizeof_ofp_switch_features
    | FlowModMsg msg ->
      sizeof_ofp_match + sizeof_ofp_flow_mod + 
        sum (List.map Action.sizeof msg.mfActions)
    | PacketOutMsg msg -> PacketOut.sizeof msg
    | BarrierRequest -> 0
    | BarrierReply -> 0

  let blit_message (msg : t) (out : Cstruct.t) = match msg with
    | Hello buf
    | EchoRequest buf
    | EchoReply buf ->
      Cstruct.blit buf 0 out 0 (Cstruct.len buf)
    | FeaturesRequest -> ()
    | FlowModMsg flow_mod -> FlowMod.marshal flow_mod out
    | PacketOutMsg msg -> 
      let _ = PacketOut.marshal msg out in
      ()
    | PacketInMsg _ -> () (* TODO(arjun): wtf? *)
    | FeaturesReply _ -> () (* TODO(arjun): wtf? *)
    | BarrierRequest -> ()
    | BarrierReply -> ()

  let marshal (xid : xid) (msg : t) : string = 
    let sizeof_buf = sizeof_ofp_header + sizeof_body msg in
    let buf = Cstruct.create sizeof_buf in
    set_ofp_header_version buf 0x1;
    set_ofp_header_typ buf (msg_code_to_int (msg_code_of_message msg));
    set_ofp_header_length buf sizeof_buf;
    set_ofp_header_xid buf xid;
    blit_message msg (Cstruct.shift buf sizeof_ofp_header);
    let str = Cstruct.to_string buf in
    str
end
