(** OpenFlow 1.0 *)

open Printf
open Word
open MessagesDef

exception Unparsable of string

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

module Capabilities = struct

  type t = capabilities

  let parse bits =
    { arp_match_ip = Word32.test_bit 7 bits; 
      queue_stats = Word32.test_bit 6 bits; 
      ip_reasm = Word32.test_bit 5 bits; 
      stp = Word32.test_bit 3 bits; 
      port_stats = Word32.test_bit 2 bits; 
      table_stats = Word32.test_bit 1 bits; 
      flow_stats = Word32.test_bit 0 bits;
    }

  let marshal (c : t) : Word32.t =
    let bits = Word32.from_int32 0l in
    let bits = Word32.bit bits 7 c.arp_match_ip in
    let bits = Word32.bit bits 6 c.queue_stats in
    let bits = Word32.bit bits 5 c.ip_reasm in
    let bits = Word32.bit bits 3 c.stp in 
    let bits = Word32.bit bits 2 c.port_stats in
    let bits = Word32.bit bits 1 c.table_stats in
    let bits = Word32.bit bits 0 c.flow_stats in
    bits

end

module Actions = struct

  type t = actions

 let parse bits = 
   { output = Word32.test_bit 0 bits; 
     set_vlan_id = Word32.test_bit 1 bits; 
     set_vlan_pcp = Word32.test_bit 2 bits; 
     strip_vlan = Word32.test_bit 3 bits;
     set_dl_src = Word32.test_bit 4 bits; 
     set_dl_dst = Word32.test_bit 5 bits; 
     set_nw_src = Word32.test_bit 6 bits; 
     set_nw_dst = Word32.test_bit 7 bits;
     set_nw_tos = Word32.test_bit 8 bits; 
     set_tp_src = Word32.test_bit 9 bits; 
     set_tp_dst = Word32.test_bit 10 bits; 
     enqueue = Word32.test_bit 11 bits; 
     vendor = Word32.test_bit 12 bits; }

  let marshal (a : actions) : Word32.t =
    let bits = Word32.from_int32 0l in
    let bits = Word32.bit bits 0 a.output in  
    let bits = Word32.bit bits 1 a.set_vlan_id in  
    let bits = Word32.bit bits 2 a.set_vlan_pcp in  
    let bits = Word32.bit bits 3 a.strip_vlan in 
    let bits = Word32.bit bits 4 a.set_dl_src in  
    let bits = Word32.bit bits 5 a.set_dl_dst in  
    let bits = Word32.bit bits 6 a.set_nw_src in  
    let bits = Word32.bit bits 7 a.set_nw_dst in 
    let bits = Word32.bit bits 8 a.set_nw_tos in  
    let bits = Word32.bit bits 9 a.set_tp_src in  
    let bits = Word32.bit bits 10 a.set_tp_dst in  
    let bits = Word32.bit bits 11 a.enqueue in  
    let bits = Word32.bit bits 12 a.vendor in  
    bits

end

module Features = struct

  type t = features

  let parse (buf : Cstruct.buf) : t =
    let switch_id = Word64.from_int64 
      (get_ofp_switch_features_datapath_id buf) in 
    let num_buffers = Word32.from_int32
      (get_ofp_switch_features_n_buffers buf) in
    let num_tables = Word8.from_int (get_ofp_switch_features_n_tables buf) in 
    let supported_capabilities = Capabilities.parse
      (Word32.from_int32 (get_ofp_switch_features_capabilities buf)) in
    let supported_actions = Actions.parse 
      (Word32.from_int32 (get_ofp_switch_features_action buf)) in
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

  let set_nw_mask (f : Word32.t) (off : int) (v : int) : Word32.t = 
    (* 0x3f = b111111 (six bit field) *)
    let v = Int32.of_int (0x3f land v) in (* select six LSB *)
    Word32.from_int32 (Int32.logor (Word32.to_int32 f) 
                         (Int32.shift_left v off))

  (* TODO(arjun): this is different from mirage *)
  let get_nw_mask (f : Word32.t) (off : int) : int = 
    let f = Word32.to_int32 f in
    (Int32.to_int (Int32.shift_right f off)) land 0x3f

  let marshal m = 
    let ret = Word32.from_int32 0l in 
    let ret = Word32.bit ret 0 m.in_port in
    let ret = Word32.bit ret 1 m.dl_vlan in 
    let ret = Word32.bit ret 2 m.dl_src in
    let ret = Word32.bit ret 3 m.dl_dst in
    let ret = Word32.bit ret 4 m.dl_type in
    let ret = Word32.bit ret 5 m.nw_proto in
    let ret = Word32.bit ret 6 m.tp_src in
    let ret = Word32.bit ret 7 m.tp_dst in
    let ret = set_nw_mask ret 8 m.nw_src in
    let ret = set_nw_mask ret 14 m.nw_dst in
    let ret = Word32.bit ret 20 m.dl_vlan_pcp in 
    let ret = Word32.bit ret 21 m.nw_tos in
    Word32.to_int32 ret

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
    let bits = Word32.from_int32 bits in
    { nw_tos = Word32.test_bit 21 bits;
      dl_vlan_pcp = Word32.test_bit 20 bits;
      nw_dst = get_nw_mask bits 14; 
      nw_src = get_nw_mask bits 8; 
      tp_dst = Word32.test_bit 7 bits; 
      tp_src = Word32.test_bit 6 bits; 
      nw_proto = Word32.test_bit 5 bits; 
      dl_type = Word32.test_bit 4 bits; 
      dl_dst = Word32.test_bit 3 bits; 
      dl_src = Word32.test_bit 2 bits; 
      dl_vlan = Word32.test_bit 1 bits; 
      in_port = Word32.test_bit 0 bits;
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
      Wildcards.nw_src = if is_none m.matchNwSrc then 0x3f else 0x0;
      Wildcards.nw_dst = if is_none m.matchNwDst then 0x3f else 0x0;
      Wildcards.dl_vlan_pcp = is_none m.matchDlVlanPcp;
      Wildcards.nw_tos = is_none m.matchNwTos;
  }

  let if_some16 x = match x with
    | Some n -> Word16.to_int n
    | None -> 0

  let if_some8 x = match x with
    | Some n -> Word8.to_int n
    | None -> 0

  let if_some32 x = match x with
    | Some n -> Word32.to_int32 n
    | None -> 0l

  let if_word48 x = match x with
    | Some n -> Word48.to_bytes n
    | None -> "\x00\x00\x00\x00\x00\x00"

 let marshal m bits = 
   set_ofp_match_wildcards bits (Wildcards.marshal (wildcards_of_match m));
   set_ofp_match_in_port bits (if_some16 m.matchInPort); 
   set_ofp_match_dl_src (if_word48 m.matchDlSrc) 0 bits;
   set_ofp_match_dl_dst (if_word48 m.matchDlDst) 0 bits;
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
          Some (Word48.from_bytes 
                  (Cstruct.to_string (get_ofp_match_dl_src bits)));
      matchDlDst = 
        if w.Wildcards.dl_dst then 
          None
        else
          Some (Word48.from_bytes 
                  (Cstruct.to_string (get_ofp_match_dl_dst bits)));
      matchDlVlan =
        if w.Wildcards.dl_vlan then
          None
        else
          Some (Word16.from_int (get_ofp_match_dl_vlan bits));
      matchDlVlanPcp = 
        if w.Wildcards.dl_vlan_pcp then
          None
        else
          Some (Word8.from_int (get_ofp_match_dl_vlan_pcp bits));
      matchDlTyp =
        if w.Wildcards.dl_type then
          None
        else
          Some (Word16.from_int (get_ofp_match_dl_type bits));
      matchNwSrc = 
        if w.Wildcards.nw_src = 0x3f then (* TODO(arjun): prefixes *)
          None
        else
          Some (Word32.from_int32 (get_ofp_match_nw_src bits));
      matchNwDst = 
        if w.Wildcards.nw_dst = 0x3f then (* TODO(arjun): prefixes *)
          None
        else
          Some (Word32.from_int32 (get_ofp_match_nw_dst bits));
      matchNwProto =
        if w.Wildcards.nw_proto then
          None
        else
          Some (Word8.from_int (get_ofp_match_nw_proto bits));
      matchNwTos = 
        if w.Wildcards.nw_tos then 
          None
        else 
          Some (Word8.from_int (get_ofp_match_nw_tos bits));
      matchTpSrc =
        if w.Wildcards.tp_src then
          None
        else
          Some (Word16.from_int (get_ofp_match_tp_src bits));
      matchTpDst =
        if w.Wildcards.tp_dst then
          None
        else
          Some (Word16.from_int (get_ofp_match_tp_dst bits));
      matchInPort =
        if w.Wildcards.in_port then
          None
        else
          Some (Word16.from_int (get_ofp_match_in_port bits));

    }

end

module Header = struct

  let ver : Word8.t = Word8.from_int 0x01


  type t = {
    ver: Word8.t;
    typ: msg_code;
    len: int;
    xid: Word32.t
  }
      
  (** [parse buf] assumes that [buf] has size [sizeof_ofp_header] *)
  let parse buf = 
    { ver = Word8.from_int (get_ofp_header_version buf);
      typ = begin match int_to_msg_code (get_ofp_header_typ buf) with
        | Some typ -> typ
        | None -> raise (Unparsable "unrecognized message code")
      end;
      len = get_ofp_header_length buf;
      xid = Word32.from_int32 (get_ofp_header_xid buf)
    }

end

module Message = struct

  type t = message

  
  let parse (hdr : Header.t) (buf : Cstruct.buf) : xid * t =
    let msg =  match hdr.Header.typ with
      | HELLO -> Hello buf
      | ECHO_REQ -> EchoRequest buf
      | ECHO_RESP -> EchoReply buf
      | FEATURES_REQ -> FeaturesRequest
      | FEATURES_RESP -> FeaturesReply (Features.parse buf)
      | _ -> failwith "expected msg"
    in
    let xid = hdr.Header.xid in
    (xid, msg)

  let msg_code_of_message (msg : t) : msg_code = match msg with
    | Hello _ -> HELLO
    | EchoRequest _ -> ECHO_REQ
    | EchoReply _ -> ECHO_RESP
    | FeaturesRequest -> FEATURES_REQ
    | FeaturesReply _ -> FEATURES_RESP

  open Bigarray

  let sizeof_body (msg : t) : int = match msg with
    | Hello buf -> Array1.dim buf
    | EchoRequest buf -> Array1.dim buf
    | EchoReply buf -> Array1.dim buf
    | FeaturesRequest -> 0
    | FeaturesReply _ -> sizeof_ofp_switch_features
    | _ -> failwith "unknowns"

  let blit_message (msg : t) (out : Cstruct.buf) = match msg with
    | Hello buf
    | EchoRequest buf
    | EchoReply buf ->
      Cstruct.blit_buffer buf 0 out 0 (Cstruct.len buf)
    | FeaturesRequest -> ()

  let marshal (xid : xid) (msg : t) : string = 
    let sizeof_buf = sizeof_ofp_header + sizeof_body msg in
    eprintf "marshaling %d bytes.\n%!" sizeof_buf;
    let buf = Array1.create char c_layout sizeof_buf in
    eprintf "MArshaling.\n%!";
    set_ofp_header_version buf 0x1;
    set_ofp_header_typ buf (msg_code_to_int (msg_code_of_message msg));
    set_ofp_header_length buf sizeof_buf;
    set_ofp_header_xid buf (Word32.to_int32 xid);
    eprintf "Wrote teh header\n%!";
    blit_message msg buf;
    eprintf "Wrote the message body\n%!";
    Cstruct.to_string buf

end
