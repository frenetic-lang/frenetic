(** OpenFlow 1.0 (protocol version 0x01) *)

open Printf
open OpenFlow0x01
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

cenum ofp_stats_types {
  OFPST_DESC;
  OFPST_FLOW;
  OFPST_AGGREGATE;
  OFPST_TABLE;
  OFPST_PORT;
  OFPST_QUEUE;
  OFPST_VENDOR = 0xffff
} as uint16_t


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
  let pkt = match Packet_Parser.parse_packet pkt_bits with 
    | Some pkt -> pkt 
    | None -> 
      raise (Unparsable (sprintf "malformed packet in packet_in")) in
  { packetInBufferId = bufId;
    packetInTotalLen = total_len;
    packetInPort = in_port;
    packetInReason = reason;
    packetInPacket = pkt }
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
        | Misc.Inl _ -> 0
        | Misc.Inr bytes -> Cstruct.len bytes)

  let marshal (pktOut : t) (buf : Cstruct.t) : int =
    set_ofp_packet_out_buffer_id buf
      (match pktOut.pktOutBufOrBytes with
        | Misc.Inl n -> n
        | _ -> -1l);
    set_ofp_packet_out_in_port buf
      (match pktOut.pktOutPortId with
        | None -> PseudoPort.none
        | Some n -> n);
    set_ofp_packet_out_actions_len buf
      (sum (List.map Action.sizeof pktOut.pktOutActions));
    let _ = List.fold_left
      (fun buf act -> Cstruct.shift buf (Action.marshal act buf))
      (Cstruct.shift buf sizeof_ofp_packet_out)
      (Action.move_controller_last pktOut.pktOutActions) in
    begin match pktOut.pktOutBufOrBytes with
    | Misc.Inl n -> ()
    | _ -> ()
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

module StatsRequest = struct
    
    type t = statsRequest

    let sizeof buf = failwith "NYI: StatsRequest.sizeof"

    let marshal msg = failwith "NYI: StatsRequest.marshal"

end

module StatsReply = struct
    
    type t = statsReply

    let sizeof msg = failwith "NYI: StatsReply.sizeof"

    let parse buf = failwith "NYI: StatsReply.parse"

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
      | STATS_RESP -> Some (StatsReplyMsg (StatsReply.parse buf))
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
    | StatsRequestMsg _ -> STATS_REQ
    | StatsReplyMsg _ -> STATS_RESP

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
    | StatsRequestMsg _ -> "StatsRequest"
    | StatsReplyMsg _ -> "StatsReply"

  open Bigarray

  (** Size of the message body, without the header *)
  let sizeof_body (msg : t) : int = match msg with
    | Hello buf -> Cstruct.len buf
    | EchoRequest buf -> Cstruct.len buf
    | EchoReply buf -> Cstruct.len buf
    | FeaturesRequest -> 0
    | FeaturesReply _ -> sizeof_ofp_switch_features
    | FlowModMsg msg ->
      Match.size + sizeof_ofp_flow_mod + 
        sum (List.map Action.sizeof msg.mfActions)
    | PacketOutMsg msg -> PacketOut.sizeof msg
    | BarrierRequest -> 0
    | BarrierReply -> 0
    | StatsRequestMsg msg -> StatsRequest.sizeof msg
    | StatsReplyMsg msg -> StatsReply.sizeof msg
    | _ -> 
      failwith "Not yet implemented"

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
    | StatsRequestMsg msg -> 
      let _ = StatsRequest.marshal msg in
      ()
    | StatsReplyMsg _ -> ()

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


