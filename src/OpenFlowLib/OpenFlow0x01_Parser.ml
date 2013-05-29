(** OpenFlow 1.0 (protocol version 0x01) *)

open Printf
open Frenetic_Bit
open OpenFlow0x01

module Log = Frenetic_Log

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

let string_of_msg_code code = match code with
  | HELLO -> "HELLO"
  | ERROR -> "ERROR"
  | ECHO_REQ -> "ECHO_REQ"
  | ECHO_RESP -> "ECHO_RESP"
  | VENDOR -> "VENDOR"
  | FEATURES_REQ -> "FEATURES_REQ"
  | FEATURES_RESP -> "FEATURES_RESP"
  | GET_CONFIG_REQ -> "GET_CONFIG_REQ"
  | GET_CONFIG_RESP -> "GET_CONFIG_RESP"
  | SET_CONFIG -> "SET_CONFIG"
  | PACKET_IN -> "PACKET_IN"
  | FLOW_REMOVED -> "FLOW_REMOVED"
  | PORT_STATUS -> "PORT_STATUS"
  | PACKET_OUT -> "PACKET_OUT"
  | FLOW_MOD -> "FLOW_MOD"
  | PORT_MOD -> "PORT_MOD"
  | STATS_REQ -> "STATS_REQ"
  | STATS_RESP -> "STATS_RESP"
  | BARRIER_REQ -> "BARRIER_REQ"
  | BARRIER_RESP -> "BARRIER_RESP"
  | QUEUE_GET_CONFIG_REQ -> "QUEUE_GET_CONFIG_REQ"
  | QUEUE_GET_CONFIG_RESP -> "QUEUE_GET_CONFIG_RESP"

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
    { packetInBufferId = bufId;
      packetInTotalLen = total_len;
      packetInPort = in_port;
      packetInReason = reason;
      packetInPacket = Cstruct.shift bits sizeof_ofp_packet_in
    } 
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
        | Buffer _ -> 0
        | Packet bytes -> Cstruct.len bytes)

  let marshal (pktOut : t) (buf : Cstruct.t) : int =
    set_ofp_packet_out_buffer_id buf
      (match pktOut.pktOutBufOrBytes with
        | Buffer n -> n
        | _ -> -1l);
    set_ofp_packet_out_in_port buf
      (match pktOut.pktOutPortId with
        | None -> PseudoPort.none
        | Some n -> n);
    set_ofp_packet_out_actions_len buf
      (sum (List.map Action.sizeof pktOut.pktOutActions));
    let buf = List.fold_left
      (fun buf act -> Cstruct.shift buf (Action.marshal act buf))
      (Cstruct.shift buf sizeof_ofp_packet_out)
      (Action.move_controller_last pktOut.pktOutActions) in
    begin match pktOut.pktOutBufOrBytes with
    | Buffer n -> ()
    | Packet bytes ->
      Cstruct.blit bytes 0 buf 0 (Cstruct.len bytes)
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

module PortFeatures = struct
    
  let parse bits : portFeatures =
    { portFeat10MBHD = test_bit 0 bits;
      portFeat10MBFD = test_bit 1 bits;
      portFeat100MBHD = test_bit 2 bits;
      portFeat100MBFD = test_bit 3 bits;
      portFeat1GBHD = test_bit 4 bits;
      portFeat1GBFD = test_bit 5 bits;
      portFeat10GBFD = test_bit 6 bits;
      portFeatCopper = test_bit 7 bits;
      portFeatFiber = test_bit 8 bits;
      portFeatAutoneg = test_bit 9 bits;
      portFeatPause = test_bit 10 bits;
      portFeatPauseAsym = test_bit 11 bits
    }

end

module PortState = struct

  (* MJR: GAH, the enum values from OF1.0 make NO SENSE AT ALL. Two of
     them have the SAME value, and the rest make no sense as bit
     vectors. Only portStateDown is parsed correctly ATM *)
  let parse bits : portState =
    { portStateDown = test_bit 0 bits;
      portStateSTPListen = false;
      portStateSTPForward = false;
      portStateSTPBlock = false;
      portStateSTPMask = false
    }

end

module PortConfig = struct

  let parse bits : portConfig =
    { portConfigDown = test_bit 0 bits;
      portConfigNoSTP = test_bit 1 bits;
      portConfigNoRecv = test_bit 2 bits;
      portConfigNoRecvSTP = test_bit 3 bits;
      portConfigNoFlood = test_bit 4 bits;
      portConfigNoFWD = test_bit 5 bits;
      portConfigNoPacketIn = test_bit 6 bits
    }

end

module PortDesc = struct

    cstruct ofp_phy_port {
      uint16_t port_no;
      uint8_t hw_addr[6];
      uint8_t name[16]; (* OFP_MAX_PORT_NAME_LEN, Null-terminated *)
      uint32_t config; (* Bitmap of OFPPC_* flags. *)
      uint32_t state; (* Bitmap of OFPPS_* flags. *)
      (* Bitmaps of OFPPF_* that describe features. All bits zeroed if
       * unsupported or unavailable. *)
      uint32_t curr; (* Current features. *)
      uint32_t advertised; (* Features being advertised by the port. *)
      uint32_t supported; (* Features supported by the port. *)
      uint32_t peer (* Features advertised by peer. *)
    } as big_endian
    
  let parse (bits : Cstruct.t) : portDesc =
    let portDescPortNo = get_ofp_phy_port_port_no bits in
    let hw_addr = Packet.mac_of_bytes (Cstruct.to_string (get_ofp_phy_port_hw_addr bits)) in
    let name = Cstruct.to_string (get_ofp_phy_port_name bits) in
    let config = PortConfig.parse (get_ofp_phy_port_config bits) in
    let state = PortState.parse (get_ofp_phy_port_state bits) in
    let curr = PortFeatures.parse (get_ofp_phy_port_curr bits) in
    let advertised = PortFeatures.parse (get_ofp_phy_port_advertised bits) in
    let supported = PortFeatures.parse (get_ofp_phy_port_supported bits) in
    let peer = PortFeatures.parse (get_ofp_phy_port_peer bits) in
    { portDescPortNo;
      portDescHwAddr = hw_addr;
      portDescName = name;
      portDescConfig = config;
      portDescState = state;
      portDescCurr = curr;
      portDescAdvertised = advertised;
      portDescSupported = supported;
      portDescPeer = peer
    }
end

module PortReason = struct

    cenum ofp_port_reason {
      OFPPR_ADD;
      OFPPR_DELETE;
      OFPPR_MODIFY
    } as uint8_t


  let parse bits : portChangeReason =
    match (int_to_ofp_port_reason bits) with
      | Some OFPPR_ADD -> PortAdd
      | Some OFPPR_DELETE -> PortDelete
      | Some OFPPR_MODIFY -> PortModify

  let to_string rea = match rea with
    | PortAdd -> "PortAdd"
    | PortDelete -> "PortDelete"
    | PortModify -> "PortModify"

end

module PortStatus = struct

    cstruct ofp_port_status {
      uint8_t reason;               (* One of OFPPR_* *)
      uint8_t pad[7]
    } as big_endian


  let parse (bits : Cstruct.t) : portStatus =
    let portStatusReason = PortReason.parse (get_ofp_port_status_reason bits) in 
    let _ = get_ofp_port_status_pad bits in
    let portStatusDesc = PortDesc.parse bits in
    { portStatusReason;
      portStatusDesc }

  let to_string ps =
    let {portStatusReason; portStatusDesc} = ps in
    Printf.sprintf "PortStatus %s %d" (PortReason.to_string portStatusReason)
      (portStatusDesc.portDescPortNo)
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
      (Action.move_controller_last m.mfActions) in 
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

  let to_string hdr =
    Printf.sprintf "{ %d, %s, len = %d, xid = %d }"
      hdr.ver
      (string_of_msg_code hdr.typ)
      hdr.len
      (Int32.to_int hdr.xid)

end

module StatsRequest = struct
    
  type t = statsRequest

  cstruct ofp_stats_request {
    uint16_t req_type;
    uint16_t flags
  } as big_endian

  let to_string msg = match msg with
    | DescriptionReq -> "DescriptionReq"
    | IndividualFlowReq req ->
      "IndividualFlowReq " ^ (IndividualFlowRequest.to_string req)
    | AggregateFlowReq req ->
      "AggregateFlowReq " ^ (AggregateFlowRequest.to_string req)
    | TableReq -> "TableReq"
    | PortReq p -> "PortReq " ^ (PseudoPort.to_string p)

  let sizeof msg =
    let header_size = sizeof_ofp_stats_request in
    match msg with
    | DescriptionReq -> header_size
    | IndividualFlowReq req -> 
      header_size + (IndividualFlowRequest.sizeof req)
    | _ -> 
      (* CNS: Please implement me!! *)
      failwith (Printf.sprintf "NYI: StatsRequest.sizeof %s" (to_string msg))

  let ofp_stats_type_of_request req = match req with
    | DescriptionReq -> OFPST_DESC
    | IndividualFlowReq _ -> OFPST_FLOW
    | AggregateFlowReq _ -> OFPST_AGGREGATE
    | TableReq -> OFPST_TABLE
    | PortReq _ -> OFPST_PORT

  let marshal msg out = 
    let req_type = ofp_stats_type_of_request msg in
    let flags = 0x0 in
    set_ofp_stats_request_req_type out (ofp_stats_types_to_int req_type);
    set_ofp_stats_request_flags out flags;
    let out' = Cstruct.shift out sizeof_ofp_stats_request in
    match msg with
    | DescriptionReq -> sizeof_ofp_stats_request
    | IndividualFlowReq req -> IndividualFlowRequest.marshal req out'
    | AggregateFlowReq req -> AggregateFlowRequest.marshal req out'
    | _ -> 
      failwith (Printf.sprintf "NYI: StatsRequest.marshal %s" (to_string msg))

end

module StatsReply = struct
    
  type t = statsReply

  cstruct ofp_stats_reply {
    uint16_t stats_type;
    uint16_t flags
  } as big_endian

  let sizeof msg = failwith "NYI: StatsReply.sizeof"

  module Description = struct

    type t = DescriptionStats.t

    let desc_str_len = 256
    let serial_num_len = 32
  
    cstruct ofp_desc_stats {
      uint8_t mfr_desc[256];
      uint8_t hw_desc[256];
      uint8_t sw_desc[256];
      uint8_t serial_num[32];
      uint8_t dp_desc[256]
    } as big_endian
  
    let mkString bits size =
      let new_string = String.create size in
      Cstruct.blit_to_string bits 0 new_string 0 size;
      new_string
  
    let parse bits =
      let mfr_desc = mkString (get_ofp_desc_stats_mfr_desc bits) desc_str_len in
      let hw_desc = mkString (get_ofp_desc_stats_hw_desc bits) desc_str_len in
      let sw_desc = mkString (get_ofp_desc_stats_sw_desc bits) desc_str_len in
      let serial_num = 
        mkString (get_ofp_desc_stats_serial_num bits) serial_num_len in
      let dp_desc = mkString (get_ofp_desc_stats_dp_desc bits) desc_str_len in
      { DescriptionStats.manufacturer = mfr_desc
      ; DescriptionStats.hardware = hw_desc
      ; DescriptionStats.software = sw_desc
      ; DescriptionStats.serial_number = serial_num
      ; DescriptionStats.datapath = dp_desc }

  end

  module Flow = struct

    cstruct ofp_flow_stats {
      uint16_t length;
      uint8_t table_id;
      uint8_t pad;
      uint8_t of_match[40]; (* Size of struct ofp_match. *)
      uint32_t duration_sec;
      uint32_t duration_nsec;
      uint16_t priority;
      uint16_t idle_timeout;
      uint16_t hard_timeout;
      uint8_t pad2[6];
      uint64_t cookie;
      uint64_t packet_count;
      uint64_t byte_count
    } as big_endian

    type t = IndividualFlowStats.t

    let _parse bits =
      (* length = flow stats + actions *)
      let length = get_ofp_flow_stats_length bits in
      let flow_stats_size = sizeof_ofp_flow_stats in
      let actions_size = length - flow_stats_size in

      (* get fields *)
      let table_id = get_ofp_flow_stats_table_id bits in
      let of_match = Match.parse (get_ofp_flow_stats_of_match bits) in
      let duration_sec = get_ofp_flow_stats_duration_sec bits in
      let duration_nsec = get_ofp_flow_stats_duration_nsec bits in
      let priority = get_ofp_flow_stats_priority bits in
      let idle_timeout = get_ofp_flow_stats_idle_timeout bits in
      let hard_timeout = get_ofp_flow_stats_hard_timeout bits in
      let cookie = get_ofp_flow_stats_cookie bits in
      let packet_count = get_ofp_flow_stats_packet_count bits in
      let byte_count = get_ofp_flow_stats_byte_count bits in

      (* get actions *)
      let bits_after_flow_stats = Cstruct.shift bits sizeof_ofp_flow_stats in
      let action_bits, rest = 
        Cstruct.split bits_after_flow_stats actions_size in
      let actions = Action.parse_sequence action_bits in

      let open IndividualFlowStats in
      ( { table_id = table_id
        ; of_match = of_match
        ; duration_sec = Int32.to_int duration_sec
        ; duration_nsec = Int32.to_int duration_nsec
        ; priority = priority
        ; idle_timeout = idle_timeout
        ; hard_timeout = hard_timeout
        ; cookie = cookie
        ; packet_count = packet_count
        ; byte_count = byte_count
        ; actions = actions }
      , rest)

    let parse bits = fst (_parse bits)

    let rec parse_sequence bits =
      if Cstruct.len bits <= 0 then
        []
      else
        let (v, bits') = _parse bits in
        v :: parse_sequence bits'

  end

  module Aggregate = struct

    type t = AggregateFlowStats.t
    let parse bits = failwith "NYI"

  end

  module Table = struct

    type t = TableStats.t
    let parse bits = failwith "NYI"

  end

  module Port = struct

    type t = PortStats.t
    let parse bits = failwith "NYI"

  end

  let parse bits =
    let stats_type_code = get_ofp_stats_reply_stats_type bits in
    let body = Cstruct.shift bits sizeof_ofp_stats_reply in
    match int_to_ofp_stats_types stats_type_code with
    | Some OFPST_DESC -> DescriptionRep (Description.parse body)
    | Some OFPST_FLOW -> IndividualFlowRep (Flow.parse_sequence body)
    | Some OFPST_AGGREGATE -> AggregateFlowRep (Aggregate.parse body)
    | Some OFPST_TABLE -> TableRep (Table.parse body)
    | Some OFPST_PORT -> PortRep (Port.parse body)
    | Some OFPST_QUEUE -> 
      let msg = "NYI: OFPST_QUEUE ofp_stats_type in stats_reply" in
      raise (Unparsable msg)
    | Some OFPST_VENDOR -> 
      let msg = "NYI: OFPST_VENDOR ofp_stats_type in stats_reply" in
      raise (Unparsable msg)
    | None -> 
      let msg = 
        sprintf "bad ofp_stats_type in stats_reply (%d)" stats_type_code in
      raise (Unparsable msg)

end

module Error = struct
  
  type t = error
  
  cstruct ofp_error_msg {
    uint16_t error_type;
    uint16_t error_code
  } as big_endian
  
  cenum ofp_error_type {
    OFPET_HELLO_FAILED;
    OFPET_BAD_REQUEST;
    OFPET_BAD_ACTION;
    OFPET_FLOW_MOD_FAILED;
    OFPET_PORT_MOD_FAILED;
    OFPET_QUEUE_OP_FAILED
  } as uint16_t
  
  module HelloFailed = struct
    
    type t = helloFailedError
    
    cenum ofp_hello_failed_code {
      OFPHFC_INCOMPATIBLE;
      OFPHFC_EPERM
    } as uint16_t
    
    let parse error_code =
      match int_to_ofp_hello_failed_code error_code with
      | Some OFPHFC_INCOMPATIBLE -> HF_Incompatible
      | Some OFPHFC_EPERM -> HF_Eperm
      | None ->
        let msg = "NYI: ofp_hello_failed_code in error" in
              raise (Unparsable msg)
  end
  
  module BadRequest = struct
    
    type t = badRequestError
    
    cenum ofp_bad_request_code {
      OFPBRC_BAD_VERSION;
      OFPBRC_BAD_TYPE;
      OFPBRC_BAD_STAT;
      OFPBRC_BAD_VENDOR;
      OFPBRC_BAD_SUBTYPE;
      OFPBRC_EPERM;
      OFPBRC_BAD_LEN;
      OFPBRC_BUFFER_EMPTY;
      OFPBRC_BUFFER_UNKNOWN
    } as uint16_t
    
    let parse error_code = 
      match int_to_ofp_bad_request_code error_code with
      | Some OFPBRC_BAD_VERSION -> BR_BadVersion
      | Some OFPBRC_BAD_TYPE -> BR_BadType
      | Some OFPBRC_BAD_STAT -> BR_BadStat
      | Some OFPBRC_BAD_VENDOR -> BR_BadVendor
      | Some OFPBRC_BAD_SUBTYPE -> BR_BadSubType
      | Some OFPBRC_EPERM -> BR_Eperm
      | Some OFPBRC_BAD_LEN -> BR_BadLen
      | Some OFPBRC_BUFFER_EMPTY -> BR_BufferEmpty
      | Some OFPBRC_BUFFER_UNKNOWN -> BR_BufferUnknown
      | None ->
        let msg = "NYI: ofp_bad_request_code in error" in
              raise (Unparsable msg)
  end
  
  module BadAction = struct
    
    type t = badActionError
    
    cenum ofp_bad_action_code {
      OFPBAC_BAD_TYPE;
      OFPBAC_BAD_LEN;
      OFPBAC_BAD_VENDOR;
      OFPBAC_BAD_VENDOR_TYPE;
      OFPBAC_BAD_OUT_PORT;
      OFPBAC_BAD_ARGUMENT;
      OFPBAC_EPERM;
      OFPBAC_TOO_MANY;
      OFPBAC_BAD_QUEUE
    } as uint16_t
    
    let parse error_code =
      match int_to_ofp_bad_action_code error_code with
      | Some OFPBAC_BAD_TYPE -> BA_BadType
      | Some OFPBAC_BAD_LEN -> BA_BadLen
      | Some OFPBAC_BAD_VENDOR -> BA_BadVendor
      | Some OFPBAC_BAD_VENDOR_TYPE -> BA_BadVendorType
      | Some OFPBAC_BAD_OUT_PORT -> BA_BadOutPort
      | Some OFPBAC_BAD_ARGUMENT -> BA_BadArgument
      | Some OFPBAC_EPERM -> BA_Eperm
      | Some OFPBAC_TOO_MANY -> BA_TooMany
      | Some OFPBAC_BAD_QUEUE -> BA_BadQueue
      | None ->
        let msg = "NYI: ofp_bad_action_code in error" in
              raise (Unparsable msg)
  end
  
  module FlowModFailed = struct
    
    type t = flowModFailedError
    
    cenum ofp_flow_mod_failed_code {
      OFPFMFC_ALL_TABLES_FULL;
      OFPFMFC_OVERLAP;
      OFPFMFC_EPERM;
      OFPFMFC_BAD_EMERG_TIMEOUT;
      OFPFMFC_BAD_COMMAND;
      OFPFMFC_UNSUPPORTED
    } as uint16_t
    
    let parse error_code =
      match int_to_ofp_flow_mod_failed_code error_code with
      | Some OFPFMFC_ALL_TABLES_FULL -> FM_AllTablesFull
      | Some OFPFMFC_OVERLAP -> FM_Overlap
      | Some OFPFMFC_EPERM -> FM_Eperm
      | Some OFPFMFC_BAD_EMERG_TIMEOUT -> FM_BadEmergTimeout
      | Some OFPFMFC_BAD_COMMAND -> FM_BadCommand
      | Some OFPFMFC_UNSUPPORTED -> FM_Unsupported
      | None ->
        let msg = "NYI: ofp_flow_mod_failed_code in error" in
              raise (Unparsable msg)
  end
  
  module PortModFailed = struct
    
    type t = portModFailedError
    
    cenum ofp_port_mod_failed_code {
      OFPPMFC_BAD_PORT;
      OFPPMFC_BAD_HW_ADDR
    } as uint16_t
    
    let parse error_code =
      match int_to_ofp_port_mod_failed_code error_code with
      | Some OFPPMFC_BAD_PORT -> PM_BadPort
      | Some OFPPMFC_BAD_HW_ADDR -> PM_BadHwAddr
      | None ->
        let msg = "NYI: ofp_port_mod_failed_code in error" in
              raise (Unparsable msg)
  end
  
  module QueueOpFailed = struct
    
    type t = queueOpFailedError
    
    cenum ofp_queue_op_failed_code {
      OFPQOFC_BAD_PORT;
      OFPQOFC_BAD_QUEUE;
      OFPQOFC_EPERM
    } as uint16_t
    
    let parse error_code = 
      match int_to_ofp_queue_op_failed_code error_code with
      | Some OFPQOFC_BAD_PORT -> QO_BadPort
      | Some OFPQOFC_BAD_QUEUE -> QO_BadQueue
      | Some OFPQOFC_EPERM -> QO_Eperm
      | None ->
        let msg = "NYI: ofp_queue_op_failed_code in error" in
              raise (Unparsable msg)
  end
  
  let parse bits =
    let error_type = get_ofp_error_msg_error_type bits in
    let error_code = get_ofp_error_msg_error_code bits in
    let body = Cstruct.shift bits sizeof_ofp_error_msg in
    match int_to_ofp_error_type error_type with
    | Some OFPET_HELLO_FAILED -> HelloFailed ((HelloFailed.parse error_code), body)
    | Some OFPET_BAD_REQUEST -> BadRequest ((BadRequest.parse error_code), body)
    | Some OFPET_BAD_ACTION -> BadAction ((BadAction.parse error_code), body)
    | Some OFPET_FLOW_MOD_FAILED -> FlowModFailed ((FlowModFailed.parse error_code), body)
    | Some OFPET_PORT_MOD_FAILED -> PortModFailed ((PortModFailed.parse error_code), body)
    | Some OFPET_QUEUE_OP_FAILED -> QueueOpFailed ((QueueOpFailed.parse error_code), body)
    | None ->
      let msg =
        sprintf "bad ofp_error_type in ofp_error_msg (%d)" error_type in
      raise(Unparsable msg)
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
      | PORT_STATUS -> Some (PortStatusMsg (PortStatus.parse buf))
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
      let _ = StatsRequest.marshal msg out in
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


