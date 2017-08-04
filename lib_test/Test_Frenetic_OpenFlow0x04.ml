open Core
open Frenetic_OpenFlow0x04
open Arbitrary_Frenetic_OpenFlow0x04

(* Mostly stolen from Cstruct.hexdump, but less pretty format *)
let buf_to_hex t =
  let c = ref 0 in
  let s = ref "" in
  for i = 0 to Cstruct.len t - 1 do
    s := !s ^ (Printf.sprintf "%.2x" (Char.to_int (Bigarray.Array1.get t.buffer (t.off+i))));
    incr c;
  done;
  !s 

let hex_to_buf str =
  let s = ref "" in
  for i = 0 to (String.length str)/2-1 do
    let hexbyte = (String.sub str (i*2) 2) in
    let corresponding_char = Char.of_int (int_of_string ("0x" ^ hexbyte)) in
    s := !s ^ (
      match corresponding_char with
      | None -> ""
      | Some ch -> Char.to_string ch 
    );
  done;
  !s

(* Pads a string with zeroes so that it is atleast `len` characters. *)
let zero_pad (len : int) (e : string) : string =
  let padding_size = max 0 (len - (String.length e)) in
  let padding = String.make padding_size '\000' in
  String.concat [e; padding]

let test_marshal frenetic_msg msg_name =
  let cs = Message.marshal 0l frenetic_msg in
  let frenetic_hex = buf_to_hex (Cstruct.of_string cs) in
  let ryu_hex_file_name = "lib_test/data/openflow0x04/" ^ msg_name ^ ".hex" in
  let ryu_hex = In_channel.read_all ryu_hex_file_name in
  (*printf "RYU: %s\nFR:  %s\n%!" ryu_hex frenetic_hex; *)
  frenetic_hex = ryu_hex

let debug_parse frenetic_msg ryu_hex ryu_msg = 
  let frenetic_as_string = Message.to_string frenetic_msg in
  let cs = Message.marshal 0l frenetic_msg in
  let frenetic_hex = buf_to_hex (Cstruct.of_string cs) in
  let ryu_as_string = Message.to_string ryu_msg in
  printf "RYU: %s\nFR:  %s\n%!" ryu_hex frenetic_hex;
  printf "RYU: %s\nFR:  %s\n%!" ryu_as_string frenetic_as_string; 
  ()

let test_parse_hex frenetic_msg ryu_hex = 
  let ryu_raw = hex_to_buf ryu_hex in
  let header_buf = String.sub ryu_raw 0 Frenetic_OpenFlow_Header.size in
  let header = Frenetic_OpenFlow_Header.parse (Cstruct.of_string header_buf) in
  let message_buf = String.sub ryu_raw (Frenetic_OpenFlow_Header.size) ((String.length ryu_raw) - (Frenetic_OpenFlow_Header.size)) in
  let ryu_msg = snd (Frenetic_OpenFlow0x04.Message.parse header message_buf) in 
  (* debug_parse frenetic_msg ryu_hex ryu_msg; *)
  ryu_msg = frenetic_msg

let test_parse frenetic_msg msg_name = 
  let ryu_hex_file_name = "lib_test/data/openflow0x04/" ^ msg_name ^ ".hex" in
  let ryu_hex = In_channel.read_all ryu_hex_file_name in
  test_parse_hex frenetic_msg ryu_hex
  
(******** OFPT_HELLO *)

let%test "OfpHello Marshal" = 
  let frenetic_msg = Message.Hello ([ VersionBitMap [4; 1] ]) in
  test_marshal frenetic_msg "OfpHello"

let%test "OfpHello Parse" = 
  (* This requires the bitmaps to be listed [4;1] instead of [1;4] because Ocaml considers those
     two lists as different.  Perhaps VersionBitMap should use sets instead of lists. *)
  let frenetic_msg = Message.Hello ([ VersionBitMap [4; 1] ]) in
  test_parse frenetic_msg "OfpHello"

(******** OFPT_ERROR_MESSAGE *)

let%test "OfpErrorMessage Parse" =
  let all_error_combinations = [
    ("OFPET_TABLE_MOD_FAILED_OFPTMFC_BAD_CONFIG", TableModFailed TaBadConfig);
    ("OFPET_TABLE_MOD_FAILED_OFPTMFC_BAD_TABLE", TableModFailed TaBadTable);
    ("OFPET_TABLE_MOD_FAILED_OFPTMFC_EPERM", TableModFailed TaPermError);
    ("OFPET_SWITCH_CONFIG_FAILED_OFPSCFC_BAD_FLAGS", SwitchConfigFailed ScBadFlags);
    ("OFPET_SWITCH_CONFIG_FAILED_OFPSCFC_BAD_LEN", SwitchConfigFailed ScBadLen);
    ("OFPET_SWITCH_CONFIG_FAILED_OFPSCFC_EPERM", SwitchConfigFailed ScPermError);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_BAD_EXPERIMENTER", BadInstruction InstBadExp);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_BAD_EXP_TYPE", BadInstruction InstBadExpTyp);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_BAD_LEN", BadInstruction InstBadLen);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_BAD_TABLE_ID", BadInstruction InstBadTableId);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_EPERM", BadInstruction InstPermError);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_UNKNOWN_INST", BadInstruction InstUnknownInst);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_UNSUP_INST", BadInstruction InstUnsupInst);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_UNSUP_METADATA", BadInstruction InstUnsupMeta);
    ("OFPET_BAD_INSTRUCTION_OFPBIC_UNSUP_METADATA_MASK", BadInstruction InstUnsupMetaMask);
    ("OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_ARGUMENT", TableFeatFailed TfBadArg);
    ("OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_LEN", TableFeatFailed TfBadLen);
    ("OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_METADATA", TableFeatFailed TfBadMeta);
    ("OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_TABLE", TableFeatFailed TfBadTable);
    ("OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_TYPE", TableFeatFailed TfBadType);
    ("OFPET_TABLE_FEATURES_FAILED_OFPTFFC_EPERM", TableFeatFailed TfPermError);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_BAD_BAND", MeterModFailed MeBadBand);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_BAD_BAND_VALUE", MeterModFailed MeBadBandValue);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_BAD_BURST", MeterModFailed MeBadBurst);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_BAD_COMMAND", MeterModFailed MeBadCommand);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_BAD_FLAGS", MeterModFailed MeBadFlags);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_BAD_RATE", MeterModFailed MeBadRate);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_INVALID_METER", MeterModFailed MeInvalidMeter);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_METER_EXISTS", MeterModFailed MeMeterExists);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_OUT_OF_BANDS", MeterModFailed MeOutOfBands);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_OUT_OF_METERS", MeterModFailed MeOutOfMeters);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_UNKNOWN", MeterModFailed MeUnknown);
    ("OFPET_METER_MOD_FAILED_OFPMMFC_UNKNOWN_METER", MeterModFailed MeUnknownMeter);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_ARGUMENT", BadAction ActBadArg);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_EXPERIMENTER", BadAction ActBadExp);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_EXP_TYPE", BadAction ActBadExpType);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_LEN", BadAction ActBadLen);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_OUT_GROUP", BadAction ActBadOutGroup);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_OUT_PORT", BadAction ActBadOutPort);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_QUEUE", BadAction ActBadQueue);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_SET_ARGUMENT", BadAction ActBadSetArg);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_SET_LEN", BadAction ActBadSetLen);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_SET_TYPE", BadAction ActBadSetTyp);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_TAG", BadAction ActBadTag);
    ("OFPET_BAD_ACTION_OFPBAC_BAD_TYPE", BadAction ActBadType);
    ("OFPET_BAD_ACTION_OFPBAC_EPERM", BadAction ActPermError);
    ("OFPET_BAD_ACTION_OFPBAC_MATCH_INCONSISTENT", BadAction ActMatchInconsistent);
    ("OFPET_BAD_ACTION_OFPBAC_TOO_MANY", BadAction ActTooMany);
    ("OFPET_BAD_ACTION_OFPBAC_UNSUPPORTED_ORDER", BadAction ActUnsupportedOrder);
    ("OFPET_QUEUE_OP_FAILED_OFPQOFC_BAD_PORT", QueueOpFailed QuBadPort);
    ("OFPET_QUEUE_OP_FAILED_OFPQOFC_BAD_QUEUE", QueueOpFailed QuBadQueue);
    ("OFPET_QUEUE_OP_FAILED_OFPQOFC_EPERM", QueueOpFailed QuPermError);
    ("OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_ADVERTISE", PortModFailed PoBadAdvertise);
    ("OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_CONFIG", PortModFailed PoBadConfig);
    ("OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_HW_ADDR", PortModFailed PoBadHwAddr);
    ("OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_PORT", PortModFailed PoBadPort);
    ("OFPET_PORT_MOD_FAILED_OFPPMFC_EPERM", PortModFailed PoPermError);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_COMMAND", FlowModFailed FlBadCommand);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_FLAGS", FlowModFailed FlBadFlags);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_TABLE_ID", FlowModFailed FlBadTableId);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_TIMEOUT", FlowModFailed FlBadTimeout);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_EPERM", FlowModFailed FlPermError);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_OVERLAP", FlowModFailed FlOverlap);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_TABLE_FULL", FlowModFailed FlTableFull);
    ("OFPET_FLOW_MOD_FAILED_OFPFMFC_UNKNOWN", FlowModFailed FlUnknown);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_BUCKET", GroupModFailed GrBadBucket);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_COMMAND", GroupModFailed GrBadCommand);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_TYPE", GroupModFailed GrBadTyp);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_WATCH", GroupModFailed GrBadWatch);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_CHAINED_GROUP", GroupModFailed GrChainedGroup);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_CHAINING_UNSUPPORTED", GroupModFailed GrChainingUnsupported);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_EPERM", GroupModFailed GrPermError);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_GROUP_EXISTS", GroupModFailed GrGroupExists);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_INVALID_GROUP", GroupModFailed GrInvalidGroup);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_LOOP", GroupModFailed GrLoop);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_OUT_OF_BUCKETS", GroupModFailed GrOutOfBuckets);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_OUT_OF_GROUPS", GroupModFailed GrOutOfGroups);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_UNKNOWN_GROUP", GroupModFailed GrUnknownGroup);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_WATCH_UNSUPPORTED", GroupModFailed GrWatchUnsupported);
    ("OFPET_GROUP_MOD_FAILED_OFPGMFC_WEIGHT_UNSUPPORTED", GroupModFailed GrWeightUnsupported);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_EXPERIMENTER", BadRequest ReqBadExp);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_EXP_TYPE", BadRequest ReqBadExpType);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_LEN", BadRequest ReqBadLen);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_MULTIPART", BadRequest ReqBadMultipart);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_PACKET", BadRequest ReqBadPacket);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_PORT", BadRequest ReqBadPort);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_TABLE_ID", BadRequest ReqBadTableId);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_TYPE", BadRequest ReqBadType);
    ("OFPET_BAD_REQUEST_OFPBRC_BAD_VERSION", BadRequest ReqBadVersion);
    ("OFPET_BAD_REQUEST_OFPBRC_BUFFER_EMPTY", BadRequest ReqBufferEmpty);
    ("OFPET_BAD_REQUEST_OFPBRC_BUFFER_UNKNOWN", BadRequest ReqBufferUnknown);
    ("OFPET_BAD_REQUEST_OFPBRC_EPERM", BadRequest ReqPermError);
    ("OFPET_BAD_REQUEST_OFPBRC_IS_SLAVE", BadRequest ReqIsSlave);
    ("OFPET_BAD_REQUEST_OFPBRC_MULTIPART_BUFFER_OVERFLOW", BadRequest ReqMultipartBufOverflow);
    ("OFPET_ROLE_REQUEST_FAILED_OFPRRFC_BAD_ROLE", RoleReqFailed RoBadRole);
    ("OFPET_ROLE_REQUEST_FAILED_OFPRRFC_STALE", RoleReqFailed RoStale);
    ("OFPET_ROLE_REQUEST_FAILED_OFPRRFC_UNSUP", RoleReqFailed RoUnsup);
    ("OFPET_HELLO_FAILED_OFPHFC_EPERM", HelloFailed HelloPermError);
    ("OFPET_HELLO_FAILED_OFPHFC_INCOMPATIBLE", HelloFailed HelloIncompatible)
  ] in 
  (* error_msg_hex is a hashtable mapping OFPET_* error messages to a hex packet dump *)
  let error_msg_hex = String.Table.create () ~size:50 in 
  let read_emh () = 
    In_channel.with_file "lib_test/data/openflow0x04/OfpErrorMsg.hex" ~f:(fun file ->
    In_channel.iter_lines file ~f:(fun line ->
      let line_components = String.lsplit2_exn line ~on:',' in  
      let _ = Hashtbl.add error_msg_hex (fst line_components) (snd line_components) in
      ()
    )
  ) in
  let test_one_msg msg_pair = 
    let (msg_name, error_msg) = msg_pair in
    let error_rec = {
      err = error_msg; 
      data = Cstruct.of_string msg_name
    } in
    let frenetic_msg = Message.Error error_rec in
    let ryu_hex = Hashtbl.find_exn error_msg_hex (Cstruct.to_string error_rec.data) in
    test_parse_hex frenetic_msg ryu_hex
  in
  let () = read_emh () in 
  List.fold_left ~init:true ~f:(&&) (List.map ~f:test_one_msg all_error_combinations)

(******** OFPT_ECHO_REQUEST *)

let%test "OfpEchoRequest Marshal" = 
  let frenetic_msg = Message.EchoRequest (Cstruct.of_string "OfpEchoRequest") in
  test_marshal frenetic_msg "OfpEchoRequest"

let%test "OfpEchoRequest Parse" = 
  let frenetic_msg = Message.EchoRequest (Cstruct.of_string "OfpEchoRequest") in
  test_parse frenetic_msg "OfpEchoRequest"

(******** OFPT_ECHO_REPLY *)

let%test "OfpEchoReply Marshal" = 
  let frenetic_msg = Message.EchoReply (Cstruct.of_string "OfpEchoReply") in
  test_marshal frenetic_msg "OfpEchoReply"

let%test "OfpEchoReply Parse" = 
  let frenetic_msg = Message.EchoReply (Cstruct.of_string "OfpEchoReply") in
  test_parse frenetic_msg "OfpEchoReply"

(******** OFPT_FEATURES_REQUEST *)

let%test "OfpFeaturesRequest Marshal" = 
  let frenetic_msg = Message.FeaturesRequest in
  test_marshal frenetic_msg "OfpFeaturesRequest"

(******** OFPT_FEATURES_REPLY *)

let%test "OfpFeaturesReply Parse" = 
  let feat_reply = {  
    datapath_id = 9210263729383L;
    num_buffers = 897345987l;
    num_tables = 250;
    aux_id = 65;
    supported_capabilities = { 
      flow_stats = true; table_stats = false; port_stats = false;
      group_stats = true; ip_reasm = false; queue_stats = false; port_blocked = true
    };
  } in
  let frenetic_msg = Message.FeaturesReply feat_reply in
  test_parse frenetic_msg "OfpFeaturesReply"

(******** OFPT_GET_CONFIG_REQUEST *)

let%test "OfpGetConfigRequest Marshal" = 
  let frenetic_msg = Message.GetConfigRequestMsg in
  test_marshal frenetic_msg "OfpGetConfigRequest"

(******** OFPT_GET_CONFIG_REPLY *)

let%test "OfpGetConfigReply Parse" = 
  let config_reply = { 
    flags = {frag_normal = false; frag_drop = true; frag_reasm = true;} ;
    miss_send_len = 603 
  } in
  let frenetic_msg = Message.GetConfigReplyMsg config_reply in
  test_parse frenetic_msg "OfpGetConfigReply"

(******** OFPT_SET_CONFIG *)

let%test "OfpSetConfig Marshal" = 
  let config_rec = { 
    flags = {frag_normal = true; frag_drop = false; frag_reasm = false;} ;
    miss_send_len = 603 
  } in
  let frenetic_msg = Message.SetConfigMsg config_rec in
  test_marshal frenetic_msg "OfpSetConfig"

(******** OFPT_PACKET_IN *)

let%test "OfpPacketInBuffered Parse" = 
  let payload = "Hi mom!  This is a buffered packet in." in
  let packet_in_reply = { 
    pi_total_len = String.length payload
    ; pi_reason = InvalidTTL
    ; pi_table_id = 100
    ; pi_cookie = 0L
    ; pi_ofp_match = sample_pipeline_match
    ; pi_payload = Buffered (2348957l, Cstruct.of_string payload)
  } in
  let frenetic_msg = Message.PacketInMsg packet_in_reply in
  test_parse frenetic_msg "OfpPacketInBuffered"

let%test "OfpPacketInUnbuffered Parse" = 
  let payload = "Hi mom!  This is an unbuffered packet in." in
  let packet_in_reply = { 
    pi_total_len = String.length payload
    ; pi_reason = ExplicitSend
    ; pi_table_id = 200
    ; pi_cookie = 98374L
    ; pi_ofp_match = sample_pipeline_match
    ; pi_payload = NotBuffered (Cstruct.of_string payload)
  } in
  let frenetic_msg = Message.PacketInMsg packet_in_reply in
  test_parse frenetic_msg "OfpPacketInUnbuffered"

(******** OFPT_FLOW_REMOVED *)

let%test "OfpFlowRemoved Parse" = 
  let flow_removed_reply = { 
    cookie = 98374L
    ; priority = 8977
    ; reason = FlowHardTiemout
    ; table_id = 200
    ; duration_sec = 8127346l
    ; duration_nsec = 1213414l
    ; idle_timeout = ExpiresAfter 999
    ; hard_timeout = ExpiresAfter 9999
    ; packet_count = 872364012876751L
    ; byte_count = 198237501837540L
    ; oxm = sample_lotsa_matches
  } in
  let frenetic_msg = Message.FlowRemoved flow_removed_reply in
  test_parse frenetic_msg "OfpFlowRemoved"

(******** OFPT_PORT_STATUS *)

let%test "OfpPortStatus Parse" =
  let no_features = { 
    rate_10mb_hd = false; rate_10mb_fd = false; rate_100mb_hd = false; rate_100mb_fd = false;
    rate_1gb_hd = false; rate_1gb_fd = false; rate_10gb_fd = false; rate_40gb_fd = false;
    rate_100gb_fd = false; rate_1tb_fd = false; other = false; copper = false; fiber = false;
    autoneg = false; pause = false; pause_asym = false 
  } in 
  let port_status_reply = {
    reason = PortModify;
    desc = {
      port_no = 77l;
      hw_addr = 0x102030405060L;
      name = zero_pad 16 "Port 77";
      config = { port_down = true; no_recv = false; no_fwd = true; no_packet_in = false };
      state = { link_down = false; blocked = true; live = true };
      curr =  {no_features with rate_10mb_hd = true; rate_10gb_fd = true; copper = true };
      advertised = {no_features with rate_10mb_fd = true; rate_40gb_fd = true; fiber = true };
      supported = {no_features with rate_100mb_hd = true; rate_100gb_fd = true; autoneg = true}; 
      peer = {no_features with rate_1gb_hd = true; rate_1tb_fd = true; pause = true};
      curr_speed = 10000000l;
      max_speed =  100000000l
    }
  } in
  let frenetic_msg = Message.PortStatusMsg port_status_reply in
  test_parse frenetic_msg "OfpPortStatus"

(******** OFPT_PACKET_OUT *)

let%test "OfpPacketOutBuffered Marshal" = 
  let packet_out_request = {
    po_payload = Buffered (81349218l, Cstruct.of_string "");
    po_port_id = Some 987245l;
    po_actions = sample_single_action
  } in
  let frenetic_msg = Message.PacketOutMsg packet_out_request in
  test_marshal frenetic_msg "OfpPacketOutBuffered"

let%test "OfpPacketOutUnbuffered Marshal" = 
  let packet_out_request = { 
    po_payload = NotBuffered(Cstruct.of_string "") ;
    po_port_id = Some 987145l;
    po_actions = sample_lotsa_actions
  } in
  let frenetic_msg = Message.PacketOutMsg packet_out_request in
  test_marshal frenetic_msg "OfpPacketOutUnbuffered"

(******** OFPT_FLOW_MOD *)

let%test "OfPFlowModAddSingleAction Marshal" = 
  let flow_mod_add_request = { 
    mfCookie = val_to_mask 0x12754879L
    ; mfTable_id = 100
    ; mfCommand = AddFlow
    ; mfIdle_timeout = ExpiresAfter 0x0190
    ; mfHard_timeout = ExpiresAfter 0x0600
    ; mfPriority = 0x5678
    ; mfBuffer_id = None
    ; mfOut_port = None
    ; mfOut_group = None
    ; mfFlags = { 
      fmf_send_flow_rem = true ; fmf_check_overlap = false ; fmf_reset_counts = false
      ; fmf_no_pkt_counts = true ; fmf_no_byt_counts = false
    }
    ; mfOfp_match = sample_single_match
    ; mfInstructions = [ ApplyActions sample_single_action ]
  } in
  let frenetic_msg = Message.FlowModMsg flow_mod_add_request in
  test_marshal frenetic_msg "OfPFlowModAddSingleAction"

let%test "OfPFlowModAddMultiAction Marshal" = 
  let flow_mod_add_request = { 
    mfCookie = val_to_mask 0x12554879L
    ; mfTable_id = 200
    ; mfCommand = AddFlow
    ; mfIdle_timeout = Permanent
    ; mfHard_timeout = Permanent
    ; mfPriority = 0x5478
    ; mfBuffer_id = Some 0x87132l
    ; mfOut_port = None
    ; mfOut_group = None
    ; mfFlags = { 
      fmf_send_flow_rem = false ; fmf_check_overlap = true ; fmf_reset_counts = false
      ; fmf_no_pkt_counts = false ; fmf_no_byt_counts = true 
    }
    ; mfOfp_match = sample_lotsa_matches
    ; mfInstructions = [ ApplyActions sample_lotsa_actions ]
  } in
  let frenetic_msg = Message.FlowModMsg flow_mod_add_request in
  test_marshal frenetic_msg "OfPFlowModAddMultiAction"

let%test "OfPFlowModModify Marshal" = 
  let flow_mod_mod_request = { 
    mfCookie = { m_value = 0x12753838L; m_mask = Some 0xffffffffL }
    ; mfTable_id = 200
    ; mfCommand = ModStrictFlow
    ; mfIdle_timeout = ExpiresAfter 0x0191
    ; mfHard_timeout = ExpiresAfter 0x0601
    ; mfPriority = 0x5678
    ; mfBuffer_id = None
    ; mfOut_port = None
    ; mfOut_group = None
    ; mfFlags = { 
      fmf_send_flow_rem = false ; fmf_check_overlap = false ; fmf_reset_counts = true
      ; fmf_no_pkt_counts = false ; fmf_no_byt_counts = false
    }
    ; mfOfp_match = [ OxmTCPSrc(8000); ]
    ; mfInstructions = [ 
      GotoTable 200
      ; WriteMetadata {m_value = 2134987L; m_mask=Some 0xffffffffL }
      ; WriteActions sample_single_action
      ; Clear
      ; Meter 271l
    ]
  } in
  let frenetic_msg = Message.FlowModMsg flow_mod_mod_request in
  test_marshal frenetic_msg "OfPFlowModModify"

let%test "OfPFlowModDelete Marshal" = 
  let flow_mod_delete_request = { 
    mfCookie = { m_value = 0L; m_mask = None }
    ; mfTable_id = 0xff 
    ; mfCommand = DeleteFlow
    ; mfIdle_timeout = Permanent
    ; mfHard_timeout = Permanent
    ; mfPriority = 0
    ; mfBuffer_id = None
    ; mfOut_port = Some (PhysicalPort 0x921474l)
    ; mfOut_group = Some 0xffffffffl
    ; mfFlags = { 
      fmf_send_flow_rem = false ; fmf_check_overlap = false ; fmf_reset_counts = true
      ; fmf_no_pkt_counts = false ; fmf_no_byt_counts = false
    }
    ; mfOfp_match = [ OxmUDPSrc(800); ]
    ; mfInstructions = [ ]
  } in
  let frenetic_msg = Message.FlowModMsg flow_mod_delete_request in
  test_marshal frenetic_msg "OfPFlowModDelete"

(******** OFPT_GROUP_MOD *)

let%test "OfpGroupModAddNoActions Marshal" = 
  let group_mod_request = AddGroup (All, 391247l, []) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddNoActions"

let%test "OfpGroupModAddOneAction Marshal" = 
  let bucket = { 
    bu_weight = 0; 
    bu_watch_port = None;
    bu_watch_group = None; 
    bu_actions = sample_single_action
  } in
  let group_mod_request = AddGroup (Indirect, 321347l, [bucket]) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddOneAction"  

let%test "OfpGroupModAddSelect Marshal" = 
  let bucket1 = {bu_weight = 40; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let bucket2 = {bu_weight = 10; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let group_mod_request = AddGroup (Select, 121347l, [bucket1; bucket2]) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddSelect"    

let%test "OfpGroupModAddAll Marshal" = 
  let broadcast_bucket = [
    {bu_weight = 0; bu_watch_port = None; bu_watch_group = None; bu_actions = [Output(PhysicalPort(1l))] } ;
    {bu_weight = 0; bu_watch_port = None; bu_watch_group = None; bu_actions = [Output(PhysicalPort(2l))] } ;
    {bu_weight = 0; bu_watch_port = None; bu_watch_group = None; bu_actions = [Output(PhysicalPort(3l))] } ;
  ] in
  let group_mod_request = AddGroup (All, 121340l, broadcast_bucket) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddAll"

let%test "OfpGroupModAddFf Marshal" = 
  let ff_bucket = [
    {bu_weight = 0; bu_watch_port = Some 17l; bu_watch_group = Some 0l; bu_actions = [Output(PhysicalPort(1l))] } ;
    {bu_weight = 0; bu_watch_port = Some 18l; bu_watch_group = Some 0l; bu_actions = [Output(PhysicalPort(2l))] } ;
    {bu_weight = 0; bu_watch_port = Some 19l; bu_watch_group = Some 1l; bu_actions = [Output(PhysicalPort(3l))] } ;
  ] in
  let group_mod_request = AddGroup (FF, 205793l, ff_bucket) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddFf"      

let%test "OfpGroupModModify Marshal" = 
  let bucket1 = {bu_weight = 10; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let bucket2 = {bu_weight = 40; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let group_mod_request = ModifyGroup (Select, 121347l, [bucket1; bucket2]) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModModify"    

let%test "OfpGroupModDelete Marshal" = 
  let group_mod_request = DeleteGroup (All, 391247l) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModDelete"

(******** OFPT_PORT_MOD *)

let%test "OfpPortMod Marshal" = 
  let port_mod_request = { 
    mpPortNo = 77l
    ; mpHw_addr = 0x102030405060L
    ; mpConfig = { port_down = true; no_recv = false; no_fwd = true; no_packet_in = false }
    ; mpMask = 0xffl
    ; mpAdvertise = { 
        rate_10mb_hd = false; rate_10mb_fd = true; rate_100mb_hd = false; rate_100mb_fd = false;
        rate_1gb_hd = false; rate_1gb_fd = false; rate_10gb_fd = false; rate_40gb_fd = true;
        rate_100gb_fd = false; rate_1tb_fd = false; other = false; copper = false; fiber = true;
        autoneg = false; pause = false; pause_asym = false 
      }
  } in
  let frenetic_msg = Message.PortModMsg port_mod_request in
  test_marshal frenetic_msg "OfpPortMod"

(******** OFPT_TABLE_MOD *)

let%test "OfpTableMod Marshal" = 
  let table_mod_request = { table_id = 156; config = Deprecated } in
  let frenetic_msg = Message.TableModMsg table_mod_request in
  test_marshal frenetic_msg "OfpTableMod"

(******** OFPT_MULTIPART_REQUEST *)

let%test "OfpDescStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = SwitchDescReq ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpDescStatsRequest"

let%test "OfpFlowStatsRequest Marshal" = 
  let flow_stats_request = {
    fr_table_id = 199
    ; fr_out_port = 12325l
    ; fr_out_group = 9712346l
    ; fr_cookie = { m_value = 871625978634L; m_mask = Some 0xffffffffffffL }
    ; fr_match = sample_single_match
  } in
  let frenetic_msg = Message.MultipartReq { mpr_type = FlowStatsReq flow_stats_request ; mpr_flags = true } in
  test_marshal frenetic_msg "OfpFlowStatsRequest"

let%test "OfpAggregateStatsRequest Marshal" = 
  let aggregate_stats_request = {
    fr_table_id = 201
    ; fr_out_port = 12325l
    ; fr_out_group = 9712346l
    ; fr_cookie = { m_value = 871625978634L; m_mask = Some 0xffffffffffffL }
    ; fr_match = sample_single_match
  } in
  let frenetic_msg = Message.MultipartReq { mpr_type = AggregFlowStatsReq aggregate_stats_request ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpAggregateStatsRequest"

let%test "OfpTableStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = TableStatsReq ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpTableStatsRequest"

let%test "OfpPortStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = PortStatsReq 555l; mpr_flags = false } in
  test_marshal frenetic_msg "OfpPortStatsRequest"

let%test "OfpQueueStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = QueueStatsReq {port_number=565l; queue_id=192834l}; mpr_flags = false } in
  test_marshal frenetic_msg "OfpQueueStatsRequest"

let%test "OfpGroupStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = GroupStatsReq 5123456l; mpr_flags = false } in
  test_marshal frenetic_msg "OfpGroupStatsRequest"

let%test "OfpGroupDescStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = GroupDescReq ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpGroupDescStatsRequest"

let%test "OfpGroupFeaturesStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = GroupFeatReq ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpGroupFeaturesStatsRequest"

let%test "OfpMeterStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = MeterStatsReq 6234324l; mpr_flags = false } in
  test_marshal frenetic_msg "OfpMeterStatsRequest"

let%test "OfpMeterConfigStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = MeterConfReq 6234324l; mpr_flags = false } in
  test_marshal frenetic_msg "OfpMeterConfigStatsRequest"

let%test "OfpMeterFeaturesStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = MeterFeatReq ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpMeterFeaturesStatsRequest"

let%test "OfpTableFeaturesStatsRequest Marshal" = 
  let table_features_request = [
    {
      length = 0x48  (* You shouldn't have to specify this, but ... *)
      ; table_id = 1
      ; name = zero_pad 32 "Init Table"
      ; metadata_match = 0L
      ; metadata_write = 0L
      ; config = Deprecated
      ; max_entries = 10l
      ; feature_prop = sample_single_table_property
    };
    {
      length = 0xf0
      ; table_id = 100
      ; name = zero_pad 32 "ACL Table"
      ; metadata_match = 0xfffffL
      ; metadata_write = 0xffL
      ; config = Deprecated
      ; max_entries = 500l
      ; feature_prop = sample_lotsa_table_properties
    } 
  ] in 
  let frenetic_msg = Message.MultipartReq { 
    mpr_type = TableFeatReq (Some table_features_request); 
    mpr_flags = false 
  } in
  test_marshal frenetic_msg "OfpTableFeaturesStatsRequest"

let%test "OfpPortDescStatsRequest Marshal" = 
  let frenetic_msg = Message.MultipartReq { mpr_type = PortsDescReq ; mpr_flags = false } in
  test_marshal frenetic_msg "OfpPortDescStatsRequest"

(******** OFPT_MULTIPART_REPLY *)

let%test "OfpDescStatsReply Parse" = 
  let desc_stats_reply = { 
    mfr_desc = zero_pad 256 "Manufacturer Description"
    ; hw_desc = zero_pad 256 "Hardware Description"
    ; sw_desc = zero_pad 256 "Software Descriptiuon"
    ; serial_num = zero_pad 32 "0123456789-JHJH"
    ; dp_desc = zero_pad 256 "Dataplane Description"
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = SwitchDescReply desc_stats_reply; mpreply_flags = false} in
  test_parse frenetic_msg "OfpDescStatsReply"

let%test "OfpFlowStatsReply Parse" = 
  let flow_stats_reply = {
    table_id = 100
    ; duration_sec = 999l
    ; duration_nsec = 888l
    ; priority = 0x5678
    ; idle_timeout = ExpiresAfter 0x0190
    ; hard_timeout = ExpiresAfter 0x0600
    ; flags = { 
      fmf_send_flow_rem = true ; fmf_check_overlap = false ; fmf_reset_counts = false
      ; fmf_no_pkt_counts = true ; fmf_no_byt_counts = false
    } 
    ; cookie = 0x12754879L
    ; packet_count = 4000L
    ; byte_count = 3000L
    ; ofp_match = sample_single_match
    ; instructions = [ ApplyActions sample_single_action ]
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = FlowStatsReply [flow_stats_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpFlowStatsReply"

let%test "OfpAggregateStatsReply Parse" = 
  let aggregate_stats_reply = { 
    packet_count = 4000L;
    byte_count = 3000L;
    flow_count = 2000l
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = AggregateReply aggregate_stats_reply; mpreply_flags = false} in
  test_parse frenetic_msg "OfpAggregateStatsReply"

let%test "OfpTableStatsReply Parse" = 
  let table_stats_reply = { 
    table_id = 100;
    active_count = 600l;
    lookup_count = 2000L;
    matched_count = 666L
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = TableReply [table_stats_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpTableStatsReply"

let%test "OfpPortStatsReply Parse" = 
  let port_stats_reply = { 
    psPort_no = 574190793l;
    rx_packets = 1113204397L;
    tx_packets = 2702231185L;
    rx_bytes = 2451900840L;
    tx_bytes = 2654217578L;
    rx_dropped = 2311349152L;
    tx_dropped = 2340791430L;
    rx_errors = 1441457975L;
    tx_errors = 3861416712L;
    rx_frame_err = 3760794366L;
    rx_over_err = 3471122481L;
    rx_crc_err = 38255885L;
    collisions = 4183796980L;
    duration_sec = 327091l;
    duration_nsec = 417782l
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = PortStatsReply [port_stats_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpPortStatsReply"

let%test "OfpQueueStatsReply Parse" = 
  let queue_stats_reply = { 
    qsPort_no = 574190793l;
    queue_id = 98734l;
    tx_bytes = 2654217578L;
    tx_packets = 2702231185L;
    tx_errors = 3861416712L;
    duration_sec = 327091l;
    duration_nsec = 417782l
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = QueueStatsReply [queue_stats_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpQueueStatsReply"

let%test "OfpGroupStatsReply Parse" = 
  let group_stats_reply = { 
    length = 0x48;
    group_id = 37135343l;
    ref_count = 30334666l;
    packet_count = 16467336L;
    byte_count = 31159107L;
    duration_sec = 18179039l;
    duration_nsec = 36282180l;
    bucket_stats = [
      { packet_count = 3575169166L; byte_count = 2156878186L };
      { packet_count = 3664701344L; byte_count = 998359161L }
    ]
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = GroupStatsReply [group_stats_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpGroupStatsReply"

let%test "OfpGroupDescStatsReply Parse" = 
  let bucket = { 
    bu_weight = 0; 
    bu_watch_port = None;
    bu_watch_group = None; 
    bu_actions = sample_single_action
  } in
  let group_desc_reply = {
    length = 0x28; 
    typ = Select;
    group_id = 321347l;
    bucket = [ bucket ]
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = GroupDescReply [group_desc_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpGroupDescStatsReply"

let%test "OfpGroupFeaturesStatsReply Parse" = 
  let no_actions = { output = false; copy_ttl_out = false; copy_ttl_in = false;
       set_mpls_ttl = false; dec_mpls_ttl = false; push_vlan = false;
       pop_vlan = false; push_mpls = false; pop_mpls = false; set_queue = false;
       group = false; set_nw_ttl = false; dec_nw_ttl = false; set_field = false;
       push_pbb = false; pop_pbb = false } in
  let group_features_reply = {
    typ = { all = false; select = false; indirect = true; ff = true }
    ; capabilities = { select_weight = true; select_liveness = false;
                           chaining  = true; chaining_checks = false }
    ; max_groups_all = 100l
    ; max_groups_select = 0l
    ; max_groups_indirect = 200l
    ; max_groups_ff = 0l
    ; actions_all = { no_actions with group = true; pop_pbb = true }
    ; actions_select = no_actions
    ; actions_indirect = { no_actions with push_mpls = true; push_pbb = true }
    ; actions_ff = no_actions
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = GroupFeaturesReply group_features_reply; mpreply_flags = false} in
  test_parse frenetic_msg "OfpGroupFeaturesStatsReply"

let%test "OfpMeterStatsReply Parse" = 
  let meter_stats_reply = {
    meter_id = 356936l;
    len = 72;
    flow_count = 381305l;
    packet_in_count = 283995L;
    byte_in_count = 28555L;
    duration_sec = 382212l;
    duration_nsec = 139569l;
    band = [
      { packet_band_count = 137645L; byte_band_count = 330608L };
      { packet_band_count = 92874353L; byte_band_count = 1254987L }
    ]
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = MeterReply [meter_stats_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpMeterStatsReply"

let%test "OfpMeterConfigStatsReply Parse" = 
  let meter_config_reply = {
    meter_id = 19857l;
    length = 8;
    flags = { kbps = true; pktps = false; burst = true; stats = false };
    bands = [ ]
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = MeterConfig [meter_config_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpMeterConfigStatsReply"

let%test "OfpMeterFeaturesStatsReply Parse" = 
  let meter_features_reply = {
    max_meter = 987234l;
    band_typ = { drop = true; dscpRemark = true };
    capabilities = { kbps = true; pktps = false; burst = true; stats = false };
    max_band = 100;
    max_color = 200
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = MeterFeaturesReply meter_features_reply; mpreply_flags = false} in
  test_parse frenetic_msg "OfpMeterFeaturesStatsReply"

let%test "OfpTableFeaturesStatsReply Marshal" = 
  let table_features_reply = [
    {
      length = 0x48  
      ; table_id = 1
      ; name = zero_pad 32 "Init Table"
      ; metadata_match = 0L
      ; metadata_write = 0L
      ; config = Deprecated
      ; max_entries = 10l
      ; feature_prop = sample_single_table_property
    }
  ] in 
  let frenetic_msg = Message.MultipartReply { 
    mpreply_typ = TableFeaturesReply table_features_reply; 
    mpreply_flags = false 
  } in
  test_parse frenetic_msg "OfpTableFeaturesStatsReply"

let%test "OfpPortDescStatsReply Parse" =
  let no_features = { 
    rate_10mb_hd = false; rate_10mb_fd = false; rate_100mb_hd = false; rate_100mb_fd = false;
    rate_1gb_hd = false; rate_1gb_fd = false; rate_10gb_fd = false; rate_40gb_fd = false;
    rate_100gb_fd = false; rate_1tb_fd = false; other = false; copper = false; fiber = false;
    autoneg = false; pause = false; pause_asym = false 
  } in 
  let port_desc_reply = {
    port_no = 77l;
    hw_addr = 0x102030405060L;
    name = zero_pad 16 "Port 77";
    config = { port_down = true; no_recv = false; no_fwd = true; no_packet_in = false };
    state = { link_down = false; blocked = true; live = true };
    curr = { no_features with rate_10mb_hd = true; rate_10gb_fd = true; copper = true };
    advertised = { no_features with rate_10mb_fd = true; rate_40gb_fd = true; fiber = true };
    supported = { no_features with rate_100mb_hd = true; rate_100gb_fd = true; autoneg = true }; 
    peer = { no_features with rate_1gb_hd = true; rate_1tb_fd = true; pause = true };
    curr_speed = 10000000l;
    max_speed =  100000000l
  } in
  let frenetic_msg = Message.MultipartReply {mpreply_typ = PortsDescReply [port_desc_reply]; mpreply_flags = false} in
  test_parse frenetic_msg "OfpPortDescStatsReply"

(******** OFPT_BARRIER_REQUEST *)

let%test "OfpBarrierRequest Marshal" = 
  let frenetic_msg = Message.BarrierRequest in
  test_marshal frenetic_msg "OfpBarrierRequest"

(******** OFPT_BARRIER_REPLY *)

let%test "OfpBarrierReply Parse" = 
  let frenetic_msg = Message.BarrierReply in
  test_parse frenetic_msg "OfpBarrierReply"

(******** OFPT_ROLE_REQUEST *)

let%test "OfpRoleRequest Marshal" = 
  let frenetic_msg = Message.RoleRequest { role = EqualRole; generation_id = 92580291354L } in
  test_marshal frenetic_msg "OfpRoleRequest"

(******** OFPT_ROLE_REPLY *)

let%test "OfpRoleReply Parse" = 
  let frenetic_msg = Message.RoleReply { role = SlaveRole; generation_id = 92581791354L } in
  test_parse frenetic_msg "OfpRoleReply"

(******** OFPT_GET_CONFIG_REQUEST *)

let%test "OfpQueueGetConfigRequest Marshal" = 
  let frenetic_msg = Message.QueueGetConfigReq { port = 2387456l } in
  test_marshal frenetic_msg "OfpQueueGetConfigRequest"

(******** OFPT_GET_CONFIG_REPLY *)

let%test "OfpQueueGetConfigReply Parse" = 
  let frenetic_msg = Message.QueueGetConfigReply { 
    port = 2387456l
    ; queues = [
      { queue_id = 2134l; port = 2387456l; len = 48; properties = [ MinRateProp (Rate 20); MaxRateProp (Rate 46) ] };
      { queue_id = 284570349l; port = 2387456l; len = 48; properties = [ MinRateProp (Rate 33); MaxRateProp Disabled ] }
    ]
  } in
  test_parse frenetic_msg "OfpQueueGetConfigReply"

(******** OFPT_GET_ASYNC_REQUEST *)

let%test "OfpGetAsyncRequest Marshal" = 
  let frenetic_msg = Message.GetAsyncRequest in
  test_marshal frenetic_msg "OfpGetAsyncRequest"

(******** OFPT_GET_ASYNC_REPLY *)

let%test "OfpGetAsyncReply Parse" = 
  let get_async_reply = { 
    packet_in = { 
      m_master = { table_miss = true; apply_action = false; invalid_ttl = false }
      ; m_slave = { table_miss = false; apply_action = true; invalid_ttl = true }
    }
    ; port_status = {
      m_master = { add = true; delete = false; modify = true }
      ; m_slave = { add = false; delete = true; modify = false }
    }
    ; flow_removed = { 
      m_master = { idle_timeout = true; hard_timeout = false; delete = true ; group_delete = false }
      ; m_slave = { idle_timeout = false; hard_timeout = true; delete = false ; group_delete = true }
    } 
  } in
  let frenetic_msg = Message.GetAsyncReply get_async_reply in
  test_parse frenetic_msg "OfpGetAsyncReply"

(******** OFPT_SET_ASYNC *)

let%test "OfpSetAsync Marshal" = 
  let set_async = { 
    packet_in = { 
      m_master = { table_miss = false; apply_action = true; invalid_ttl = true }
      ; m_slave = { table_miss = true; apply_action = false; invalid_ttl = false }
    }
    ; port_status = {
      m_master = { add = false; delete = true; modify = false }
      ; m_slave = { add = true; delete = false; modify = true }
    }
    ; flow_removed = { 
      m_master = { idle_timeout = false; hard_timeout = true; delete = false ; group_delete = true }
      ; m_slave = { idle_timeout = true; hard_timeout = false; delete = true ; group_delete = false }
    } 
  } in
  let frenetic_msg = Message.SetAsync set_async in
  test_marshal frenetic_msg "OfpSetAsync"

(******** OFPT_METER_MOD *)

let%test "OfpMeterMod Marshal" = 
  let meter_mod = { 
    command = AddMeter
    ; flags = { kbps = true; pktps = false; burst = true; stats = false }
    ; meter_id = 19857l
    ; bands = [
      Drop (187236l, 4345234l)
      ; DscpRemark (234214l, 2359834l, 66)
    ]
  } in
  let frenetic_msg = Message.MeterModMsg meter_mod in
  test_marshal frenetic_msg "OfpMeterMod"
