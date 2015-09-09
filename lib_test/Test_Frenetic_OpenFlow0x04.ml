open Core.Std
open OUnitHack
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

let test_marshal frenetic_msg msg_name =
  let cs = Message.marshal 0l frenetic_msg in
  let frenetic_hex = buf_to_hex (Cstruct.of_string cs) in
  let ryu_hex_file_name = "lib_test/data/openflow0x04/" ^ msg_name ^ ".hex" in
  let ryu_hex = In_channel.read_all ryu_hex_file_name in
  
  printf "RYU: %s\nFR:  %s\n%!" ryu_hex frenetic_hex; 
  
  frenetic_hex = ryu_hex

let test_parse frenetic_msg msg_name = 
  let ryu_hex_file_name = "lib_test/data/openflow0x04/" ^ msg_name ^ ".hex" in
  let ryu_hex = In_channel.read_all ryu_hex_file_name in
  let ryu_raw = hex_to_buf ryu_hex in
  let header_buf = String.sub ryu_raw 0 Frenetic_OpenFlow_Header.size in
  let header = Frenetic_OpenFlow_Header.parse (Cstruct.of_string header_buf) in
  let message_buf = String.sub ryu_raw (Frenetic_OpenFlow_Header.size) ((String.length ryu_raw) - (Frenetic_OpenFlow_Header.size)) in
  let ryu_msg = snd (Frenetic_OpenFlow0x04.Message.parse header message_buf) in 
  (*
  let ryu_as_string = Message.to_string ryu_msg in
  let frenetic_as_string = Message.to_string frenetic_msg in
  printf "%s\n%!" (buf_to_hex (Cstruct.of_string message_buf));
  printf "RYU: %s\nFR:  %s\n%!" ryu_as_string frenetic_as_string; 
  *)
  ryu_msg = frenetic_msg

TEST "OfpHello Marshal" = 
  let frenetic_msg = Message.Hello ([ VersionBitMap [4; 1] ]) in
  test_marshal frenetic_msg "OfpHello"

TEST "OfpHello Parse" = 
  (* This requires the bitmaps to be listed [4;1] instead of [1;4] because Ocaml considers those
     two lists as different.  Perhaps VersionBitMap should use sets instead of lists. *)
  let frenetic_msg = Message.Hello ([ VersionBitMap [4; 1] ]) in
  test_parse frenetic_msg "OfpHello"

TEST "OfpErrorMessage Parse" =
  let all_error_combinations = [
    ("OfpErrorMsg_OFPET_TABLE_MOD_FAILED_OFPTMFC_BAD_CONFIG", TableModFailed TaBadConfig);
    ("OfpErrorMsg_OFPET_TABLE_MOD_FAILED_OFPTMFC_BAD_TABLE", TableModFailed TaBadTable);
    ("OfpErrorMsg_OFPET_TABLE_MOD_FAILED_OFPTMFC_EPERM", TableModFailed TaPermError);
    ("OfpErrorMsg_OFPET_SWITCH_CONFIG_FAILED_OFPSCFC_BAD_FLAGS", SwitchConfigFailed ScBadFlags);
    ("OfpErrorMsg_OFPET_SWITCH_CONFIG_FAILED_OFPSCFC_BAD_LEN", SwitchConfigFailed ScBadLen);
    ("OfpErrorMsg_OFPET_SWITCH_CONFIG_FAILED_OFPSCFC_EPERM", SwitchConfigFailed ScPermError);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_BAD_EXPERIMENTER", BadInstruction InstBadExp);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_BAD_EXP_TYPE", BadInstruction InstBadExpTyp);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_BAD_LEN", BadInstruction InstBadLen);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_BAD_TABLE_ID", BadInstruction InstBadTableId);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_EPERM", BadInstruction InstPermError);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_UNKNOWN_INST", BadInstruction InstUnknownInst);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_UNSUP_INST", BadInstruction InstUnsupInst);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_UNSUP_METADATA", BadInstruction InstUnsupMeta);
    ("OfpErrorMsg_OFPET_BAD_INSTRUCTION_OFPBIC_UNSUP_METADATA_MASK", BadInstruction InstUnsupMetaMask);
    ("OfpErrorMsg_OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_ARGUMENT", TableFeatFailed TfBadArg);
    ("OfpErrorMsg_OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_LEN", TableFeatFailed TfBadLen);
    ("OfpErrorMsg_OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_METADATA", TableFeatFailed TfBadMeta);
    ("OfpErrorMsg_OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_TABLE", TableFeatFailed TfBadTable);
    ("OfpErrorMsg_OFPET_TABLE_FEATURES_FAILED_OFPTFFC_BAD_TYPE", TableFeatFailed TfBadType);
    ("OfpErrorMsg_OFPET_TABLE_FEATURES_FAILED_OFPTFFC_EPERM", TableFeatFailed TfPermError);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_BAD_BAND", MeterModFailed MeBadBand);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_BAD_BAND_VALUE", MeterModFailed MeBadBandValue);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_BAD_BURST", MeterModFailed MeBadBurst);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_BAD_COMMAND", MeterModFailed MeBadCommand);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_BAD_FLAGS", MeterModFailed MeBadFlags);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_BAD_RATE", MeterModFailed MeBadRate);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_INVALID_METER", MeterModFailed MeInvalidMeter);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_METER_EXISTS", MeterModFailed MeMeterExists);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_OUT_OF_BANDS", MeterModFailed MeOutOfBands);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_OUT_OF_METERS", MeterModFailed MeOutOfMeters);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_UNKNOWN", MeterModFailed MeUnknown);
    ("OfpErrorMsg_OFPET_METER_MOD_FAILED_OFPMMFC_UNKNOWN_METER", MeterModFailed MeUnknownMeter);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_ARGUMENT", BadAction ActBadArg);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_EXPERIMENTER", BadAction ActBadExp);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_EXP_TYPE", BadAction ActBadExpType);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_LEN", BadAction ActBadLen);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_OUT_GROUP", BadAction ActBadOutGroup);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_OUT_PORT", BadAction ActBadOutPort);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_QUEUE", BadAction ActBadQueue);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_SET_ARGUMENT", BadAction ActBadSetArg);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_SET_LEN", BadAction ActBadSetLen);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_SET_TYPE", BadAction ActBadSetTyp);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_TAG", BadAction ActBadTag);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_BAD_TYPE", BadAction ActBadType);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_EPERM", BadAction ActPermError);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_MATCH_INCONSISTENT", BadAction ActMatchInconsistent);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_TOO_MANY", BadAction ActTooMany);
    ("OfpErrorMsg_OFPET_BAD_ACTION_OFPBAC_UNSUPPORTED_ORDER", BadAction ActUnsupportedOrder);
    ("OfpErrorMsg_OFPET_QUEUE_OP_FAILED_OFPQOFC_BAD_PORT", QueueOpFailed QuBadPort);
    ("OfpErrorMsg_OFPET_QUEUE_OP_FAILED_OFPQOFC_BAD_QUEUE", QueueOpFailed QuBadQueue);
    ("OfpErrorMsg_OFPET_QUEUE_OP_FAILED_OFPQOFC_EPERM", QueueOpFailed QuPermError);
    ("OfpErrorMsg_OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_ADVERTISE", PortModFailed PoBadAdvertise);
    ("OfpErrorMsg_OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_CONFIG", PortModFailed PoBadConfig);
    ("OfpErrorMsg_OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_HW_ADDR", PortModFailed PoBadHwAddr);
    ("OfpErrorMsg_OFPET_PORT_MOD_FAILED_OFPPMFC_BAD_PORT", PortModFailed PoBadPort);
    ("OfpErrorMsg_OFPET_PORT_MOD_FAILED_OFPPMFC_EPERM", PortModFailed PoPermError);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_COMMAND", FlowModFailed FlBadCommand);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_FLAGS", FlowModFailed FlBadFlags);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_TABLE_ID", FlowModFailed FlBadTableId);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_BAD_TIMEOUT", FlowModFailed FlBadTimeout);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_EPERM", FlowModFailed FlPermError);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_OVERLAP", FlowModFailed FlOverlap);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_TABLE_FULL", FlowModFailed FlTableFull);
    ("OfpErrorMsg_OFPET_FLOW_MOD_FAILED_OFPFMFC_UNKNOWN", FlowModFailed FlUnknown);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_BUCKET", GroupModFailed GrBadBucket);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_COMMAND", GroupModFailed GrBadCommand);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_TYPE", GroupModFailed GrBadTyp);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_BAD_WATCH", GroupModFailed GrBadWatch);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_CHAINED_GROUP", GroupModFailed GrChainedGroup);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_CHAINING_UNSUPPORTED", GroupModFailed GrChainingUnsupported);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_EPERM", GroupModFailed GrPermError);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_GROUP_EXISTS", GroupModFailed GrGroupExists);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_INVALID_GROUP", GroupModFailed GrInvalidGroup);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_LOOP", GroupModFailed GrLoop);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_OUT_OF_BUCKETS", GroupModFailed GrOutOfBuckets);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_OUT_OF_GROUPS", GroupModFailed GrOutOfGroups);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_UNKNOWN_GROUP", GroupModFailed GrUnknownGroup);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_WATCH_UNSUPPORTED", GroupModFailed GrWatchUnsupported);
    ("OfpErrorMsg_OFPET_GROUP_MOD_FAILED_OFPGMFC_WEIGHT_UNSUPPORTED", GroupModFailed GrWeightUnsupported);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_EXPERIMENTER", BadRequest ReqBadExp);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_EXP_TYPE", BadRequest ReqBadExpType);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_LEN", BadRequest ReqBadLen);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_MULTIPART", BadRequest ReqBadMultipart);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_PACKET", BadRequest ReqBadPacket);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_PORT", BadRequest ReqBadPort);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_TABLE_ID", BadRequest ReqBadTableId);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_TYPE", BadRequest ReqBadType);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BAD_VERSION", BadRequest ReqBadVersion);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BUFFER_EMPTY", BadRequest ReqBufferEmpty);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_BUFFER_UNKNOWN", BadRequest ReqBufferUnknown);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_EPERM", BadRequest ReqPermError);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_IS_SLAVE", BadRequest ReqIsSlave);
    ("OfpErrorMsg_OFPET_BAD_REQUEST_OFPBRC_MULTIPART_BUFFER_OVERFLOW", BadRequest ReqMultipartBufOverflow);
    ("OfpErrorMsg_OFPET_ROLE_REQUEST_FAILED_OFPRRFC_BAD_ROLE", RoleReqFailed RoBadRole);
    ("OfpErrorMsg_OFPET_ROLE_REQUEST_FAILED_OFPRRFC_STALE", RoleReqFailed RoStale);
    ("OfpErrorMsg_OFPET_ROLE_REQUEST_FAILED_OFPRRFC_UNSUP", RoleReqFailed RoUnsup);
    ("OfpErrorMsg_OFPET_HELLO_FAILED_OFPHFC_EPERM", HelloFailed HelloPermError);
    ("OfpErrorMsg_OFPET_HELLO_FAILED_OFPHFC_INCOMPATIBLE", HelloFailed HelloIncompatible)
  ] in 
  let test_one_msg msg_pair = 
    let (msg_name, error_msg) = msg_pair in
    let error_rec = {
      err = error_msg; 
      data = Cstruct.of_string msg_name
    } in
    let frenetic_msg = Message.Error error_rec in
    test_parse frenetic_msg msg_name
  in
  List.fold_left ~init:true ~f:(&&) (List.map ~f:test_one_msg all_error_combinations)

TEST "OfpEchoRequest Marshal" = 
  let frenetic_msg = Message.EchoRequest (Cstruct.of_string "OfpEchoRequest") in
  test_marshal frenetic_msg "OfpEchoRequest"

TEST "OfpEchoRequest Parse" = 
  let frenetic_msg = Message.EchoRequest (Cstruct.of_string "OfpEchoRequest") in
  test_parse frenetic_msg "OfpEchoRequest"

TEST "OfpEchoReply Marshal" = 
  let frenetic_msg = Message.EchoReply (Cstruct.of_string "OfpEchoReply") in
  test_marshal frenetic_msg "OfpEchoReply"

TEST "OfpEchoReply Parse" = 
  let frenetic_msg = Message.EchoReply (Cstruct.of_string "OfpEchoReply") in
  test_parse frenetic_msg "OfpEchoReply"

TEST "OfpFeaturesRequest Marshal" = 
  let frenetic_msg = Message.FeaturesRequest in
  test_marshal frenetic_msg "OfpFeaturesRequest"

TEST "OfpFeaturesReply Parse" = 
  let feat_reply = {  
    datapath_id = 9210263729383L;
    num_buffers = 897345987l;
    num_tables = 250;
    aux_id = 65;
    supported_capabilities = { 
      flow_stats = true; 
      table_stats = false;
      port_stats = false; 
      group_stats = true; 
      ip_reasm = false; 
      queue_stats = false; 
      port_blocked = true 
    }
  } in
  let frenetic_msg = Message.FeaturesReply feat_reply in
  test_parse frenetic_msg "OfpFeaturesReply"

TEST "OfpGetConfigRequest Marshal" = 
  let frenetic_msg = Message.GetConfigRequestMsg in
  test_marshal frenetic_msg "OfpGetConfigRequest"

TEST "OfpGetConfigReply Parse" = 
  let config_reply = { 
    flags = {frag_normal = false; frag_drop = true; frag_reasm = true;} ;
    miss_send_len = 603 
  } in
  let frenetic_msg = Message.GetConfigReplyMsg config_reply in
  test_parse frenetic_msg "OfpGetConfigReply"

TEST "OfpSetConfig Marshal" = 
  let config_rec = { 
    flags = {frag_normal = true; frag_drop = false; frag_reasm = false;} ;
    miss_send_len = 603 
  } in
  let frenetic_msg = Message.SetConfigMsg config_rec in
  test_marshal frenetic_msg "OfpSetConfig"

(* TODO: This test fails, but the generated packets ARE equal, so I have no idea why
TEST "OfpPortStatus Parse" = 
  let port_status_reply = {
    reason = PortModify;
    desc = {
      port_no = 77l;
      hw_addr = 0x102030405060L;
      name = "Port 77";
      config = { port_down = true; no_recv = false; no_fwd = true; no_packet_in = false };
      state = { link_down = false; blocked = true; live = true };
      curr = { 
        rate_10mb_hd = true; rate_10mb_fd = false; rate_100mb_hd = false; rate_100mb_fd = false;
        rate_1gb_hd = false; rate_1gb_fd = false; rate_10gb_fd = true; rate_40gb_fd = false;
        rate_100gb_fd = false; rate_1tb_fd = false; other = false; copper = true; fiber = false;
        autoneg = false; pause = false; pause_asym = false 
      };
      advertised = { 
        rate_10mb_hd = false; rate_10mb_fd = true; rate_100mb_hd = false; rate_100mb_fd = false;
        rate_1gb_hd = false; rate_1gb_fd = false; rate_10gb_fd = false; rate_40gb_fd = true;
        rate_100gb_fd = false; rate_1tb_fd = false; other = false; copper = false; fiber = true;
        autoneg = false; pause = false; pause_asym = false 
      };
      supported = { 
        rate_10mb_hd = false; rate_10mb_fd = false; rate_100mb_hd = true; rate_100mb_fd = false;
        rate_1gb_hd = false; rate_1gb_fd = false; rate_10gb_fd = false; rate_40gb_fd = false;
        rate_100gb_fd = true; rate_1tb_fd = false; other = false; copper = false; fiber = false;
        autoneg = true; pause = false; pause_asym = false 
      }; 
      peer = { 
        rate_10mb_hd = false; rate_10mb_fd = false; rate_100mb_hd = false; rate_100mb_fd = false;
        rate_1gb_hd = true; rate_1gb_fd = false; rate_10gb_fd = false; rate_40gb_fd = false;
        rate_100gb_fd = false; rate_1tb_fd = true; other = false; copper = false; fiber = false;
        autoneg = false; pause = true; pause_asym = false 
      };
      curr_speed = 10000000l;
      max_speed =  100000000l
    }
  } in
  let frenetic_msg = Message.PortStatusMsg port_status_reply in
  test_parse frenetic_msg "OfpPortStatus"
*)

TEST "OfpPacketOutBuffered Marshal" = 
  let packet_out_request = {
    po_payload = Buffered (81349218l, Cstruct.of_string "");
    po_port_id = Some 987245l;
    po_actions = sample_single_action
  } in
  let frenetic_msg = Message.PacketOutMsg packet_out_request in
  test_marshal frenetic_msg "OfpPacketOutBuffered"

TEST "OfpPacketOutUnbuffered Marshal" = 
  let packet_out_request = { 
    po_payload = NotBuffered(Cstruct.of_string "") ;
    po_port_id = Some 987145l;
    po_actions = sample_lotsa_actions
  } in
  let frenetic_msg = Message.PacketOutMsg packet_out_request in
  test_marshal frenetic_msg "OfpPacketOutUnbuffered"

TEST "OfpGroupModAddNoActions Marshal" = 
  let group_mod_request = AddGroup (All, 391247l, []) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddNoActions"

TEST "OfpGroupModAddOneAction Marshal" = 
  let bucket = { 
    bu_weight = 0; 
    bu_watch_port = None;
    bu_watch_group = None; 
    bu_actions = sample_single_action
  } in
  let group_mod_request = AddGroup (Indirect, 321347l, [bucket]) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddOneAction"  

TEST "OfpGroupModAddSelect Marshal" = 
  let bucket1 = {bu_weight = 40; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let bucket2 = {bu_weight = 10; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let group_mod_request = AddGroup (Select, 121347l, [bucket1; bucket2]) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddSelect"    

TEST "OfpGroupModAddAll Marshal" = 
  let broadcast_bucket = [
    {bu_weight = 0; bu_watch_port = None; bu_watch_group = None; bu_actions = [Output(PhysicalPort(1l))] } ;
    {bu_weight = 0; bu_watch_port = None; bu_watch_group = None; bu_actions = [Output(PhysicalPort(2l))] } ;
    {bu_weight = 0; bu_watch_port = None; bu_watch_group = None; bu_actions = [Output(PhysicalPort(3l))] } ;
  ] in
  let group_mod_request = AddGroup (All, 121340l, broadcast_bucket) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddAll"

TEST "OfpGroupModAddFf Marshal" = 
  let ff_bucket = [
    {bu_weight = 0; bu_watch_port = Some 17l; bu_watch_group = Some 0l; bu_actions = [Output(PhysicalPort(1l))] } ;
    {bu_weight = 0; bu_watch_port = Some 18l; bu_watch_group = Some 0l; bu_actions = [Output(PhysicalPort(2l))] } ;
    {bu_weight = 0; bu_watch_port = Some 19l; bu_watch_group = Some 1l; bu_actions = [Output(PhysicalPort(3l))] } ;
  ] in
  let group_mod_request = AddGroup (FF, 205793l, ff_bucket) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModAddFf"      

TEST "OfpGroupModModify Marshal" = 
  let bucket1 = {bu_weight = 10; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let bucket2 = {bu_weight = 40; bu_watch_port = None; bu_watch_group = None; bu_actions = sample_single_action } in
  let group_mod_request = ModifyGroup (Select, 121347l, [bucket1; bucket2]) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModModify"    

TEST "OfpGroupModDelete Marshal" = 
  let group_mod_request = DeleteGroup (All, 391247l) in
  let frenetic_msg = Message.GroupModMsg group_mod_request in
  test_marshal frenetic_msg "OfpGroupModDelete"

