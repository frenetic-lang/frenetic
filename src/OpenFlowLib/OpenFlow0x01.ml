open Frenetic_Bit
open Packet
open Format
open OpenFlow0x01_Core

let sum (lst : int list) = List.fold_left (fun x y -> x + y) 0 lst

cenum ofp_stats_types {
  OFPST_DESC;
  OFPST_FLOW;
  OFPST_AGGREGATE;
  OFPST_TABLE;
  OFPST_PORT;
  OFPST_QUEUE;
  OFPST_VENDOR = 0xffff
} as uint16_t


module PortDescription = struct

  module PortConfig = struct

    type t =
      { down : bool (* Port is administratively down. *)
      ; no_stp : bool (* Disable 802.1D spanning tree on port. *)
      ; no_recv : bool (* Drop all packets except 802.1D spanning
                                 * tree packets. *)
      ; no_recv_stp : bool (* Drop received 802.1D STP packets. *)
      ; no_flood : bool (* Do not include this port when flooding. *)
      ; no_fwd : bool (* Drop packets forwarded to port. *)
      ; no_packet_in : bool (* Do not send packet-in msgs for port. *)
      }

    let to_string c = Printf.sprintf
      "{ down = %B; \
         no_stp = %B; \
         no_recv = %B; \
         no_recv_stp = %B; \
         no_flood = %B; \
         no_fwd = %B; \
         no_packet_in = %B }"
      c.down
      c.no_stp
      c.no_recv
      c.no_recv_stp
      c.no_flood
      c.no_fwd
      c.no_packet_in

    let of_int d =
      { down = test_bit 0 d;
        no_stp = test_bit 1 d;
        no_recv = test_bit 2 d;
        no_recv_stp = test_bit 3 d;
        no_flood = test_bit 4 d;
        no_fwd = test_bit 5 d;
        no_packet_in = test_bit 6 d
      }

    let size_of _ = 4

  end

  module PortState = struct

    type t =
      { down : bool  (* No physical link present. *)
      ; stp_listen : bool
      ; stp_forward : bool
      ; stp_block : bool }

    let to_string p = Printf.sprintf
      "{ down = %B; \
         stp_listen = %B; \
         stp_forward = %B; \
         stp_block = %B }"
      p.down
      p.stp_listen
      p.stp_forward
      p.stp_block

    (* MJR: GAH, the enum values from OF1.0 make NO SENSE AT ALL. Two of
       them have the SAME value, and the rest make no sense as bit
       vectors. Only portStateDown is parsed correctly ATM *)
    let of_int d =
      { down = test_bit 0 d
      ; stp_listen = false
      ; stp_forward = false
      ; stp_block = false }

    let size_of _ = 4

  end

  module PortFeatures = struct

    type t =
      { f_10MBHD : bool (* 10 Mb half-duplex rate support. *)
      ; f_10MBFD : bool (* 10 Mb full-duplex rate support. *)
      ; f_100MBHD : bool (* 100 Mb half-duplex rate support. *)
      ; f_100MBFD : bool (* 100 Mb full-duplex rate support. *)
      ; f_1GBHD : bool (* 1 Gb half-duplex rate support. *)
      ; f_1GBFD : bool (* 1 Gb full-duplex rate support. *)
      ; f_10GBFD : bool (* 10 Gb full-duplex rate support. *)
      ; copper : bool (* Copper medium. *)
      ; fiber : bool (* Fiber medium. *)
      ; autoneg : bool (* Auto-negotiation. *)
      ; pause : bool (* Pause. *)
      ; pause_asym : bool (* Asymmetric pause. *)
      }

    let to_string p = Printf.sprintf
      "{ f_10MBHD = %B; \
         f_10MBFD = %B; \
         f_100MBHD = %B; \
         f_100MBFD = %B; \
         f_1GBHD = %B; \
         f_1GBFD = %B; \
         f_10GBFD = %B; \
         copper = %B; \
         fiber = %B; \
         autoneg = %B; \
         pause = %B; \
         pause_asym = %B }"
      p.f_10MBHD
      p.f_10MBFD
      p.f_100MBHD
      p.f_100MBFD
      p.f_1GBHD
      p.f_1GBFD
      p.f_10GBFD
      p.copper
      p.fiber
      p.autoneg
      p.pause
      p.pause_asym

    let size_of _ = 4

    let of_int bits =
      { f_10MBHD = test_bit 0 bits
      ; f_10MBFD = test_bit 1 bits
      ; f_100MBHD = test_bit 2 bits
      ; f_100MBFD = test_bit 3 bits
      ; f_1GBHD = test_bit 4 bits
      ; f_1GBFD = test_bit 5 bits
      ; f_10GBFD = test_bit 6 bits
      ; copper = test_bit 7 bits
      ; fiber = test_bit 8 bits
      ; autoneg = test_bit 9 bits
      ; pause = test_bit 10 bits
      ; pause_asym = test_bit 11 bits }

  end

  type t =
    { port_no : portId
    ; hw_addr : dlAddr
    ; name : string
    ; config : PortConfig.t
    ; state : PortState.t
    ; curr : PortFeatures.t
    ; advertised : PortFeatures.t
    ; supported : PortFeatures.t
    ; peer : PortFeatures.t }

  cstruct ofp_phy_port {
    uint16_t port_no;
    uint8_t hw_addr[6];
    uint8_t name[16]; (* OFP_MAX_PORT_NAME_LEN, Null-terminated *)
    uint32_t config; (* Bitmap of OFPPC_* flags. *)
    uint32_t state; (* Bitmap of OFPPS_* flags. *)
    (* Bitmaps of OFPPF_* that describe features. All bits zeroed if
     * unsupported or unavailable. *)
    uint32_t curr; (* Current features. *)
    uint32_t advertised; (* SwitchFeatures being advertised by the port. *)
    uint32_t supported; (* SwitchFeatures supported by the port. *)
    uint32_t peer (* SwitchFeatures advertised by peer. *)
  } as big_endian

  let to_string d = Printf.sprintf
    "{ port_no = %s; hw_addr = %s; name = %s; config = %s; state = %s; \
       curr = %s; advertised = %s; supported = %s; peer = %s }"
    (string_of_portId d.port_no)
    (string_of_dlAddr d.hw_addr)
    d.name
    (PortConfig.to_string d.config)
    (PortState.to_string d.state)
    (PortFeatures.to_string d.curr)
    (PortFeatures.to_string d.advertised)
    (PortFeatures.to_string d.supported)
    (PortFeatures.to_string d.peer)

  let parse (bits : Cstruct.t) : t =
    let portDescPortNo = get_ofp_phy_port_port_no bits in
    let hw_addr = Packet.mac_of_bytes (Cstruct.to_string (get_ofp_phy_port_hw_addr bits)) in
    let name = Cstruct.to_string (get_ofp_phy_port_name bits) in
    let config = PortConfig.of_int (get_ofp_phy_port_config bits) in
    let state = PortState.of_int (get_ofp_phy_port_state bits) in
    let curr = PortFeatures.of_int (get_ofp_phy_port_curr bits) in
    let advertised = PortFeatures.of_int (get_ofp_phy_port_advertised bits) in
    let supported = PortFeatures.of_int (get_ofp_phy_port_supported bits) in
    let peer = PortFeatures.of_int (get_ofp_phy_port_peer bits) in
    { port_no = portDescPortNo
    ; hw_addr = hw_addr
    ; name = name
    ; config = config
    ; state = state
    ; curr = curr
    ; advertised = advertised
    ; supported = supported
    ; peer = peer }

  let size_of _ = sizeof_ofp_phy_port

end

module PortStatus = struct

  module ChangeReason = struct

    type t =
      | Add
      | Delete
      | Modify

    cenum ofp_port_reason {
      OFPPR_ADD;
      OFPPR_DELETE;
      OFPPR_MODIFY
    } as uint8_t

    let of_int d = 
      let reason_code = int_to_ofp_port_reason d in
      match reason_code with
      | Some OFPPR_ADD -> Add
      | Some OFPPR_DELETE -> Delete
      | Some OFPPR_MODIFY -> Modify
      | None ->
        raise (Unparsable
          (Printf.sprintf "unexpected ofp_port_reason %d" d))

    let to_int reason = 
      let reason_code = match reason with
        | Add -> OFPPR_ADD
        | Delete -> OFPPR_DELETE
        | Modify -> OFPPR_MODIFY
        in
      ofp_port_reason_to_int reason_code

    let to_string reason = match reason with
        | Add -> "Add"
        | Delete -> "Delete"
        | Modify -> "Modify"

    let size_of _ = 1

  end

  type t =
    { reason : ChangeReason.t
    ; desc : PortDescription.t }

  cstruct ofp_port_status {
      uint8_t reason;               (* One of OFPPR_* *)
      uint8_t pad[7]
  } as big_endian

  let to_string status = Printf.sprintf
    "{ reason = %s; desc = %s }"
    (ChangeReason.to_string status.reason)
    (PortDescription.to_string status.desc)

  let parse bits =
    let reason = ChangeReason.of_int (get_ofp_port_status_reason bits) in
    let _ = Cstruct.shift bits sizeof_ofp_port_status in
    let description = PortDescription.parse bits in
    { reason = reason
    ; desc = description }

  let to_string ps =
    let {reason; desc} = ps in
    Printf.sprintf "PortStatus %s %d"
      (ChangeReason.to_string reason)
      desc.PortDescription.port_no

  let size_of status = 
    ChangeReason.size_of status.reason + PortDescription.size_of status.desc

end

module SwitchFeatures = struct

  type supported_wildcards =
    { dlSrc : bool
    ; dlDst : bool
    ; dlTyp : bool
    ; dlVlan : bool
    ; dlVlanPcp : bool
    ; nwSrc : bool
    ; nwDst : bool
    ; nwProto : bool
    ; nwTos : bool
    ; tpSrc : bool
    ; tpDst : bool
    ; inPort : bool }

  module Capabilities = struct

    type t =
      { flow_stats : bool
      ; table_stats : bool
      ; port_stats : bool
      ; stp : bool
      ; ip_reasm : bool
      ; queue_stats : bool
      ; arp_match_ip : bool }

    let size_of _ = 4

    let to_string c = Printf.sprintf
      "{ flow_stats = %B; \
         table_stats = %B; \
         port_stats = %B; \
         stp = %B; \
         ip_reasm = %B; \
         queue_stats = %B; \
         arp_mat_ip = %B }"
      c.flow_stats
      c.table_stats
      c.port_stats
      c.stp
      c.ip_reasm
      c.queue_stats
      c.arp_match_ip

    let of_int d =
      { arp_match_ip = test_bit 7 d
      ; queue_stats = test_bit 6 d
      ; ip_reasm = test_bit 5 d
      ; stp = test_bit 3 d
      ; port_stats = test_bit 2 d
      ; table_stats = test_bit 1 d
      ; flow_stats = test_bit 0 d }

    let to_int c =
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

  module SupportedActions = struct

    type t =
      { output : bool
      ; set_vlan_id : bool
      ; set_vlan_pcp : bool
      ; strip_vlan : bool
      ; set_dl_src : bool
      ; set_dl_dst : bool
      ; set_nw_src : bool
      ; set_nw_dst : bool
      ; set_nw_tos : bool
      ; set_tp_src : bool
      ; set_tp_dst : bool
      ; enqueue : bool
      ; vendor : bool }

    let size_of _ = 4

    let to_string a = Printf.sprintf
      "{ output = %B; \
         set_vlan_id = %B; \
         set_vlan_pcp = %B; \
         strip_vlan = %B; \
         set_dl_src = %B; \
         set_dl_dst = %B; \
         set_nw_src = %B; \
         set_nw_dst = %B; \
         set_nw_tos = %B; \
         set_tp_src = %B; \
         set_tp_dst = %B; \
         enqueue = %B; \
         vendor = %B }"
      a.output
      a.set_vlan_id
      a.set_vlan_pcp
      a.strip_vlan
      a.set_dl_src
      a.set_dl_dst
      a.set_nw_src
      a.set_nw_dst
      a.set_nw_tos
      a.set_tp_src
      a.set_tp_dst
      a.enqueue
      a.vendor

    let of_int d =
      { output = test_bit 0 d
      ; set_vlan_id = test_bit 1 d
      ; set_vlan_pcp = test_bit 2 d
      ; strip_vlan = test_bit 3 d
      ; set_dl_src = test_bit 4 d
      ; set_dl_dst = test_bit 5 d
      ; set_nw_src = test_bit 6 d
      ; set_nw_dst = test_bit 7 d
      ; set_nw_tos = test_bit 8 d
      ; set_tp_src = test_bit 9 d
      ; set_tp_dst = test_bit 10 d
      ; enqueue = test_bit 11 d
      ; vendor = test_bit 12 d }

    let to_int a =
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

  type t =
    { switch_id : int64
    ; num_buffers : int32
    ; num_tables : int8
    ; supported_capabilities : Capabilities.t
    ; supported_actions : SupportedActions.t
    ; ports : PortDescription.t list }

  cstruct ofp_switch_features {
    uint64_t datapath_id;
    uint32_t n_buffers;
    uint8_t n_tables;
    uint8_t pad[3];
    uint32_t capabilities;
    uint32_t action
  } as big_endian

  let to_string feats = Printf.sprintf
    "{ switch_id = %Ld; num_buffers = %s; num_tables = %d; \
       supported_capabilities = %s; supported_actions = %s; ports = %s }"
    feats.switch_id
    (Int32.to_string feats.num_buffers)
    feats.num_tables
    (Capabilities.to_string feats.supported_capabilities)
    (SupportedActions.to_string feats.supported_actions)
    (Frenetic_Misc.string_of_list PortDescription.to_string feats.ports)

  let parse (buf : Cstruct.t) : t =
    let switch_id = get_ofp_switch_features_datapath_id buf in
    let num_buffers = get_ofp_switch_features_n_buffers buf in
    let num_tables = get_ofp_switch_features_n_tables buf in
    let supported_capabilities = Capabilities.of_int
      (get_ofp_switch_features_capabilities buf) in
    let supported_actions = SupportedActions.of_int
      (get_ofp_switch_features_action buf) in
    let buf = Cstruct.shift buf sizeof_ofp_switch_features in
    let portIter =
      Cstruct.iter
        (fun buf -> Some PortDescription.sizeof_ofp_phy_port)
        PortDescription.parse
        buf in
    let ports = Cstruct.fold (fun acc bits -> bits :: acc) portIter [] in
    { switch_id
    ; num_buffers
    ; num_tables
    ; supported_capabilities
    ; supported_actions
    ; ports }

  let size_of feats = 
    sizeof_ofp_switch_features 
    + sum (List.map PortDescription.size_of feats.ports)
end

module StatsRequest = struct

  module IndividualFlowRequest = struct

    type t =
      { of_match : Match.t
      ; table_id : int8
      ; port : PseudoPort.t option }

    cstruct ofp_flow_stats_request {
      uint8_t of_match[40];
      uint8_t table_id;
      uint8_t pad;
      uint16_t out_port
    } as big_endian

    let size_of _ = sizeof_ofp_flow_stats_request

    let to_string req =
      Printf.sprintf "{of_match = %s; table_id = %d; port = %s}"
        (Match.to_string req.of_match)
        req.table_id
        (Frenetic_Misc.string_of_option PseudoPort.to_string req.port)

    let marshal req out =
      let _ = Match.marshal req.of_match out in
      set_ofp_flow_stats_request_table_id out req.table_id;
      begin match req.port with
      | Some port ->
        set_ofp_flow_stats_request_out_port out (PseudoPort.marshal port);
      | None ->
        let open PseudoPort in
        let port_code = 0xFFFF in
        set_ofp_flow_stats_request_out_port out port_code
      end;
      size_of req

  end

  module AggregateFlowRequest = struct

    type t = { of_match : Match.t
             ; table_id : int8
             ; port : PseudoPort.t option }

    cstruct ofp_aggregate_stats_request {
      uint8_t of_match[40];
      uint8_t table_id;
      uint8_t pad;
      uint16_t out_port
    } as big_endian

    let to_string req =
      Printf.sprintf "{of_match = %s; table_id = %d; port = %s}"
        (Match.to_string req.of_match)
        req.table_id
        (Frenetic_Misc.string_of_option PseudoPort.to_string req.port)

    let size_of _ = sizeof_ofp_aggregate_stats_request

    let marshal req out =
      let _ = Match.marshal req.of_match out in
      set_ofp_aggregate_stats_request_table_id out req.table_id;
      begin match req.port with
      | Some port ->
        set_ofp_aggregate_stats_request_out_port out (PseudoPort.marshal port);
      | None ->
        let open PseudoPort in
        let port_code = 0xFFFF in (* TODO(arjun): PsuedoPort.marshal does this *)
        set_ofp_aggregate_stats_request_out_port out port_code
      end;
      size_of req

  end

  type t =
  | DescriptionReq
  | IndividualFlowReq of IndividualFlowRequest.t
  | AggregateFlowReq of AggregateFlowRequest.t
  | TableReq
  | PortReq of PseudoPort.t
  (* TODO(cole): queue and vendor stats requests. *)

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

  let size_of msg =
    let header_size = sizeof_ofp_stats_request in
    match msg with
    | DescriptionReq -> header_size
    | IndividualFlowReq req ->
      header_size + (IndividualFlowRequest.size_of req)
    | AggregateFlowReq req -> 
      header_size + (AggregateFlowRequest.size_of req)
    | _ ->
      failwith (Printf.sprintf "NYI: StatsRequest.size_of: %s" (to_string msg))

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

  module DescriptionStats = struct

    type t =
      { manufacturer : string
      ; hardware : string
      ; software : string
      ; serial_number : string
      ; datapath : string }

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
      { manufacturer = mfr_desc
      ; hardware = hw_desc
      ; software = sw_desc
      ; serial_number = serial_num
      ; datapath = dp_desc }

    let size_of _ = sizeof_ofp_desc_stats

    let to_string desc = Printf.sprintf
      "{ manufacturer = %s; hardware = %s; software = %s;\
         serial_number = %s; datapath = %s}"
      desc.manufacturer desc.hardware desc.software
      desc.serial_number desc.datapath

  end

  module IndividualFlowStats = struct

    type t =
      { table_id : int8
      ; of_match : Match.t
      ; duration_sec : int32
      ; duration_nsec : int32
      ; priority : int16
      ; idle_timeout : int
      ; hard_timeout : int
      ; cookie : int64
      ; packet_count : int64
      ; byte_count : int64
      ; actions : Action.sequence }

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

    let to_string stats = Printf.sprintf
      "{ table_id = %d; of_match = %s; duration_sec = %d; duration_nsec = %d\
         priority = %d; idle_timeout = %d; hard_timeout = %d; cookie = %Ld\
         packet_count = %Ld; byte_count = %Ld; actions = %s }"
      stats.table_id
      (Match.to_string stats.of_match)
      (Int32.to_int stats.duration_sec)
      (Int32.to_int stats.duration_nsec)
      stats.priority
      stats.idle_timeout
      stats.hard_timeout
      stats.cookie
      stats.packet_count
      stats.byte_count
      (Action.sequence_to_string stats.actions)

    let sequence_to_string = Frenetic_Misc.string_of_list to_string

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

      ( { table_id = table_id
        ; of_match = of_match
        ; duration_sec = duration_sec
        ; duration_nsec = duration_nsec
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

  module AggregateFlowStats = struct

    type t =
      { packet_count : int64
      ; byte_count : int64
      ; flow_count : int32 }

    cstruct ofp_aggregate_stats {
      uint64_t packet_count;
      uint64_t byte_count;
      uint32_t flow_count;
      uint8_t pad[4]
    } as big_endian

    let to_string stats = Printf.sprintf
      "{ packet_count = %Ld; byte_count = %Ld; flow_count = %ld }"
      stats.packet_count
      stats.byte_count
      stats.flow_count

    let parse bits = 
      { packet_count = get_ofp_aggregate_stats_packet_count bits;
	byte_count = get_ofp_aggregate_stats_byte_count bits;
	flow_count = get_ofp_aggregate_stats_flow_count bits }

  end

  module TableStats = struct

    type t =
      { table_id : int8
      ; name : string
      ; wildcards : SwitchFeatures.supported_wildcards
      ; max_entries : int32
      ; active_count : int32
      ; lookup_count : int64
      ; matched_count : int64 }

    let to_string stats = failwith "NYI: TableStats.to_string"
    let parse _ = failwith "NYI: TableStats.parse"

  end

  module PortStats = struct

    type t =
      { port_no : PseudoPort.t
      ; rx_packets : int64
      ; tx_packets : int64
      ; rx_bytes : int64
      ; tx_bytes : int64
      ; rx_dropped : int64
      ; tx_dropped : int64
      ; rx_errors : int64
      ; tx_errors : int64
      ; rx_frame_err : int64
      ; rx_over_err : int64
      ; rx_crc_err : int64
      ; collisions : int64 }

    let to_string stats = failwith "NYI: PortStats.to_string"
    let parse _ = failwith "NYI: PortStats.parse"

  end

  type t =
    | DescriptionRep of DescriptionStats.t
    | IndividualFlowRep of IndividualFlowStats.t list
    | AggregateFlowRep of AggregateFlowStats.t
    | TableRep of TableStats.t
    | PortRep of PortStats.t

  cstruct ofp_stats_reply {
    uint16_t stats_type;
    uint16_t flags
  } as big_endian

  let to_string rep = match rep with
    | DescriptionRep stats -> DescriptionStats.to_string stats
    | IndividualFlowRep stats ->
      Frenetic_Misc.string_of_list IndividualFlowStats.to_string stats
    | AggregateFlowRep stats -> AggregateFlowStats.to_string stats
    | TableRep stats -> TableStats.to_string stats
    | PortRep stats -> PortStats.to_string stats

  let parse bits =
    let stats_type_code = get_ofp_stats_reply_stats_type bits in
    let body = Cstruct.shift bits sizeof_ofp_stats_reply in
    match int_to_ofp_stats_types stats_type_code with
    | Some OFPST_DESC -> DescriptionRep (DescriptionStats.parse body)
    | Some OFPST_FLOW ->
      IndividualFlowRep (IndividualFlowStats.parse_sequence body)
    | Some OFPST_AGGREGATE ->
      AggregateFlowRep (AggregateFlowStats.parse body)
    | Some OFPST_TABLE -> TableRep (TableStats.parse body)
    | Some OFPST_PORT -> PortRep (PortStats.parse body)
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

    type t =
      | Incompatible
      | Eperm

    cenum ofp_hello_failed_code {
      OFPHFC_INCOMPATIBLE;
      OFPHFC_EPERM
    } as uint16_t

    let of_int error_code =
      match int_to_ofp_hello_failed_code error_code with
      | Some OFPHFC_INCOMPATIBLE -> Incompatible
      | Some OFPHFC_EPERM -> Eperm
      | None ->
        let msg = "NYI: ofp_hello_failed_code in error" in
              raise (Unparsable msg)

    let to_string e = match e with
      | Incompatible -> "Incompatible"
      | Eperm -> "Eperm"

  end

  module BadRequest = struct

    type t =
      | BadVersion
      | BadType
      | BadStat
      | BadVendor
      | BadSubType
      | Eperm
      | BadLen
      | BufferEmpty
      | BufferUnknown

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

    let of_int error_code =
      match int_to_ofp_bad_request_code error_code with
      | Some OFPBRC_BAD_VERSION -> BadVersion
      | Some OFPBRC_BAD_TYPE -> BadType
      | Some OFPBRC_BAD_STAT -> BadStat
      | Some OFPBRC_BAD_VENDOR -> BadVendor
      | Some OFPBRC_BAD_SUBTYPE -> BadSubType
      | Some OFPBRC_EPERM -> Eperm
      | Some OFPBRC_BAD_LEN -> BadLen
      | Some OFPBRC_BUFFER_EMPTY -> BufferEmpty
      | Some OFPBRC_BUFFER_UNKNOWN -> BufferUnknown
      | None ->
        let msg = "NYI: ofp_bad_request_code in error" in
              raise (Unparsable msg)

    let to_string r = match r with
      | BadVersion -> "BadVersion"
      | BadType -> "BadType"
      | BadStat -> "BadStat"
      | BadVendor -> "BadVendor"
      | BadSubType -> "BadSubType"
      | Eperm -> "Eperm"
      | BadLen -> "BadLen"
      | BufferEmpty -> "BufferEmpty"
      | BufferUnknown -> "BufferUnknown"

  end

  module BadAction = struct

    type t =
      | BadType
      | BadLen
      | BadVendor
      | BadVendorType
      | BadOutPort
      | BadArgument
      | Eperm
      | TooMany
      | BadQueue

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

    let of_int error_code =
      match int_to_ofp_bad_action_code error_code with
      | Some OFPBAC_BAD_TYPE -> BadType
      | Some OFPBAC_BAD_LEN -> BadLen
      | Some OFPBAC_BAD_VENDOR -> BadVendor
      | Some OFPBAC_BAD_VENDOR_TYPE -> BadVendorType
      | Some OFPBAC_BAD_OUT_PORT -> BadOutPort
      | Some OFPBAC_BAD_ARGUMENT -> BadArgument
      | Some OFPBAC_EPERM -> Eperm
      | Some OFPBAC_TOO_MANY -> TooMany
      | Some OFPBAC_BAD_QUEUE -> BadQueue
      | None ->
        let msg = "NYI: ofp_bad_action_code in error" in
              raise (Unparsable msg)

    let to_string a = match a with
      | BadType -> "BadType"
      | BadLen -> "BadLen"
      | BadVendor -> "BadVendor"
      | BadVendorType -> "BadVendorType"
      | BadOutPort -> "BadOutPort"
      | BadArgument -> "BadArgument"
      | Eperm -> "Eperm"
      | TooMany -> "TooMany"
      | BadQueue -> "BadQueue"

  end

  module FlowModFailed = struct

    type t =
      | AllTablesFull
      | Overlap
      | Eperm
      | BadEmergTimeout
      | BadCommand
      | Unsupported

    cenum ofp_flow_mod_failed_code {
      OFPFMFC_ALL_TABLES_FULL;
      OFPFMFC_OVERLAP;
      OFPFMFC_EPERM;
      OFPFMFC_BAD_EMERG_TIMEOUT;
      OFPFMFC_BAD_COMMAND;
      OFPFMFC_UNSUPPORTED
    } as uint16_t

    let of_int error_code =
      match int_to_ofp_flow_mod_failed_code error_code with
      | Some OFPFMFC_ALL_TABLES_FULL -> AllTablesFull
      | Some OFPFMFC_OVERLAP -> Overlap
      | Some OFPFMFC_EPERM -> Eperm
      | Some OFPFMFC_BAD_EMERG_TIMEOUT -> BadEmergTimeout
      | Some OFPFMFC_BAD_COMMAND -> BadCommand
      | Some OFPFMFC_UNSUPPORTED -> Unsupported
      | None ->
        let msg = "NYI: ofp_flow_mod_failed_code in error" in
              raise (Unparsable msg)

    let to_string f = match f with
      | AllTablesFull -> "AllTablesFull"
      | Overlap -> "Overlap"
      | Eperm -> "Eperm"
      | BadEmergTimeout -> "BadEmergTimeout"
      | BadCommand -> "BadCommand"
      | Unsupported -> "Unsupported"

  end

  module PortModFailed = struct

    type t =
      | BadPort
      | BadHwAddr

    cenum ofp_port_mod_failed_code {
      OFPPMFC_BAD_PORT;
      OFPPMFC_BAD_HW_ADDR
    } as uint16_t

    let of_int error_code =
      match int_to_ofp_port_mod_failed_code error_code with
      | Some OFPPMFC_BAD_PORT -> BadPort
      | Some OFPPMFC_BAD_HW_ADDR -> BadHwAddr
      | None ->
        let msg = "NYI: ofp_port_mod_failed_code in error" in
              raise (Unparsable msg)

    let to_string = function
      | BadPort -> "BadPort"
      | BadHwAddr -> "BadHwAddr"

  end

  module QueueOpFailed = struct

    type t =
      | BadPort
      | BadQueue
      | Eperm

    cenum ofp_queue_op_failed_code {
      OFPQOFC_BAD_PORT;
      OFPQOFC_BAD_QUEUE;
      OFPQOFC_EPERM
    } as uint16_t

    let of_int error_code =
      match int_to_ofp_queue_op_failed_code error_code with
      | Some OFPQOFC_BAD_PORT -> BadPort
      | Some OFPQOFC_BAD_QUEUE -> BadQueue
      | Some OFPQOFC_EPERM -> Eperm
      | None ->
        let msg = "NYI: ofp_queue_op_failed_code in error" in
              raise (Unparsable msg)

    let to_string = function
      | BadPort -> "BadPort"
      | BadQueue -> "BadQueue"
      | Eperm -> "Eperm"

  end

  (* Each error is composed of a pair (error_code, data) *)
  type t =
    | HelloFailed of HelloFailed.t * Cstruct.t
    | BadRequest of BadRequest.t * Cstruct.t
    | BadAction of BadAction.t * Cstruct.t
    | FlowModFailed of FlowModFailed.t * Cstruct.t
    | PortModFailed of PortModFailed.t * Cstruct.t
    | QueueOpFailed of QueueOpFailed.t  * Cstruct.t

  let parse bits =
    let error_type = get_ofp_error_msg_error_type bits in
    let error_code = get_ofp_error_msg_error_code bits in
    let body = Cstruct.shift bits sizeof_ofp_error_msg in
    match int_to_ofp_error_type error_type with
    | Some OFPET_HELLO_FAILED ->
      HelloFailed ((HelloFailed.of_int error_code), body)
    | Some OFPET_BAD_REQUEST ->
      BadRequest ((BadRequest.of_int error_code), body)
    | Some OFPET_BAD_ACTION ->
      BadAction ((BadAction.of_int error_code), body)
    | Some OFPET_FLOW_MOD_FAILED ->
      FlowModFailed ((FlowModFailed.of_int error_code), body)
    | Some OFPET_PORT_MOD_FAILED ->
      PortModFailed ((PortModFailed.of_int error_code), body)
    | Some OFPET_QUEUE_OP_FAILED ->
      QueueOpFailed ((QueueOpFailed.of_int error_code), body)
    | None ->
      let msg =
        sprintf "bad ofp_error_type in ofp_error_msg (%d)" error_type in
      raise(Unparsable msg)

  let to_string = function
    | HelloFailed (code, _) -> 
      "HelloFailed (" ^ (HelloFailed.to_string code) ^ ", <data>)"
    | BadRequest (code, _) -> 
      "BadRequest (" ^ (BadRequest.to_string code) ^ ", <data>)"
    | BadAction (code, _) -> 
      "BadAction (" ^ (BadAction.to_string code) ^ ", <data>)"
    | FlowModFailed (code, _) -> 
      "FlowModFailed (" ^ (FlowModFailed.to_string code) ^ ", <data>)"
    | PortModFailed (code, _) ->
      "PortModFailed (" ^ (PortModFailed.to_string code) ^ ", <data>)"
    | QueueOpFailed (code, _) ->
      "QueueOpFailed (" ^ (QueueOpFailed.to_string code) ^ ", <data>)"

end

module Message = struct
(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

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

  module Header = struct

    let ver : int = 0x01

    type t =
      { ver: int
      ; typ: msg_code
      ; len: int
      ; xid: int32 }

    cstruct ofp_header {
      uint8_t version;
      uint8_t typ;
      uint16_t length;
      uint32_t xid
    } as big_endian

    let size = sizeof_ofp_header
    let size_of _ = size
    let len hdr = hdr.len

    let marshal hdr out =
      set_ofp_header_version out hdr.ver;
      set_ofp_header_typ out (msg_code_to_int hdr.typ);
      set_ofp_header_length out hdr.len;
      set_ofp_header_xid out hdr.xid;
      size_of hdr

    (** [parse buf] assumes that [buf] has size [sizeof_ofp_header]. *)
    let parse body_buf =
      let buf = Cstruct.of_string body_buf in
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

  type t =
    | Hello of bytes
    | ErrorMsg of Error.t
    | EchoRequest of bytes
    | EchoReply of bytes
    | SwitchFeaturesRequest
    | SwitchFeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | PacketInMsg of PacketIn.t
    | PortStatusMsg of PortStatus.t
    | PacketOutMsg of PacketOut.t
    | BarrierRequest (* JNF: why not "BarrierRequestMsg"? *)
    | BarrierReply (* JNF: why not "BarrierReplyMsg"? *)
    | StatsRequestMsg of StatsRequest.t
    | StatsReplyMsg of StatsReply.t

  let delete_all_flows =
    let open FlowMod in
    FlowModMsg
      { mod_cmd = Command.DeleteFlow
      ; match_ = Match.all
      ; priority = 0
      ; actions = []
      ; cookie = 0L
      ; idle_timeout = Timeout.Permanent
      ; hard_timeout = Timeout.Permanent
      ; notify_when_removed = false
      ; apply_to_packet = None
      ; out_port = None
      ; check_overlap = false }

  let parse (hdr : Header.t) (body_buf : string) : (xid * t) =
    let buf = Cstruct.of_string body_buf in
    let msg = match hdr.Header.typ with
      | HELLO -> Hello buf
      | ERROR -> ErrorMsg (Error.parse buf)
      | ECHO_REQ -> EchoRequest buf
      | ECHO_RESP -> EchoReply buf
      | FEATURES_REQ -> SwitchFeaturesRequest
      | FEATURES_RESP -> SwitchFeaturesReply (SwitchFeatures.parse buf)
      | PACKET_IN -> PacketInMsg (PacketIn.parse buf)
      | PORT_STATUS -> PortStatusMsg (PortStatus.parse buf)
      | BARRIER_REQ -> BarrierRequest
      | BARRIER_RESP -> BarrierReply
      | STATS_RESP -> StatsReplyMsg (StatsReply.parse buf)
      | code -> raise (Ignored
        (Printf.sprintf "unexpected message type (%s)"
          (string_of_msg_code code)))
      in
    (hdr.Header.xid, msg)

  let msg_code_of_message (msg : t) : msg_code = match msg with
    | Hello _ -> HELLO
    | ErrorMsg _ -> ERROR
    | EchoRequest _ -> ECHO_REQ
    | EchoReply _ -> ECHO_RESP
    | SwitchFeaturesRequest -> FEATURES_REQ
    | SwitchFeaturesReply _ -> FEATURES_RESP
    | FlowModMsg _ -> FLOW_MOD
    | PacketOutMsg _ -> PACKET_OUT
    | PortStatusMsg _ -> PORT_STATUS
    | PacketInMsg _ -> PACKET_IN
    | BarrierRequest -> BARRIER_REQ
    | BarrierReply -> BARRIER_RESP
    | StatsRequestMsg _ -> STATS_REQ
    | StatsReplyMsg _ -> STATS_RESP

  let to_string (msg : t) : string = match msg with
    | Hello _ -> "Hello"
    | ErrorMsg _ -> "Error"
    | EchoRequest _ -> "EchoRequest"
    | EchoReply _ -> "EchoReply"
    | SwitchFeaturesRequest -> "SwitchFeaturesRequest"
    | SwitchFeaturesReply _ -> "SwitchFeaturesReply"
    | FlowModMsg _ -> "FlowMod"
    | PacketOutMsg _ -> "PacketOut"
    | PortStatusMsg _ -> "PortStatus"
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
    | SwitchFeaturesRequest -> 0
    | SwitchFeaturesReply rep -> SwitchFeatures.size_of rep
    | FlowModMsg msg -> FlowMod.size_of msg
    | PacketOutMsg msg -> PacketOut.size_of msg
    | BarrierRequest -> 0
    | BarrierReply -> 0
    | StatsRequestMsg msg -> StatsRequest.size_of msg
    | ErrorMsg _
    | PacketInMsg _
    | PortStatusMsg _
    | StatsReplyMsg _ -> 
      raise (Invalid_argument "cannot marshal (controller should not send \
                               this message to a switch")

  let blit_message (msg : t) (out : Cstruct.t) = match msg with
    | Hello buf
    | EchoRequest buf
    | EchoReply buf ->
      Cstruct.blit buf 0 out 0 (Cstruct.len buf)
    | SwitchFeaturesRequest -> ()
    | FlowModMsg flow_mod ->
      let _ = FlowMod.marshal flow_mod out in
      ()
    | PacketOutMsg msg ->
      let _ = PacketOut.marshal msg out in
      ()
    (* | PacketInMsg _ -> () (\* TODO(arjun): wtf? *\) *)
    (* | SwitchFeaturesReply _ -> () (\* TODO(arjun): wtf? *\) *)
    | BarrierRequest -> () (* no body, this is okay *)
    | BarrierReply -> () (* no body, this is okay *)
    | StatsRequestMsg msg ->
      let _ = StatsRequest.marshal msg out in
      ()
    | ErrorMsg _
    | PacketInMsg _
    | PortStatusMsg _
    | SwitchFeaturesReply _
    | StatsReplyMsg _ ->
      failwith "should not reach this line (sizeof_body should raise)"

  let size_of msg = Header.size + sizeof_body msg

  let marshal (xid : xid) (msg : t) : string =
    let hdr = let open Header in
      {ver = ver; typ = msg_code_of_message msg; len = 0; xid = xid} in
    let sizeof_buf = Header.size_of hdr + sizeof_body msg in
    let hdr = {hdr with Header.len = sizeof_buf} in
    let buf = Cstruct.create sizeof_buf in
    let _ = Header.marshal hdr buf in
    blit_message msg (Cstruct.shift buf (Header.size_of hdr));
    let str = Cstruct.to_string buf in
    str

end

module type PLATFORM = sig
  exception SwitchDisconnected of switchId
  val send_to_switch : switchId -> xid -> Message.t -> unit Lwt.t
  val recv_from_switch : switchId -> (xid * Message.t) Lwt.t
  val accept_switch : unit -> SwitchFeatures.t Lwt.t
end
