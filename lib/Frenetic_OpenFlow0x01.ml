open Core.Std
open Frenetic_Packet
open Format

exception Unparsable of string
exception Ignored of string

type 'a mask = { m_value : 'a; m_mask : 'a option } with sexp

type switchId = int64 with sexp

type portId = int16 with sexp

type queueId = int32 with sexp

type xid = Frenetic_OpenFlow_Header.xid

type pattern =
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan option
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr mask option
    ; nwDst : nwAddr mask option
    ; nwProto : nwProto option
    ; nwTos : nwTos option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option 
    } with sexp

type pseudoPort =
  | PhysicalPort of portId
  | InPort
  | Table
  | Normal
  | Flood
  | AllPorts
  | Controller of int
  | Local
with sexp

type action =
  | Output of pseudoPort
  | SetDlVlan of dlVlan
  | SetDlVlanPcp of dlVlanPcp
  | SetDlSrc of dlAddr
  | SetDlDst of dlAddr
  | SetNwSrc of nwAddr
  | SetNwDst of nwAddr
  | SetNwTos of nwTos
  | SetTpSrc of tpPort
  | SetTpDst of tpPort
  | Enqueue of pseudoPort * queueId
with sexp

type timeout =
  | Permanent
  | ExpiresAfter of int16
with sexp

type flowModCommand =
  | AddFlow
  | ModFlow
  | ModStrictFlow
  | DeleteFlow
  | DeleteStrictFlow
with sexp

type flowMod =
    { command : flowModCommand
    ; pattern: pattern
    ; priority : int16
    ; actions : action list
    ; cookie : int64
    ; idle_timeout : timeout
    ; hard_timeout : timeout
    ; notify_when_removed : bool
    ; apply_to_packet : int32 option
    ; out_port : pseudoPort option
    ; check_overlap : bool
    } with sexp

type payload =
  | Buffered of int32 * Cstruct.t
  | NotBuffered of Cstruct.t
with sexp

type packetInReason =
  | NoMatch
  | ExplicitSend
with sexp

type packetIn =
    { input_payload : payload
    ; total_len : int16
    ; port : portId
    ; reason : packetInReason
    } with sexp

type packetOut =
    { output_payload : payload
    ; port_id : portId option
    ; apply_actions : action list
    }
with sexp

type flowRemovedReason =
  | IdleTimeout
  | HardTimeout
  | Delete
with sexp

type flowRemoved =
    { pattern : pattern
    ; cookie : int64
    ; priority : int16
    ; reason : flowRemovedReason
    ; duration_sec : int32
    ; duration_nsec : int32
    ; idle_timeout : timeout
    ; packet_count : int64
    ; byte_count : int64
    } with sexp

type statsReq =
  { sr_of_match : pattern
  ; sr_table_id : int8
  ; sr_out_port : pseudoPort option
  } with sexp

type request =
  | DescriptionRequest
  | FlowTableStatsRequest
  | IndividualRequest of statsReq
  | AggregateRequest of statsReq
  | PortRequest of pseudoPort option
with sexp

type descriptionStats =
    { manufacturer : string
    ; hardware : string
    ; software : string
    ; serial_number : string
    ; datapath : string
    } with sexp

type individualStats =
    { table_id : int8
    ; of_match : pattern
    ; duration_sec : int32
    ; duration_nsec : int32
    ; priority : int16
    ; idle_timeout : int16
    ; hard_timeout : int16
    ; cookie : int64
    ; packet_count : int64
    ; byte_count : int64
    ; actions : action list
    } with sexp

type aggregateStats =
    { total_packet_count : int64
    ; total_byte_count : int64
    ; flow_count : int32
    } with sexp

type portStats =
    { port_no : int16
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
    ; collisions : int64
    } with sexp

type reply =
  | DescriptionRep of descriptionStats
  | IndividualFlowRep of individualStats list
  | AggregateFlowRep of aggregateStats
  | PortRep of portStats list
with sexp

type stpState =
  | Listen
  | Learn
  | Forward
  | Block
with sexp

type portState = 
  { down : bool; 
    stp_state : stpState 
  } with sexp

type portFeatures =
  { f_10MBHD : bool
  ; f_10MBFD : bool
  ; f_100MBHD : bool
  ; f_100MBFD : bool
  ; f_1GBHD : bool
  ; f_1GBFD : bool
  ; f_10GBFD : bool
  ; copper : bool
  ; fiber : bool
  ; autoneg : bool
  ; pause : bool
  ; pause_asym : bool
  } with sexp

type portConfig =
  { down : bool
  ; no_stp : bool
  ; no_recv : bool
  ; no_recv_stp : bool
  ; no_flood : bool
  ; no_fwd : bool
  ; no_packet_in : bool
  } with sexp

type portDescription =
  { port_no : portId
  ; hw_addr : dlAddr
  ; name : string
  ; config : portConfig
  ; state : portState
  ; curr : portFeatures
  ; advertised : portFeatures
  ; supported : portFeatures
  ; peer : portFeatures 
  } with sexp

module Format = struct

  open Format

  let bytes fmt bytes =
    try
      Frenetic_Packet.format_packet fmt (Frenetic_Packet.parse bytes)
    with exn -> (* TODO(arjun): should catch right error *)
      fprintf fmt "unparsable packet"

  let payload fmt payload =
    match payload with
      | NotBuffered buf -> bytes fmt buf
      | Buffered (n, buf) -> fprintf fmt "%a (buffered at %s)" bytes buf
        (Int32.to_string n)

  let reason fmt = function
      | NoMatch -> fprintf fmt "NoMatch"
      | ExplicitSend -> fprintf fmt "ExplicitSend"

  let packetIn fmt pktIn =
    fprintf fmt
      "@[packetIn{@;<1 2>@[@[total_len=%d@]@ @[port=%d@]@ @[reason=%a@]@ \
                    @[payload=%a@]@]@ }@]"
      pktIn.total_len pktIn.port reason pktIn.reason
      payload pktIn.input_payload

  (* TODO(jnf): we have this defined in several places. Consolidate. *)
  let string_of_mk formatter x =
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    formatter fmt x;
    fprintf fmt "@?";
    Buffer.contents buf

  let descriptionStats fmt v =
    fprintf fmt "@[{@[@[manufacturer=%s;@]@ @[hardware=%s;@]@ \
                      @[software=%s;@]@ @[serial=%s;@]@ @[datapath=%s@]@]}@]"
      v.manufacturer v.hardware v.software v.serial_number v.datapath

  (* TODO(arjun): must fill *)
  let individualStats fmt v =
    fprintf fmt "individualStats"

  let aggregateStats fmt v =
    fprintf fmt "@[{@[@[packets=%Ld;@]@ @[bytes=%Ld;@]@ @[flows=%ld@]@]}@]"
      v.total_packet_count v.total_byte_count v.flow_count

  let fmt_one_port_stat fmt (v: portStats) = 
    fprintf fmt "@[{@[port_no=%d@ \
                      rx_packets=%Ld@ tx_packets=%Ld@ \
                      rx_bytes=%Ld@ tx_bytes=%Ld@ \
                      rx_dropped=%Ld@ tx_dropped=%Ld@ \
                      rx_errors=%Ld@ tx_errors=%Ld@ \
                      rx_frame_err=%Ld@ rx_over_err=%Ld@ rx_crc_err=%Ld@ \
                      collisions=%Ld@]}@]"
      v.port_no
      v.rx_packets v.tx_packets
      v.rx_bytes v.tx_bytes
      v.rx_dropped v.tx_dropped
      v.rx_errors v.tx_errors
      v.rx_frame_err v.rx_over_err v.rx_crc_err
      v.collisions

  let portStats fmt (vl : portStats list) =
    List.iter vl (fmt_one_port_stat fmt)

  let reply fmt v = match v with
    | DescriptionRep st -> descriptionStats fmt st
    | IndividualFlowRep st -> individualStats fmt st
    | AggregateFlowRep st -> aggregateStats fmt st
    | PortRep st -> portStats fmt st

  let string_of_mk formatter x =
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    formatter fmt x;
    fprintf fmt "@?";
    Buffer.contents buf

end

let add_flow prio pat ?(idle_to = Permanent) ?(notify_removed = false) actions =
  { command = AddFlow;
    pattern = pat;
    priority = prio;
    actions = actions;
    cookie = 0L;
    idle_timeout = idle_to;
    hard_timeout = Permanent;
    notify_when_removed = notify_removed;
    out_port =  None;
    apply_to_packet = None;
    check_overlap = false
  }

let delete_flow_strict prio pat port =
  { command = DeleteStrictFlow
  ; pattern = pat
  ; priority = prio
  ; actions = []
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  ; notify_when_removed = false
  ; apply_to_packet = None
  ; out_port = port
  ; check_overlap = false
  }

let match_all = {
  dlSrc = None;
  dlDst = None;
  dlTyp = None;
  dlVlan = None;
  dlVlanPcp = None;
  nwSrc = None;
  nwDst = None;
  nwProto = None;
  nwTos = None;
  tpSrc = None;
  tpDst = None;
  inPort = None
}

let delete_all_flows =
  { command = DeleteFlow
  ; pattern = match_all
  ; priority = 0
  ; actions = []
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  ; notify_when_removed = false
  ; apply_to_packet = None
  ; out_port = None
  ; check_overlap = false }


let parse_payload = function
  | Buffered (_, b)
  | NotBuffered b ->
    Frenetic_Packet.parse b

let marshal_payload buffer pkt =
  let payload = Frenetic_Packet.marshal pkt in
  match buffer with
    | Some b -> Buffered (b, payload)
    | None -> NotBuffered payload


let packetIn_to_string  = Format.string_of_mk Format.packetIn

let string_of_switchId = Printf.sprintf "0x%Lx"
let string_of_portId = string_of_int
let string_of_queueId =  Int32.to_string

let bit (x : int32) (n : int) (v : bool) : int32 = Frenetic_Bits.bit x n v
let test_bit (n:int) (x:int32) : bool = Frenetic_Bits.test_bit n x

let vlan_none = 0xffff

cenum ofp_stats_types {
  OFPST_DESC;
  OFPST_FLOW;
  OFPST_AGGREGATE;
  OFPST_TABLE;
  OFPST_PORT;
  OFPST_QUEUE;
  OFPST_VENDOR = 0xffff
} as uint16_t

type wildcards = {
  in_port: bool;
  dl_vlan: bool;
  dl_src: bool;
  dl_dst: bool;
  dl_type: bool;
  nw_proto: bool;
  tp_src: bool;
  tp_dst: bool;
  nw_src: int; (* XXX: unsigned *)
  nw_dst: int; (* XXX: unsigned *)
  dl_vlan_pcp: bool;
  nw_tos: bool;
} with sexp

(** Internal module, only used to parse the wildcards bitfield *)
module Wildcards = struct

  let set_nw_mask (f:int32) (off : int) (v : int) : int32 =
    let value = (0x3f land v) lsl off in
    Int32.(bit_or f (of_int_exn value))

  let get_nw_mask (f : int32) (off : int) : int =
    Int32.(to_int_exn (shift_right f off)) land 0x3f

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
    sprintf
      "in_port:%b,dl_vlan:%b,dl_src:%b,dl_dst:%b,dl_type:%b,\
       nw_proto:%b,tp_src:%b,tp_dst:%b,nw_src:%d,nw_dst:%d,\
       dl_vlan_pcp:%b,nw_tos:%b"
      h.in_port
      h.dl_vlan
      h.dl_src h.dl_dst
      h.dl_type
      h.nw_proto
      h.tp_src h.tp_dst
      h.nw_src h.nw_dst
      h.dl_vlan_pcp
      h.nw_tos

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

  type t = pattern with sexp

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

  let size_of _ = sizeof_ofp_match

  let is_none x = match x with
    | None -> true
    | Some _ -> false

  let mask_bits x = match x with
    | None -> 32 (* WildcardAll *)
    | Some x -> match x.m_mask with
                  | None -> 0 (* WildcardExact *)
                  | Some m -> Int32.to_int_exn m

  let wildcards_of_match (m : t) : wildcards =
    { in_port = is_none m.inPort;
      dl_vlan =
	(match m.dlVlan with
	  | None -> true
	  | Some None -> false
	  | Some (Some _) -> false);
      dl_src = is_none m.dlSrc;
      dl_dst = is_none m.dlDst;
      dl_type = is_none m.dlTyp;
      nw_proto = is_none m.nwProto;
      tp_src = is_none m.tpSrc;
      tp_dst = is_none m.tpDst;
      nw_src = mask_bits m.nwSrc;
      nw_dst = mask_bits m.nwDst;
      dl_vlan_pcp = is_none m.dlVlanPcp;
      nw_tos = is_none m.nwTos;
    }

  let if_some16 x = match x with
    | Some n -> n
    | None -> 0

  let if_some8 x = match x with
    | Some n -> n
    | None -> 0

  let if_some32mask x = match x with
    | Some x -> x.m_value
    | None -> 0l

  let if_word48 x = match x with
    | Some n -> n
    | None -> 0L

  let marshal (m : pattern) bits =
    set_ofp_match_wildcards bits (Wildcards.marshal (wildcards_of_match m));
    set_ofp_match_in_port bits (if_some16 m.inPort);
    set_ofp_match_dl_src (bytes_of_mac (if_word48 m.dlSrc)) 0 bits;
    set_ofp_match_dl_dst (bytes_of_mac (if_word48 m.dlDst)) 0 bits;
    let vlan = match m.dlVlan with
      | Some (Some v) -> v
      | Some None -> vlan_none
      | None -> vlan_none in
    set_ofp_match_dl_vlan bits (vlan);
    set_ofp_match_dl_vlan_pcp bits (if_some8 m.dlVlanPcp);
    set_ofp_match_dl_type bits (if_some16 m.dlTyp);
    set_ofp_match_nw_tos bits (if_some8 m.nwTos);
    set_ofp_match_nw_proto bits (if_some8 m.nwProto);
    set_ofp_match_nw_src bits (if_some32mask m.nwSrc);
    set_ofp_match_nw_dst bits (if_some32mask m.nwDst);
    set_ofp_match_tp_src bits (if_some16 m.tpSrc);
    set_ofp_match_tp_dst bits (if_some16 m.tpDst);
    sizeof_ofp_match

  let parse bits =
    let w = Wildcards.parse (get_ofp_match_wildcards bits) in
    { dlSrc =
        if w.dl_src then
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_src bits)));
      dlDst =
        if w.dl_dst then
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_dst bits)));
      dlVlan =
        if w.dl_vlan then
          None
        else
          begin
            let vlan = get_ofp_match_dl_vlan bits in
            if vlan = vlan_none then
              Some None
            else
              Some (Some vlan)
          end;
      dlVlanPcp =
        if w.dl_vlan_pcp then
          None
        else
          Some (get_ofp_match_dl_vlan_pcp bits);
      dlTyp =
        if w.dl_type then
          None
        else
          Some (get_ofp_match_dl_type bits);
      nwSrc =
      (* Oversimplified, since we don't support IP prefixes *)
        if w.nw_src >= 32 then
          None
        else
          if w.nw_src = 0 then
            Some {m_value = (get_ofp_match_nw_src bits); m_mask = None}
          else
            Some {m_value = (get_ofp_match_nw_src bits);
                   m_mask = Some (Int32.of_int_exn w.nw_src)};
      nwDst =
        (* Oversimplified, since we don't support IP prefixes *)
        if w.nw_dst >= 32 then
          None
        else
          if w.nw_dst = 0 then
            Some {m_value = (get_ofp_match_nw_dst bits); m_mask = None}
          else
            Some {m_value = (get_ofp_match_nw_dst bits);
                  m_mask = Some (Int32.of_int_exn w.nw_dst)};
      nwProto =
        if w.nw_proto then
          None
        else
          Some (get_ofp_match_nw_proto bits);
      nwTos =
        if w.nw_tos then
          None
        else
          Some (get_ofp_match_nw_tos bits);
      tpSrc =
        if w.tp_src then
          None
        else
          Some (get_ofp_match_tp_src bits);
      tpDst =
        if w.tp_dst then
          None
        else
          Some (get_ofp_match_tp_dst bits);
      inPort =
        if w.in_port then
          None
        else
          Some (get_ofp_match_in_port bits);
    }

  (* Helper for to_string *)
  let fld_str (lbl : string) (pr : 'a -> string) (v : 'a option)
      : string option =
    match v with
      | None -> None
      | Some a -> Some (sprintf "%s = %s" lbl (pr a))

  let mask_pr (pr : 'a -> string) (pr2 : 'a -> string) (v : 'a mask) : string =
    match v.m_mask with
      | None -> sprintf "%s" (pr v.m_value)
      | Some a -> sprintf "%s/%s" (pr v.m_value) (pr2 a)

  let to_string (x : t) : string =
    let all_fields =
      [ fld_str "dlSrc" string_of_mac x.dlSrc;
        fld_str "dlDst" string_of_mac x.dlDst;
        fld_str "dlTyp" Frenetic_Packet.string_of_dlTyp x.dlTyp;
        (match x.dlVlan with
          | None -> None
          | Some None -> Some "dlVlan = none"
          | Some (Some vlan) -> fld_str "dlVlan" string_of_int (Some vlan));
        fld_str "dlVlanPcp" Frenetic_Packet.string_of_dlVlanPcp x.dlVlanPcp;
        fld_str "nwSrc" (mask_pr Frenetic_Packet.string_of_nwAddr Int32.to_string) x.nwSrc;
        fld_str "nwDst" (mask_pr Frenetic_Packet.string_of_nwAddr Int32.to_string) x.nwDst;
        fld_str "nwProto" Frenetic_Packet.string_of_nwProto x.nwProto;
        fld_str "nwTos" Frenetic_Packet.string_of_nwTos x.nwTos;
        fld_str "tpSrc" Frenetic_Packet.string_of_tpPort x.tpSrc;
        fld_str "tpDst" Frenetic_Packet.string_of_tpPort x.tpDst;
        fld_str "inPort" string_of_portId x.inPort ] in
    let set_fields =
      List.fold_right
        ~f:(fun fo acc -> match fo with None -> acc | Some f -> f :: acc)
        all_fields ~init:[] in
    match set_fields with
      | [] -> "{*}"
      | _ ->  "{" ^ (String.concat ~sep:", " set_fields) ^ "}"
end

module PseudoPort = struct

  type t = pseudoPort with sexp

      (* Physical ports are numbered starting from 1. *)
      cenum ofp_port {
        (* Maximum number of physical switch ports. *)
        OFPP_MAX = 0xff00;

        (*Fake output "ports". *)
        OFPP_IN_PORT = 0xfff8;
        OFPP_TABLE   = 0xfff9;
        OFPP_NORMAL  = 0xfffa;
        OFPP_FLOOD   = 0xfffb;
        OFPP_ALL     = 0xfffc;
        OFPP_CONTROLLER = 0xfffd;
        OFPP_LOCAL   = 0xfffe;
        OFPP_NONE    = 0xffff
      } as uint16_t

  let size_of _ = 2

  (* Pseudo-ports show up in two sorts of places:

     1. As an output port in a flow table action. In which case, the
        wire-format for output actions has a dedicated field for the
        number of bits to send to the controller. the marshal function for
        actions handles extracting the parameter of [Controller] ports.

     2. Everywhere else, it is actually (I think) an error to use Controller
        as a pseudo-port.

     In summary, Controller should be a type apart from the other pseudo-ports.
  *)
  let marshal (t : t) : int = match t with
    | PhysicalPort p -> p
    | InPort -> ofp_port_to_int OFPP_IN_PORT
    | Table -> ofp_port_to_int OFPP_TABLE
    | Normal -> ofp_port_to_int OFPP_NORMAL
    | Flood -> ofp_port_to_int OFPP_FLOOD
    | AllPorts -> ofp_port_to_int OFPP_ALL
    (* see wall of text above *)
    | Controller _ -> ofp_port_to_int OFPP_CONTROLLER
    | Local -> ofp_port_to_int OFPP_LOCAL

  let marshal_optional (t : t option) : int = match t with
    | None -> ofp_port_to_int OFPP_NONE
    | Some x -> marshal x

  let to_string (t : t) : string = match t with
    | PhysicalPort p -> string_of_int p
    | InPort -> "InPort"
    | Table -> "Table"
    | Normal -> "Normal"
    | Flood -> "Flood"
    | AllPorts -> "AllPorts"
    | Controller n -> sprintf "Controller<%d bytes>" n
    | Local -> "Local"

  let make ofp_port_code len =
    match int_to_ofp_port ofp_port_code with
      | Some OFPP_IN_PORT -> InPort
      | Some OFPP_TABLE -> Table
      | Some OFPP_NORMAL -> Normal
      | Some OFPP_FLOOD -> Flood
      | Some OFPP_ALL -> AllPorts
      | Some OFPP_CONTROLLER -> Controller len
      | Some OFPP_LOCAL -> Local
      | _ ->
        if ofp_port_code <= (ofp_port_to_int OFPP_MAX) then
          PhysicalPort ofp_port_code
        else
          raise
            (Unparsable (sprintf "unsupported port number (%d)" ofp_port_code))
end

module Action = struct

  type t = action with sexp

  type sequence = t list

      cstruct ofp_action_header {
        uint16_t typ;
        uint16_t len
      } as big_endian

      cstruct ofp_action_output {
        uint16_t port;
        uint16_t max_len
      } as big_endian

      cstruct ofp_action_vlan_vid {
        uint16_t vlan_vid;
        uint8_t pad[2]
      } as big_endian

      cstruct ofp_action_strip_vlan {
        uint8_t pad[4]
      } as big_endian

      cstruct ofp_action_vlan_pcp {
        uint8_t vlan_pcp;
        uint8_t pad[3]
      } as big_endian

      cstruct ofp_action_dl_addr {
        uint8_t dl_addr[6];
        uint8_t pad[6]
      } as big_endian

      cstruct ofp_action_nw_addr {
        uint32_t nw_addr
      } as big_endian

      cstruct ofp_action_tp_port {
        uint16_t tp_port;
        uint8_t pad[2]
      } as big_endian

      cstruct ofp_action_nw_tos {
        uint8_t nw_tos;
        uint8_t pad[3]
      } as big_endian

      cstruct ofp_action_enqueue {
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

  let type_code (a : t) = match a with
    | Output _ -> OFPAT_OUTPUT
    | SetDlVlan None -> OFPAT_STRIP_VLAN
    | SetDlVlan (Some _) -> OFPAT_SET_VLAN_VID
    | SetDlVlanPcp _ -> OFPAT_SET_VLAN_PCP
    | SetDlSrc _ -> OFPAT_SET_DL_SRC
    | SetDlDst _ -> OFPAT_SET_DL_DST
    | SetNwSrc _ -> OFPAT_SET_NW_SRC
    | SetNwDst _ -> OFPAT_SET_NW_DST
    | SetNwTos _ -> OFPAT_SET_NW_TOS
    | SetTpSrc _ -> OFPAT_SET_TP_SRC
    | SetTpDst _ -> OFPAT_SET_TP_DST
    | Enqueue _ -> OFPAT_ENQUEUE

  let size_of (a : t) =
    let h = sizeof_ofp_action_header in
    let body =
      match a with
        | Output _ -> sizeof_ofp_action_output
        | SetDlVlan None -> sizeof_ofp_action_strip_vlan
        | SetDlVlan (Some _) -> sizeof_ofp_action_vlan_vid
        | SetDlVlanPcp _ -> sizeof_ofp_action_vlan_pcp
        | SetDlSrc _
        | SetDlDst _ -> sizeof_ofp_action_dl_addr
        | SetNwSrc _
        | SetNwDst _ -> sizeof_ofp_action_nw_addr
        | SetNwTos _ -> sizeof_ofp_action_nw_tos
        | SetTpSrc _
        | SetTpDst _ -> sizeof_ofp_action_tp_port
        | Enqueue _ -> sizeof_ofp_action_enqueue in
    h + body

  let size_of_sequence acts = List.fold_left ~f:(+) ~init:0 (List.map ~f:size_of acts)

  let marshal a bits =
    set_ofp_action_header_typ bits (ofp_action_type_to_int (type_code a));
    set_ofp_action_header_len bits (size_of a);
    let bits' = Cstruct.shift bits sizeof_ofp_action_header in
    begin match a with
      | Output pp ->
        set_ofp_action_output_port bits' (PseudoPort.marshal pp);
        set_ofp_action_output_max_len bits'
          (match pp with
            | Controller w -> w
            | _ -> 0)
      | SetNwSrc addr
      | SetNwDst addr -> set_ofp_action_nw_addr_nw_addr bits' addr
      | SetTpSrc pt
      | SetTpDst pt -> set_ofp_action_tp_port_tp_port bits' pt
      | SetDlVlan (Some vid) -> set_ofp_action_vlan_vid_vlan_vid bits' vid
      | SetDlVlan None -> ()
      | SetDlVlanPcp n -> set_ofp_action_vlan_pcp_vlan_pcp bits' n
      | SetNwTos n -> set_ofp_action_nw_tos_nw_tos bits' n
      | SetDlSrc mac
      | SetDlDst mac ->
        set_ofp_action_dl_addr_dl_addr (Frenetic_Packet.bytes_of_mac mac) 0 bits'
      | Enqueue (pp, qid) ->
	set_ofp_action_enqueue_port bits' (PseudoPort.marshal pp);
	set_ofp_action_enqueue_queue_id bits' qid
    end;
    size_of a

  let is_to_controller (act : t) : bool = match act with
    | Output (Controller _) -> true
    | _ -> false

  let move_controller_last (lst : sequence) : sequence =
    let (to_ctrl, not_to_ctrl) = List.partition_tf ~f:is_to_controller lst in
    not_to_ctrl @ to_ctrl

  let to_string (t : t) : string = match t with
    | Output p -> "Output " ^ PseudoPort.to_string p
    | SetDlVlan None -> "SetDlVlan None"
    | SetDlVlan (Some n) -> sprintf "SetDlVlan %d" n
    | SetDlVlanPcp n -> sprintf "SetDlVlanPcp n"
    | SetDlSrc mac -> "SetDlSrc " ^ string_of_mac mac
    | SetDlDst mac -> "SetDlDst " ^ string_of_mac mac
    | SetNwSrc ip -> "SetNwSrc " ^ string_of_ip ip
    | SetNwDst ip -> "SetNwDst " ^ string_of_ip ip
    | SetNwTos d -> sprintf "SetNwTos %x" d
    | SetTpSrc n -> sprintf "SetTpSrc %d" n
    | SetTpDst n -> sprintf "SetTpDst %d" n
    | Enqueue(pp,n) -> sprintf "Enqueue %s %s" (PseudoPort.to_string pp) (Int32.to_string n)

  let sequence_to_string (lst : sequence) : string =
    "[" ^ (String.concat ~sep:"; " (List.map ~f:to_string lst)) ^ "]"

  let _parse bits =
    let length = get_ofp_action_header_len bits in
    let ofp_action_code = get_ofp_action_header_typ bits in
    let bits' = Cstruct.shift bits sizeof_ofp_action_header in
    let act = match int_to_ofp_action_type ofp_action_code with
      | Some OFPAT_OUTPUT ->
        let ofp_port_code = get_ofp_action_output_port bits' in
        let len = get_ofp_action_output_max_len bits' in
        Output (PseudoPort.make ofp_port_code len)
      | Some OFPAT_SET_VLAN_VID ->
        let vid = get_ofp_action_vlan_vid_vlan_vid bits' in
        if vid = vlan_none then
          SetDlVlan None
        else
          SetDlVlan (Some vid)
      | Some OFPAT_SET_VLAN_PCP ->
        SetDlVlanPcp (get_ofp_action_vlan_pcp_vlan_pcp bits')
      | Some OFPAT_STRIP_VLAN -> SetDlVlan None
      | Some OFPAT_SET_DL_SRC ->
        let dl =
          mac_of_bytes
            (Cstruct.to_string (get_ofp_action_dl_addr_dl_addr bits')) in
        SetDlSrc dl
      | Some OFPAT_SET_DL_DST ->
        let dl =
          mac_of_bytes
            (Cstruct.to_string (get_ofp_action_dl_addr_dl_addr bits')) in
        SetDlDst dl
      | Some OFPAT_SET_NW_SRC ->
        SetNwSrc (get_ofp_action_nw_addr_nw_addr bits')
      | Some OFPAT_SET_NW_DST ->
        SetNwDst (get_ofp_action_nw_addr_nw_addr bits')
      | Some OFPAT_SET_NW_TOS ->
        SetNwTos (get_ofp_action_nw_tos_nw_tos bits')
      | Some OFPAT_SET_TP_SRC ->
        SetTpSrc (get_ofp_action_tp_port_tp_port bits')
      | Some OFPAT_SET_TP_DST ->
        SetTpDst (get_ofp_action_tp_port_tp_port bits')
      | Some OFPAT_ENQUEUE ->
	let ofp_port_code = get_ofp_action_enqueue_port bits' in
	Enqueue(PseudoPort.make ofp_port_code 0 (* TODO(jnf): replace with non-dummy *),
		get_ofp_action_enqueue_queue_id bits')
      | None ->
        raise (Unparsable
                 (sprintf "unrecognized ofpat_action_type (%d)" ofp_action_code)) in
    (Cstruct.shift bits length, act)

  let parse bits = snd (_parse bits)

  let rec parse_sequence bits : sequence =
    if Cstruct.len bits = 0 then
      []
    else
      let bits', act = _parse bits in
      act::(parse_sequence bits')

end

module Timeout = struct

  type t = timeout with sexp

  let to_string t = match t with
    | Permanent -> "Permanent"
    | ExpiresAfter n -> Printf.sprintf "ExpiresAfter %d" n

  let size_of _ = 2

  let to_int x = match x with
    | Permanent -> 0
    | ExpiresAfter w -> w

  let of_int d =
    if d = 0 then Permanent else ExpiresAfter d
end


module FlowMod = struct

  module Command = struct

    type t = flowModCommand with sexp

    cenum ofp_flow_mod_command {
      OFPFC_ADD;
      OFPFC_MODIFY;
      OFPFC_MODIFY_STRICT;
      OFPFC_DELETE;
      OFPFC_DELETE_STRICT
    } as uint16_t

    let size_of _ = 2

    let to_string cmd = match cmd with
      | AddFlow -> "ADD"
      | ModFlow -> "MOD"
      | ModStrictFlow -> "MOD_STRICT"
      | DeleteFlow -> "DELETE"
      | DeleteStrictFlow -> "DELETE_STRICT"

    let to_int t = match t with
      | AddFlow -> ofp_flow_mod_command_to_int OFPFC_ADD
      | ModFlow -> ofp_flow_mod_command_to_int OFPFC_MODIFY
      | ModStrictFlow -> ofp_flow_mod_command_to_int OFPFC_MODIFY_STRICT
      | DeleteFlow -> ofp_flow_mod_command_to_int OFPFC_DELETE
      | DeleteStrictFlow -> ofp_flow_mod_command_to_int OFPFC_DELETE_STRICT

    let of_int d =
      let command_code = int_to_ofp_flow_mod_command d in
      match command_code with
        | Some OFPFC_ADD -> AddFlow
        | Some OFPFC_MODIFY -> ModFlow
        | Some OFPFC_MODIFY_STRICT -> ModStrictFlow
        | Some OFPFC_DELETE -> DeleteFlow
        | Some OFPFC_DELETE_STRICT -> DeleteStrictFlow
        | None -> raise
          (Unparsable (Printf.sprintf "unexpected ofp_flow_mod_command %d" d))

  end

  type t = flowMod with sexp

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

  let to_string (m:t) = Printf.sprintf
    "{ command = %s; match = %s; priority = %d; actions = %s; cookie = %Ld;\
       idle_timeout = %s; hard_timeout = %s; notify_when_removed = %B;\
       apply_to_packet = %s; out_port = %s; check_overlap = %B }"
    (Command.to_string m.command)
    (Match.to_string m.pattern)
    m.priority
    (Action.sequence_to_string m.actions)
    m.cookie
    (Timeout.to_string m.idle_timeout)
    (Timeout.to_string m.hard_timeout)
    m.notify_when_removed
    (Frenetic_Util.string_of_option Int32.to_string m.apply_to_packet)
    (Frenetic_Util.string_of_option PseudoPort.to_string m.out_port)
    m.check_overlap

  let size_of (msg:flowMod) =
    (Match.size_of msg.pattern)
    + sizeof_ofp_flow_mod
    + (Action.size_of_sequence msg.actions)

  let flags_to_int (check_overlap : bool) (notify_when_removed : bool) =
    (if check_overlap then 1 lsl 1 else 0) lor
      (if notify_when_removed then 1 lsl 0 else 0)

  let check_overlap_of_flags flags =
    (1 lsl 1) land flags <> 0

  let notify_when_removed_of_flags flags =
    (1 lsl 0) land flags <> 0

  let parse bits =
    let pattern = Match.parse bits in
    let bits = Cstruct.shift bits (Match.size_of pattern) in
    let cookie = get_ofp_flow_mod_cookie bits in
    let command = get_ofp_flow_mod_command bits in
    let idle_timeout = get_ofp_flow_mod_idle_timeout bits in
    let hard_timeout = get_ofp_flow_mod_hard_timeout bits in
    let priority = get_ofp_flow_mod_priority bits in
    let buffer_id = get_ofp_flow_mod_buffer_id bits in
    let out_port = get_ofp_flow_mod_out_port bits in
    let flags = get_ofp_flow_mod_flags bits in
    let bits = Cstruct.shift bits sizeof_ofp_flow_mod in
    let actions = Action.parse_sequence bits in
    { command = Command.of_int command;
      pattern = pattern;
      priority = priority;
      actions = actions;
      cookie = cookie;
      idle_timeout = Timeout.of_int idle_timeout;
      hard_timeout = Timeout.of_int hard_timeout;
      notify_when_removed = notify_when_removed_of_flags flags;
      apply_to_packet =
	(match buffer_id with
	  | -1l -> None
	  | n -> Some n);
      out_port =
	(let open PseudoPort in
	 if ofp_port_to_int OFPP_NONE = out_port then None
     (* XXX(seliopou) does not expect a Controller port, so passing the dummy 0
      * respects the spec.
      *)
     else Some (make out_port 0));
      check_overlap = check_overlap_of_flags flags }

  let marshal (msg:flowMod) bits =
    let bits = Cstruct.shift bits (Match.marshal msg.pattern bits) in
    set_ofp_flow_mod_cookie bits (msg.cookie);
    set_ofp_flow_mod_command bits (Command.to_int msg.command);
    set_ofp_flow_mod_idle_timeout bits (Timeout.to_int msg.idle_timeout);
    set_ofp_flow_mod_hard_timeout bits (Timeout.to_int msg.hard_timeout);
    set_ofp_flow_mod_priority bits (msg.priority);
    set_ofp_flow_mod_buffer_id bits
      (match msg.apply_to_packet with
        | None -> -1l
        | Some bufId -> bufId);
    set_ofp_flow_mod_out_port bits (PseudoPort.marshal_optional msg.out_port);
    set_ofp_flow_mod_flags bits
      (flags_to_int msg.check_overlap msg.notify_when_removed);
    let bits = Cstruct.shift bits sizeof_ofp_flow_mod in
    let _ = List.fold_left
      ~f:(fun bits act ->
        begin match act with
          | Output Table ->
            failwith "OFPP_TABLE not allowed in installed flow"
          | _ -> ()
        end;
        Cstruct.shift bits (Action.marshal act bits))
      ~init:bits
      (Action.move_controller_last msg.actions) in
    size_of msg

end

module Payload = struct

  type t = payload with sexp

  let size_of p = match p with
    | Buffered(_,bytes)
    | NotBuffered bytes ->
      Cstruct.len bytes

  let to_string p =
    match p with
      | Buffered (buf_id,pk) ->
	Printf.sprintf "#%s[%s]"
	  (Int32.to_string buf_id)
	  (Frenetic_Packet.to_string (Frenetic_Packet.parse pk))
      | NotBuffered(pk) ->
	Printf.sprintf "[%s]"
	  (Frenetic_Packet.to_string (Frenetic_Packet.parse pk))

   let marshal p out =
     let _ = match p with
       | Buffered(_,bytes)
       | NotBuffered bytes ->
         Cstruct.blit bytes 0 out 0 (Cstruct.len bytes) in
     size_of p
end

module PacketIn = struct

  module Reason = struct

    type t = packetInReason with sexp

    cenum ofp_reason {
      NO_MATCH = 0;
      ACTION = 1
    } as uint8_t

    let of_int d = match int_to_ofp_reason d with
      | Some NO_MATCH -> NoMatch
      | Some ACTION -> ExplicitSend
      | None -> raise (Unparsable (sprintf "bad reason in packet_in (%d)" d))

    let to_int r = match r with
      | NoMatch -> ofp_reason_to_int NO_MATCH
      | ExplicitSend -> ofp_reason_to_int ACTION

    let to_string r = match r with
      | NoMatch -> "NO_MATCH"
      | ExplicitSend -> "EXPLICIT_SEND" (* XXX(seliopou): inconsistent naming
                                           with respect to standard. Should be
                                           ACTION *)

    let size_of _ = 1

  end

  type t = packetIn with sexp

  cstruct ofp_packet_in {
    uint32_t buffer_id;
    uint16_t total_len;
    uint16_t in_port;
    uint8_t reason;
    uint8_t pad
  } as big_endian

  let to_string pi =
    Printf.sprintf
      "{ in_port = %d; payload = %s }"
      pi.port
      (Payload.to_string pi.input_payload)

  let parse bits =
    let buf_id = match get_ofp_packet_in_buffer_id bits with
      | -1l -> None
      | n -> Some n in
    let total_len = get_ofp_packet_in_total_len bits in
    let in_port = get_ofp_packet_in_in_port bits in
    let reason = Reason.of_int (get_ofp_packet_in_reason bits) in
    let pk = Cstruct.shift bits sizeof_ofp_packet_in in
    let payload = match buf_id with
      | None -> NotBuffered pk
      | Some n -> Buffered (n, pk) in
    { input_payload = payload;
      total_len = total_len;
      port = in_port;
      reason = reason }

  let size_of pi =
    sizeof_ofp_packet_in + Payload.size_of pi.input_payload

  let marshal pi out =
    let buf_id = match pi.input_payload with
      | NotBuffered(_) -> -1l
      | Buffered(n,_) -> n in
    set_ofp_packet_in_buffer_id out buf_id;
    set_ofp_packet_in_total_len out pi.total_len;
    set_ofp_packet_in_in_port out pi.port;
    set_ofp_packet_in_reason out (Reason.to_int pi.reason);
    let out = Cstruct.shift out sizeof_ofp_packet_in in
    let _ = Payload.marshal pi.input_payload out in
    size_of pi
end

module FlowRemoved = struct

  module Reason = struct

    type t = flowRemovedReason with sexp

    cenum ofp_flow_removed_reason {
      IDLE_TIMEOUT = 0;
      HARD_TIMEOUT = 1;
      DELETE = 2
    } as uint8_t

    let of_int d = match int_to_ofp_flow_removed_reason d with
      | Some IDLE_TIMEOUT -> IdleTimeout
      | Some HARD_TIMEOUT -> HardTimeout
      | Some DELETE -> Delete
      | None -> raise (Unparsable (sprintf "bad reason in flow_removed (%d)" d))

    let to_int r = match r with
      | IdleTimeout -> ofp_flow_removed_reason_to_int IDLE_TIMEOUT
      | HardTimeout -> ofp_flow_removed_reason_to_int HARD_TIMEOUT
      | Delete -> ofp_flow_removed_reason_to_int DELETE

    let to_string r = match r with
      | IdleTimeout -> "IDLE_TIMEOUT"
      | HardTimeout -> "HARD_TIMEOUT"
      | Delete -> "DELETE"

    let size_of _ = 1

  end

  type t = flowRemoved with sexp

  cstruct ofp_flow_removed {
    uint64_t cookie;
    uint16_t priority;
    uint8_t reason;
    uint8_t pad[1];
    uint32_t duration_sec;
    uint32_t duration_nsec;
    uint16_t idle_timeout;
    uint8_t pad2[2];
    uint64_t packet_count;
    uint64_t byte_count
  } as big_endian

  let parse bits =
    let pattern = Match.parse bits in
    let bits = Cstruct.shift bits (Match.size_of pattern) in
    let cookie = get_ofp_flow_removed_cookie bits in
    let priority = get_ofp_flow_removed_priority bits in
    let reason = Reason.of_int (get_ofp_flow_removed_reason bits) in
    let duration_sec = get_ofp_flow_removed_duration_sec bits in
    let duration_nsec = get_ofp_flow_removed_duration_nsec bits in
    let idle_timeout = Timeout.of_int (get_ofp_flow_removed_idle_timeout bits) in
    let packet_count = get_ofp_flow_removed_packet_count bits in
    let byte_count = get_ofp_flow_removed_byte_count bits in
    { pattern = pattern
    ; cookie = cookie
    ; priority = priority
    ; reason = reason
    ; duration_sec = duration_sec
    ; duration_nsec = duration_nsec
    ; idle_timeout = idle_timeout
    ; packet_count = packet_count
    ; byte_count = byte_count }

  let to_string msg = Printf.sprintf
    "{ flow = %s; cookie  = %Ld; priority = %d; reason = %s; duration_sec = %ld;\
       duration_nsec = %ld; idle_timeout = %s; packet_count = %Ld; byte_count = %Ld }"
    (Match.to_string msg.pattern)
    msg.cookie
    msg.priority
    (Reason.to_string msg.reason)
    msg.duration_sec
    msg.duration_nsec
    (Timeout.to_string msg.idle_timeout)
    msg.packet_count
    msg.byte_count

  let size_of msg =
    (Match.size_of msg.pattern)
    + sizeof_ofp_flow_removed

  let marshal (msg:t) (bits:Cstruct.t) : int =
    let bits = Cstruct.shift bits (Match.marshal msg.pattern bits) in
    set_ofp_flow_removed_cookie bits (msg.cookie);
    set_ofp_flow_removed_priority bits (msg.priority);
    set_ofp_flow_removed_reason bits (Reason.to_int msg.reason);
    set_ofp_flow_removed_duration_sec bits (msg.duration_sec);
    set_ofp_flow_removed_duration_nsec bits (msg.duration_nsec);
    set_ofp_flow_removed_idle_timeout bits (Timeout.to_int msg.idle_timeout);
    set_ofp_flow_removed_packet_count bits (msg.packet_count);
    set_ofp_flow_removed_byte_count bits (msg.byte_count);
    size_of msg
end

module PacketOut = struct

  type t = packetOut with sexp

  cstruct ofp_packet_out {
    uint32_t buffer_id;
    uint16_t in_port;
    uint16_t actions_len
  } as big_endian

  let to_string out = Printf.sprintf
    "{ payload = ...; port_id = %s; actions = %s }"
    (Frenetic_Util.string_of_option string_of_portId out.port_id)
    (Action.sequence_to_string out.apply_actions)

  let parse bits =
    let buf_id = match get_ofp_packet_out_buffer_id bits with
      | -1l -> None
      | n -> Some n in
    let in_port = get_ofp_packet_out_in_port bits in
    let actions_len = get_ofp_packet_out_actions_len bits in
    let bits_after_actions_len = Cstruct.shift bits sizeof_ofp_packet_out in
    let actions_bits,pk = Cstruct.split bits_after_actions_len actions_len in
    let actions = Action.parse_sequence actions_bits in
    let payload = match buf_id with
      | None -> NotBuffered pk
      | Some n -> Buffered(n,pk) in
    { output_payload = payload;
      port_id =
	(let open PseudoPort in
	if ofp_port_to_int OFPP_NONE = in_port then None
	else Some in_port);
      apply_actions = actions }

  let size_of po =
    sizeof_ofp_packet_out +
      (Action.size_of_sequence po.apply_actions) +
      (Payload.size_of po.output_payload)

  let marshal (pkt_out : t) (buf : Cstruct.t) : int =
    set_ofp_packet_out_buffer_id buf
      (match pkt_out.output_payload with
        | Buffered (n, _) -> n
        | NotBuffered _  -> -1l);
    set_ofp_packet_out_in_port buf
      (PseudoPort.marshal_optional
         (match pkt_out.port_id with
           | Some id -> Some (PhysicalPort id)
           | None -> None));
    set_ofp_packet_out_actions_len buf
      (Action.size_of_sequence pkt_out.apply_actions);
    let buf = List.fold_left
      ~f:(fun buf act -> Cstruct.shift buf (Action.marshal act buf))
      ~init:(Cstruct.shift buf sizeof_ofp_packet_out)
      (Action.move_controller_last pkt_out.apply_actions) in
    let _ = Payload.marshal pkt_out.output_payload buf in
    size_of pkt_out

end


module PortDescription = struct

  module PortConfig = struct

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

    let to_int d =
      let bits = Int32.zero in
      let bits = bit bits 6 d.no_packet_in in
      let bits = bit bits 5 d.no_fwd in
      let bits = bit bits 4 d.no_flood in
      let bits = bit bits 3 d.no_recv_stp in
      let bits = bit bits 2 d.no_recv in
      let bits = bit bits 1 d.no_stp in
      let bits = bit bits 0 d.down in
      bits

    let size_of _ = 4

  end

  module PortState = struct

    module StpState = struct

      let mask = Int32.shift_left 3l 8

      let to_string t =
        match t with
          | Listen -> "LISTEN"
          | Learn -> "LEARN"
          | Forward -> "FORWARD"
          | Block -> "BLOCK"

      let to_int t =
        Int32.shift_left
          (match t with Listen -> 0l | Learn -> 1l | Forward -> 2l | Block -> 3l)
          8

      let of_int d =
        let d_masked = Int32.bit_and d mask in
        if d_masked = to_int Listen then Listen
        else if d_masked = to_int Learn then Learn
        else if d_masked = to_int Forward then Forward
        else if d_masked = to_int Block then Block
        else raise (Unparsable
          (Printf.sprintf "Unexpected ofp_port_state for STP: %ld" d_masked))
    end


    let to_string (p : portState) = Printf.sprintf
      "{ down = %B; \
         stp_state = %s }"
      p.down
      (StpState.to_string p.stp_state)

    let of_int d =
      { down = test_bit 0 d
      ; stp_state = StpState.of_int d }

    let to_int d =
      let bits = StpState.to_int d.stp_state in
      let bits = bit bits 0 d.down in
      bits

    let size_of _ = 4

  end

  module PortFeatures = struct

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

    let to_int f =
      let bits = Int32.zero in
      let bits = bit bits 11 f.pause_asym in
      let bits = bit bits 10 f.pause in
      let bits = bit bits 9 f.autoneg in
      let bits = bit bits 8 f.fiber in
      let bits = bit bits 7 f.copper in
      let bits = bit bits 6 f.f_10GBFD in
      let bits = bit bits 5 f.f_1GBFD in
      let bits = bit bits 4 f.f_1GBHD in
      let bits = bit bits 3 f.f_100MBFD in
      let bits = bit bits 2 f.f_100MBHD in
      let bits = bit bits 1 f.f_10MBFD in
      let bits = bit bits 0 f.f_10MBHD in
      bits
  end

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

  let parse (bits : Cstruct.t) : portDescription =
    let portDescPortNo = get_ofp_phy_port_port_no bits in
    let hw_addr = Frenetic_Packet.mac_of_bytes (Cstruct.to_string (get_ofp_phy_port_hw_addr bits)) in
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

  let marshal pd out =
    set_ofp_phy_port_port_no out pd.port_no;
    set_ofp_phy_port_hw_addr (bytes_of_mac pd.hw_addr) 0 out;
    set_ofp_phy_port_name pd.name 0 out;
    set_ofp_phy_port_config out (PortConfig.to_int pd.config);
    set_ofp_phy_port_state out (PortState.to_int pd.state);
    set_ofp_phy_port_curr out (PortFeatures.to_int pd.curr);
    set_ofp_phy_port_advertised out (PortFeatures.to_int pd.advertised);
    set_ofp_phy_port_supported out (PortFeatures.to_int pd.supported);
    set_ofp_phy_port_peer out (PortFeatures.to_int pd.peer);
    sizeof_ofp_phy_port
end

module PortStatus = struct

  cstruct ofp_port_status {
      uint8_t reason; (* One of OFPPR_* *)
      uint8_t pad[7]
  } as big_endian

  module ChangeReason = struct

    type t =
      | Add
      | Delete
      | Modify
    with sexp

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
        | Add -> "ADD"
        | Delete -> "DELETE"
        | Modify -> "MODIFY"

    let size_of t = sizeof_ofp_port_status

  end

  type t =
    { reason : ChangeReason.t;
      desc : portDescription 
    } with sexp

  let to_string status = Printf.sprintf
    "{ reason = %s; desc = %s }"
    (ChangeReason.to_string status.reason)
    (PortDescription.to_string status.desc)

  let size_of ps =
    ChangeReason.size_of ps.reason + PortDescription.size_of ps.desc

  let parse bits0 =
    let reason = ChangeReason.of_int (get_ofp_port_status_reason bits0) in
    let bits1 = Cstruct.shift bits0 sizeof_ofp_port_status in
    let description = PortDescription.parse bits1 in
    { reason = reason
    ; desc = description }

  let marshal ps bits0 =
    set_ofp_port_status_reason bits0 (ChangeReason.to_int ps.reason);
    let bits1 = Cstruct.shift bits0 sizeof_ofp_port_status in
    let _ = PortDescription.marshal ps.desc bits1 in
    size_of ps
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
    ; inPort : bool 
    } with sexp

  module Capabilities = struct

    type t =
      { flow_stats : bool
      ; table_stats : bool
      ; port_stats : bool
      ; stp : bool
      ; ip_reasm : bool
      ; queue_stats : bool
      ; arp_match_ip : bool 
      } with sexp

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
    with sexp

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
    ; ports : portDescription list }
  with sexp

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
    (Frenetic_Util.string_of_list PortDescription.to_string feats.ports)

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
    + List.sum (module Int) ~f:ident (List.map ~f:PortDescription.size_of feats.ports)

  let marshal feats out =
    set_ofp_switch_features_datapath_id out feats.switch_id;
    set_ofp_switch_features_n_buffers out feats.num_buffers;
    set_ofp_switch_features_n_tables out feats.num_tables;
    set_ofp_switch_features_capabilities out
      (Capabilities.to_int feats.supported_capabilities);
    set_ofp_switch_features_action out
      (SupportedActions.to_int feats.supported_actions);
    let _ =
      List.fold_left
	~f:(fun out port -> Cstruct.shift out (PortDescription.marshal port out))
	~init:(Cstruct.shift out sizeof_ofp_switch_features) feats.ports in
    size_of feats

end

module SwitchConfig = struct

  module FragFlags = struct

    type t =
      | FragNormal
      | FragDrop
      | FragReassemble
    with sexp

    let of_int d = match d with
      | 0 -> FragNormal
      | 1 -> FragDrop
      | 2 -> FragReassemble
      | _ -> raise (Unparsable "malformed frag flags")

    let to_int f = match f with
      | FragNormal -> 0
      | FragDrop -> 1
      | FragReassemble -> 2

    let to_string f = match f with
      | FragNormal -> "FRAG_NORMAL"
      | FragDrop -> "FRAG_DROP"
      | FragReassemble -> "FRAG_REASSEMBLE"

  end

  type t = { frag_flags : FragFlags.t;
	     miss_send_len : int }
  with sexp

      cstruct ofp_switch_config {
	uint16_t flags;
	uint16_t miss_send_len
      } as big_endian

  let size_of _ = sizeof_ofp_switch_config

  let parse buf =
    let frag_flags = get_ofp_switch_config_flags buf in
    let miss_send_len = get_ofp_switch_config_miss_send_len buf in
    { frag_flags = FragFlags.of_int frag_flags;
      miss_send_len = miss_send_len }

  let marshal sc buf =
    set_ofp_switch_config_flags buf (FragFlags.to_int sc.frag_flags);
    set_ofp_switch_config_miss_send_len buf sc.miss_send_len;
    size_of sc

  let to_string sc =
    Printf.sprintf
      "{ frag_flags = %s; miss_send_len = %d }"
      (FragFlags.to_string sc.frag_flags)
      sc.miss_send_len
end


let reply_to_string  = Format.string_of_mk Format.reply


module StatsRequest = struct

  type t = request
  with sexp

  cstruct ofp_stats_request {
    uint16_t req_type;
    uint16_t flags
  } as big_endian

  cstruct ofp_flow_stats_request {
    uint8_t of_match[40];
    uint8_t table_id;
    uint8_t pad;
    uint16_t out_port
  } as big_endian

  cstruct ofp_port_stats_request {
    uint16_t port_no;
    uint8_t pad[6];
  } as big_endian

  let marshal_flow_stats_request pat port table out =
    let _ = Match.marshal pat out in
    set_ofp_flow_stats_request_table_id out table;
    begin match port with
      | Some port ->
        set_ofp_flow_stats_request_out_port out (PseudoPort.marshal port);
      | None ->
        let open PseudoPort in
        let port_code = ofp_port_to_int OFPP_NONE in
        set_ofp_flow_stats_request_out_port out port_code
    end;
    sizeof_ofp_flow_stats_request

  let marshal_port_stats_request port out =
    begin match port with
      | Some port ->
        set_ofp_port_stats_request_port_no out (PseudoPort.marshal port)
      | None ->
        let open PseudoPort in
        let port_code = ofp_port_to_int OFPP_NONE in
        set_ofp_port_stats_request_port_no out port_code
    end;
    sizeof_ofp_port_stats_request

  let to_string msg = match msg with
    | DescriptionRequest ->
      "DescriptionReq"
    | FlowTableStatsRequest ->
      "FlowTableStatsReq"
    | IndividualRequest _ ->
      "IndividualFlowReq "
    | AggregateRequest _ ->
      "AggregateFlowReq "
    | PortRequest _ ->
      "PortRequest "

  let size_of msg =
    let header_size = sizeof_ofp_stats_request in
    match msg with
    | DescriptionRequest -> header_size
    | FlowTableStatsRequest -> header_size
    | AggregateRequest _
    | IndividualRequest _ -> header_size + sizeof_ofp_flow_stats_request
    | PortRequest _ -> header_size + sizeof_ofp_port_stats_request

  let ofp_stats_type_of_request req = match req with
    | DescriptionRequest -> OFPST_DESC
    | FlowTableStatsRequest -> OFPST_TABLE
    | IndividualRequest _ -> OFPST_FLOW
    | AggregateRequest _ -> OFPST_AGGREGATE
    | PortRequest _ -> OFPST_PORT

  let marshal (msg : request) (out : Cstruct.t) =
    let req_type = ofp_stats_type_of_request msg in
    let flags = 0x0 in
    set_ofp_stats_request_req_type out (ofp_stats_types_to_int req_type);
    set_ofp_stats_request_flags out flags;
    let out' = Cstruct.shift out sizeof_ofp_stats_request in
    match msg with
    | DescriptionRequest -> sizeof_ofp_stats_request
    | FlowTableStatsRequest -> sizeof_ofp_stats_request
    | IndividualRequest stats_req
    | AggregateRequest  stats_req ->
      marshal_flow_stats_request stats_req.sr_of_match stats_req.sr_out_port stats_req.sr_table_id out'
    | PortRequest port ->
      marshal_port_stats_request port out'

  let parse_stats_request bits =
    let sr_of_match = Match.parse (get_ofp_flow_stats_request_of_match bits) in
    let sr_table_id = get_ofp_flow_stats_request_table_id bits in
    let sr_out_port =
      (let open PseudoPort in
      if ofp_port_to_int OFPP_NONE = (get_ofp_flow_stats_request_out_port bits) then
        None
      else
        Some (PhysicalPort (get_ofp_flow_stats_request_out_port bits)))
    in
    { sr_of_match; sr_table_id; sr_out_port }

  let parse_port_request bits =
    let open PseudoPort in
    if ofp_port_to_int OFPP_NONE = (get_ofp_flow_stats_request_out_port bits) then
      None
    else
      Some (PhysicalPort (get_ofp_flow_stats_request_out_port bits))

  let parse bits =
    let stats_type_code = get_ofp_stats_request_req_type bits in
    let body = Cstruct.shift bits sizeof_ofp_stats_request in
    match int_to_ofp_stats_types stats_type_code with
    | Some OFPST_DESC -> DescriptionRequest
    | Some OFPST_TABLE -> FlowTableStatsRequest
    | Some OFPST_FLOW -> IndividualRequest (parse_stats_request body)
    | Some OFPST_AGGREGATE -> AggregateRequest (parse_stats_request body)
    | Some OFPST_QUEUE -> raise (Unparsable "queue statistics unsupported")
    | Some OFPST_VENDOR -> raise (Unparsable "vendor statistics unsupported")
    | Some OFPST_PORT -> PortRequest (parse_port_request body)
    | None ->
      let msg =
        sprintf "bad ofp_stats_type in stats_request (%d)" stats_type_code in
      raise (Unparsable msg)
end

module StatsReply = struct

  type t = reply
  with sexp

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
      let new_string = Bytes.create size in
      Cstruct.blit_to_string bits 0 new_string 0 size;
      new_string

    let parse_description_stats bits =
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

    let size_of_description_stats _ = sizeof_ofp_desc_stats

    (** Reply to an ofp_stats_request of type OFPST_FLOW *)
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

    let to_string_individual_stats stats = Printf.sprintf
      "{ table_id = %d; of_match = %s; duration_sec = %d; duration_nsec = %d\
         priority = %d; idle_timeout = %d; hard_timeout = %d; cookie = %Ld\
         packet_count = %Ld; byte_count = %Ld; actions = %s }"
      stats.table_id
      (Match.to_string stats.of_match)
      (Int32.to_int_exn stats.duration_sec)
      (Int32.to_int_exn stats.duration_nsec)
      stats.priority
      stats.idle_timeout
      stats.hard_timeout
      stats.cookie
      stats.packet_count
      stats.byte_count
      (Action.sequence_to_string stats.actions)

    let sequence_to_string = Frenetic_Util.string_of_list to_string

    let _parse_individual_stats bits =
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

    let parse_individual_stats bits = fst (_parse_individual_stats bits)

    let rec parse_sequence_individual_stats bits =
      if Cstruct.len bits <= 0 then
        []
      else
        let (v, bits') = _parse_individual_stats bits in
        v :: parse_sequence_individual_stats bits'

    cstruct ofp_aggregate_stats {
      uint64_t packet_count;
      uint64_t byte_count;
      uint32_t flow_count;
      uint8_t pad[4]
    } as big_endian

    let parse_aggregate_stats bits =
      { total_packet_count = get_ofp_aggregate_stats_packet_count bits;
        total_byte_count = get_ofp_aggregate_stats_byte_count bits;
        flow_count = get_ofp_aggregate_stats_flow_count bits }

  (** Reply to an ofp_stats_request of type OFPST_AGGREGATE *)

  cstruct ofp_port_stats {
    uint16_t port_no;
    uint8_t pad[6];
    uint64_t rx_packets;
    uint64_t tx_packets;
    uint64_t rx_bytes;
    uint64_t tx_bytes;
    uint64_t rx_dropped;
    uint64_t tx_dropped;
    uint64_t rx_errors;
    uint64_t tx_errors;
    uint64_t rx_frame_err;
    uint64_t rx_over_err;
    uint64_t rx_crc_err;
    uint64_t collisions;
  } as big_endian

 let _parse_port_stats bits =
    (* get fields *)
    let port_no = get_ofp_port_stats_port_no bits in
    let rx_packets = get_ofp_port_stats_rx_packets bits in
    let tx_packets = get_ofp_port_stats_tx_packets bits in
    let rx_bytes = get_ofp_port_stats_rx_bytes bits in
    let tx_bytes = get_ofp_port_stats_tx_bytes bits in
    let rx_dropped = get_ofp_port_stats_rx_dropped bits in
    let tx_dropped = get_ofp_port_stats_tx_dropped bits in
    let rx_errors = get_ofp_port_stats_rx_errors bits in
    let tx_errors = get_ofp_port_stats_tx_errors bits in
    let rx_frame_err = get_ofp_port_stats_rx_frame_err bits in
    let rx_over_err = get_ofp_port_stats_rx_over_err bits in
    let rx_crc_err = get_ofp_port_stats_rx_crc_err bits in
    let collisions = get_ofp_port_stats_collisions bits in

    let bits_after_port_stats = Cstruct.shift bits sizeof_ofp_port_stats in

    ( { port_no = port_no
     ; rx_packets = rx_packets
     ; tx_packets = tx_packets
     ; rx_bytes = rx_bytes
     ; tx_bytes = tx_bytes
     ; rx_dropped = rx_dropped
     ; tx_dropped = tx_dropped
     ; rx_errors = rx_errors
     ; tx_errors = tx_errors
     ; rx_frame_err = rx_frame_err
     ; rx_over_err = rx_over_err
     ; rx_crc_err = rx_crc_err
     ; collisions = collisions
    }, bits_after_port_stats)

  let rec parse_sequence_port_stats bits =
    if Cstruct.len bits <= 0 then
      []
    else
      let (v, bits') = _parse_port_stats bits in
      v :: parse_sequence_port_stats bits'

  cstruct ofp_stats_reply {
    uint16_t stats_type;
    uint16_t flags
  } as big_endian

  let parse bits =
    let stats_type_code = get_ofp_stats_reply_stats_type bits in
    let body = Cstruct.shift bits sizeof_ofp_stats_reply in
    match int_to_ofp_stats_types stats_type_code with
    | Some OFPST_DESC -> DescriptionRep (parse_description_stats body)
    | Some OFPST_FLOW ->
      IndividualFlowRep (parse_sequence_individual_stats body)
    | Some OFPST_AGGREGATE ->
      AggregateFlowRep (parse_aggregate_stats body)
    | Some OFPST_TABLE -> raise (Unparsable "table statistics unsupported")
    | Some OFPST_QUEUE -> raise (Unparsable "queue statistics unsupported")
    | Some OFPST_VENDOR -> raise (Unparsable "vendor statistics unsupported")
    | Some OFPST_PORT -> PortRep (parse_sequence_port_stats body)
    | None ->
      let msg =
        sprintf "bad ofp_stats_type in stats_reply (%d)" stats_type_code in
      raise (Unparsable msg)

  let marshal msg out =
    begin match msg with
      | DescriptionRep rep ->
        set_ofp_stats_reply_stats_type out (ofp_stats_types_to_int OFPST_DESC);
        begin let out = Cstruct.shift out sizeof_ofp_stats_reply in
          set_ofp_desc_stats_mfr_desc (rep.manufacturer) 0 out;
          set_ofp_desc_stats_hw_desc (rep.hardware) 0 out;
          set_ofp_desc_stats_sw_desc (rep.software) 0 out;
          set_ofp_desc_stats_serial_num (rep.serial_number) 0 out;
          set_ofp_desc_stats_dp_desc (rep.datapath) 0 out;
        end;
        sizeof_ofp_stats_reply + sizeof_ofp_desc_stats
      | AggregateFlowRep rep ->
        set_ofp_stats_reply_stats_type out (ofp_stats_types_to_int OFPST_AGGREGATE);
        begin let out = Cstruct.shift out sizeof_ofp_stats_reply in
          set_ofp_aggregate_stats_packet_count out (rep.total_packet_count);
          set_ofp_aggregate_stats_byte_count out (rep.total_byte_count);
          set_ofp_aggregate_stats_flow_count out (rep.flow_count)
        end;
        sizeof_ofp_stats_reply + sizeof_ofp_aggregate_stats
      | IndividualFlowRep rep ->
        set_ofp_stats_reply_stats_type out (ofp_stats_types_to_int OFPST_FLOW);
        begin let out = Cstruct.shift out sizeof_ofp_stats_reply in
          match rep with
          | [] -> set_ofp_flow_stats_length out (sizeof_ofp_flow_stats)
          | head :: tail ->
              set_ofp_flow_stats_length out (sizeof_ofp_flow_stats);
              set_ofp_flow_stats_table_id out (head.table_id);
              let out = Cstruct.shift out (Match.marshal head.of_match out) in
              set_ofp_flow_stats_duration_sec out (head.duration_sec);
              set_ofp_flow_stats_duration_nsec out (head.duration_nsec);
              set_ofp_flow_stats_priority out (head.priority);
              set_ofp_flow_stats_idle_timeout out (head.idle_timeout);
              set_ofp_flow_stats_hard_timeout out (head.hard_timeout);
              set_ofp_flow_stats_cookie out (head.cookie);
              set_ofp_flow_stats_packet_count out (head.packet_count);
              set_ofp_flow_stats_byte_count out (head.byte_count)
        end;
        (** TODO: Support the marshaling of multiple action and multiple flows *)
        sizeof_ofp_stats_reply + sizeof_ofp_flow_stats
      | PortRep rep ->
        set_ofp_stats_reply_stats_type out (ofp_stats_types_to_int OFPST_PORT);
        begin 
          let out = Cstruct.shift out sizeof_ofp_stats_reply in
          match rep with 
          | [] -> ()  (* DOn't really know yet *)
          | head :: tail -> 
            set_ofp_port_stats_port_no out (head.port_no); 
            set_ofp_port_stats_rx_packets out (head.rx_packets);
            set_ofp_port_stats_tx_packets out (head.tx_packets);
            set_ofp_port_stats_rx_bytes out (head.rx_bytes);
            set_ofp_port_stats_tx_bytes out (head.tx_bytes);
            set_ofp_port_stats_rx_dropped out (head.rx_dropped);
            set_ofp_port_stats_tx_dropped out (head.tx_dropped);
            set_ofp_port_stats_rx_errors out (head.rx_errors);
            set_ofp_port_stats_tx_errors out (head.tx_errors);
            set_ofp_port_stats_rx_frame_err out (head.rx_frame_err);
            set_ofp_port_stats_rx_over_err out (head.rx_over_err);
            set_ofp_port_stats_rx_crc_err out (head.rx_crc_err);
            set_ofp_port_stats_collisions out (head.collisions);
        end;
        (** TODO: Support the marshaling of multiple action and multiple flows *)
        sizeof_ofp_stats_reply + sizeof_ofp_port_stats
    end

  let to_string (t : t) = reply_to_string t

  let size_of (a : t) = match a with
    | DescriptionRep _ -> sizeof_ofp_stats_reply + sizeof_ofp_desc_stats
    | IndividualFlowRep _ -> sizeof_ofp_stats_reply + sizeof_ofp_flow_stats
    | AggregateFlowRep _ -> sizeof_ofp_stats_reply + sizeof_ofp_aggregate_stats
    | PortRep _ -> sizeof_ofp_stats_reply + sizeof_ofp_port_stats
end

(* See Section 5.4.4 of the OpenFlow 1.0 specification *)
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

  cenum ofp_hello_failed_code {
    OFPHFC_INCOMPATIBLE;
    OFPHFC_EPERM
  } as uint16_t

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

  cenum ofp_flow_mod_failed_code {
    OFPFMFC_ALL_TABLES_FULL;
    OFPFMFC_OVERLAP;
    OFPFMFC_EPERM;
    OFPFMFC_BAD_EMERG_TIMEOUT;
    OFPFMFC_BAD_COMMAND;
    OFPFMFC_UNSUPPORTED
  } as uint16_t

  cenum ofp_port_mod_failed_code {
    OFPPMFC_BAD_PORT;
    OFPPMFC_BAD_HW_ADDR
  } as uint16_t

  cenum ofp_queue_op_failed_code {
    OFPQOFC_BAD_PORT;
    OFPQOFC_BAD_QUEUE;
    OFPQOFC_EPERM
  } as uint16_t

  module HelloFailed = struct

    type t =
      | Incompatible
      | Eperm
    with sexp

    let type_code (a : t) = match a with
      | Incompatible -> OFPHFC_INCOMPATIBLE
      | Eperm -> OFPHFC_EPERM

    let of_int error_code =
      match int_to_ofp_hello_failed_code error_code with
      | Some OFPHFC_INCOMPATIBLE -> Incompatible
      | Some OFPHFC_EPERM -> Eperm
      | None ->
        let msg = sprintf "bad ofp_hello_failed_code in error (%d)" error_code in
        raise (Unparsable msg)

    let to_string e = match e with
      | Incompatible -> "INCOMPATIBLE"
      | Eperm -> "EPERM"

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
    with sexp

    let type_code (a : t) = match a with
      | BadVersion -> OFPBRC_BAD_VERSION
      | BadType -> OFPBRC_BAD_TYPE
      | BadStat -> OFPBRC_BAD_STAT
      | BadVendor -> OFPBRC_BAD_VENDOR
      | BadSubType -> OFPBRC_BAD_SUBTYPE
      | Eperm -> OFPBRC_EPERM
      | BadLen -> OFPBRC_BAD_LEN
      | BufferEmpty -> OFPBRC_BUFFER_EMPTY
      | BufferUnknown -> OFPBRC_BUFFER_UNKNOWN

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
        let msg = sprintf "bad ofp_bad_request_code in error (%d)" error_code in
              raise (Unparsable msg)

    let to_string r = match r with
      | BadVersion -> "BAD_VERSION"
      | BadType -> "BAD_TYPE"
      | BadStat -> "BAD_STAT"
      | BadVendor -> "BAD_VENDOR"
      | BadSubType -> "BAD_SUBTYPE"
      | Eperm -> "EPERM"
      | BadLen -> "BAD_LEN"
      | BufferEmpty -> "BUFFER_EMPTY"
      | BufferUnknown -> "BUFFER_UNKNOWN"

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
    with sexp

     let type_code (a : t) = match a with
       | BadType -> OFPBAC_BAD_TYPE
       | BadLen -> OFPBAC_BAD_LEN
       | BadVendor -> OFPBAC_BAD_VENDOR
       | BadVendorType -> OFPBAC_BAD_VENDOR_TYPE
       | BadOutPort -> OFPBAC_BAD_OUT_PORT
       | BadArgument -> OFPBAC_BAD_ARGUMENT
       | Eperm -> OFPBAC_EPERM
       | TooMany -> OFPBAC_TOO_MANY
       | BadQueue -> OFPBAC_BAD_QUEUE

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
        let msg = sprintf "bad ofp_bad_action_code in error (%d)" error_code in
              raise (Unparsable msg)

    let to_string a = match a with
      | BadType -> "BAD_TYPE"
      | BadLen -> "BAD_LEN"
      | BadVendor -> "BAD_VENDOR"
      | BadVendorType -> "BAD_VENDORTYPE"
      | BadOutPort -> "BAD_OUTPORT"
      | BadArgument -> "BAD_ARGUMENT"
      | Eperm -> "EPERM"
      | TooMany -> "TOO_MANY"
      | BadQueue -> "BAD_QUEUE"

  end

  module FlowModFailed = struct

    type t =
      | AllTablesFull
      | Overlap
      | Eperm
      | BadEmergTimeout
      | BadCommand
      | Unsupported
    with sexp

    let type_code (a : t) = match a with
      | AllTablesFull -> OFPFMFC_ALL_TABLES_FULL
      | Overlap -> OFPFMFC_OVERLAP
      | Eperm -> OFPFMFC_EPERM
      | BadEmergTimeout -> OFPFMFC_BAD_EMERG_TIMEOUT
      | BadCommand -> OFPFMFC_BAD_COMMAND
      | Unsupported -> OFPFMFC_UNSUPPORTED

    let of_int error_code =
      match int_to_ofp_flow_mod_failed_code error_code with
      | Some OFPFMFC_ALL_TABLES_FULL -> AllTablesFull
      | Some OFPFMFC_OVERLAP -> Overlap
      | Some OFPFMFC_EPERM -> Eperm
      | Some OFPFMFC_BAD_EMERG_TIMEOUT -> BadEmergTimeout
      | Some OFPFMFC_BAD_COMMAND -> BadCommand
      | Some OFPFMFC_UNSUPPORTED -> Unsupported
      | None ->
        let msg = sprintf "bad ofp_flow_mod_failed_code in error (%d)" error_code in
              raise (Unparsable msg)

    let to_string f = match f with
      | AllTablesFull -> "ALL_TABLES_FULL"
      | Overlap -> "OVERLAP"
      | Eperm -> "EPERM"
      | BadEmergTimeout -> "BAD_EMERG_TIMEOUT"
      | BadCommand -> "BAD_COMMAND"
      | Unsupported -> "UNSUPPORTED"

  end

  module PortModFailed = struct

    type t =
      | BadPort
      | BadHwAddr
    with sexp

    let type_code (a : t) = match a with
      | BadPort -> OFPPMFC_BAD_PORT
      | BadHwAddr -> OFPPMFC_BAD_HW_ADDR

    let of_int error_code =
      match int_to_ofp_port_mod_failed_code error_code with
      | Some OFPPMFC_BAD_PORT -> BadPort
      | Some OFPPMFC_BAD_HW_ADDR -> BadHwAddr
      | None ->
        let msg = sprintf "bad ofp_port_mod_failed_code in error (%d)" error_code in
              raise (Unparsable msg)

    let to_string = function
      | BadPort -> "BAD_PORT"
      | BadHwAddr -> "BAD_HW_ADDR"

  end

  module QueueOpFailed = struct

    type t =
      | BadPort
      | BadQueue
      | Eperm
    with sexp

    let type_code (a : t) = match a with
      | BadPort -> OFPQOFC_BAD_PORT
      | BadQueue -> OFPQOFC_BAD_QUEUE
      | Eperm -> OFPQOFC_EPERM

    let of_int error_code =
      match int_to_ofp_queue_op_failed_code error_code with
      | Some OFPQOFC_BAD_PORT -> BadPort
      | Some OFPQOFC_BAD_QUEUE -> BadQueue
      | Some OFPQOFC_EPERM -> Eperm
      | None ->
        let msg = sprintf "bad ofp_queue_op_failed_code in error (%d)" error_code in
              raise (Unparsable msg)

    let to_string = function
      | BadPort -> "BAD_PORT"
      | BadQueue -> "BAD_QUEUE"
      | Eperm -> "EPERM"

  end

  let size_of _ = sizeof_ofp_error_msg

  (* Each error is composed of a pair (error_code, data) *)
  type c =
    | HelloFailed of HelloFailed.t
    | BadRequest of BadRequest.t
    | BadAction of BadAction.t
    | FlowModFailed of FlowModFailed.t
    | PortModFailed of PortModFailed.t
    | QueueOpFailed of QueueOpFailed.t
  with sexp

  type t =
    | Error of c * Cstruct.t sexp_opaque
  with sexp

  let parse bits =
    let error_type = get_ofp_error_msg_error_type bits in
    let error_code = get_ofp_error_msg_error_code bits in
    let body = Cstruct.shift bits sizeof_ofp_error_msg in
    let code = match int_to_ofp_error_type error_type with
    | Some OFPET_HELLO_FAILED ->
      HelloFailed (HelloFailed.of_int error_code)
    | Some OFPET_BAD_REQUEST ->
      BadRequest (BadRequest.of_int error_code)
    | Some OFPET_BAD_ACTION ->
      BadAction (BadAction.of_int error_code)
    | Some OFPET_FLOW_MOD_FAILED ->
      FlowModFailed (FlowModFailed.of_int error_code)
    | Some OFPET_PORT_MOD_FAILED ->
      PortModFailed (PortModFailed.of_int error_code)
    | Some OFPET_QUEUE_OP_FAILED ->
      QueueOpFailed (QueueOpFailed.of_int error_code)
    | None ->
      let msg =
        sprintf "bad ofp_error_type in ofp_error_msg (%d)" error_type in
      raise(Unparsable msg)
    in
      Error(code, body)

  let marshal (Error(code, body)) out =
    begin match code with
      | HelloFailed error_code ->
        set_ofp_error_msg_error_type out (ofp_error_type_to_int OFPET_HELLO_FAILED);
        set_ofp_error_msg_error_code out (ofp_hello_failed_code_to_int (HelloFailed.type_code error_code))
      | BadRequest error_code ->
        set_ofp_error_msg_error_type out (ofp_error_type_to_int OFPET_BAD_REQUEST);
        set_ofp_error_msg_error_code out (ofp_bad_request_code_to_int (BadRequest.type_code error_code))
      | BadAction error_code ->
        set_ofp_error_msg_error_type out (ofp_error_type_to_int OFPET_BAD_ACTION);
        set_ofp_error_msg_error_code out (ofp_bad_action_code_to_int (BadAction.type_code error_code))
      | FlowModFailed error_code ->
        set_ofp_error_msg_error_type out (ofp_error_type_to_int OFPET_FLOW_MOD_FAILED);
        set_ofp_error_msg_error_code out (ofp_flow_mod_failed_code_to_int (FlowModFailed.type_code error_code))
      | PortModFailed error_code ->
        set_ofp_error_msg_error_type out (ofp_error_type_to_int OFPET_PORT_MOD_FAILED);
        set_ofp_error_msg_error_code out (ofp_port_mod_failed_code_to_int (PortModFailed.type_code error_code))
      | QueueOpFailed error_code ->
        set_ofp_error_msg_error_type out (ofp_error_type_to_int OFPET_QUEUE_OP_FAILED);
        set_ofp_error_msg_error_code out (ofp_queue_op_failed_code_to_int (QueueOpFailed.type_code error_code))
    end;
    let out = Cstruct.shift out sizeof_ofp_error_msg in
    let _ = Cstruct.blit body 0 out 0 (Cstruct.len body) in
      sizeof_ofp_error_msg + Cstruct.len body



  let to_string (Error(code, body)) =
    let len = Cstruct.len body in
    let t, c = match code with
      | HelloFailed code ->
        ("HELLO_FAILED", HelloFailed.to_string code)
      | BadRequest code ->
        ("BAD_REQUEST", BadRequest.to_string code)
      | BadAction code ->
        ("BAD_ACTION", BadAction.to_string code)
      | FlowModFailed code ->
        ("FLOW_MOD_FAILED", FlowModFailed.to_string code)
      | PortModFailed code ->
        ("PORT_MOD_FAILED", PortModFailed.to_string code)
      | QueueOpFailed code ->
        ("QUEUE_OP_FAILED", QueueOpFailed.to_string code) in
    Printf.sprintf "{ type = %s; code = %s; data = <bytes:%d> }" t c len
end

module Vendor = struct

  type t = int32 * Cstruct.t sexp_opaque
  with sexp

  cstruct ofp_vendor_header {
    uint32_t vendor
  } as big_endian

  let parse bits =
    let vendor = get_ofp_vendor_header_vendor bits in
    let body = Cstruct.shift bits sizeof_ofp_vendor_header in
    (vendor, body)

  let marshal msg out =
    let (vendor, body) = msg in
    set_ofp_vendor_header_vendor out (vendor);
    let out = Cstruct.shift out sizeof_ofp_vendor_header in
    let _ = Cstruct.blit (body) 0 out 0 (Cstruct.len (body)) in
    12

  let size_of _ = 12

  let to_string (id, bytes) =
    Printf.sprintf "{ vendor = %lu; data = <bytes:%d> }" id (Cstruct.len bytes)

end

module Message = struct
(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

  module Header = Frenetic_OpenFlow_Header

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

  type t =
    | Hello of Cstruct.t
    | ErrorMsg of Error.t
    | EchoRequest of Cstruct.t
    | EchoReply of Cstruct.t
    | VendorMsg of Vendor.t
    | SwitchFeaturesRequest
    | SwitchFeaturesReply of SwitchFeatures.t
    | FlowModMsg of FlowMod.t
    | PacketInMsg of PacketIn.t
    | FlowRemovedMsg of FlowRemoved.t
    | PortStatusMsg of PortStatus.t
    | PacketOutMsg of PacketOut.t
    | BarrierRequest
    | BarrierReply
    | StatsRequestMsg of StatsRequest.t
    | StatsReplyMsg of StatsReply.t
    | SetConfig of SwitchConfig.t
    | ConfigRequestMsg
    | ConfigReplyMsg of SwitchConfig.t
  with sexp
        
  let parse (hdr : Header.t) (body_buf : string) : (xid * t) =
    let buf = Cstruct.of_string body_buf in
    let code = match int_to_msg_code (hdr.Header.type_code) with
      | Some code -> code
      | None -> raise (Unparsable "unrecognized message code") in
    let msg = match code with
      | HELLO -> Hello buf
      | ERROR -> ErrorMsg (Error.parse buf)
      | ECHO_REQ -> EchoRequest buf
      | ECHO_RESP -> EchoReply buf
      | VENDOR -> VendorMsg (Vendor.parse buf)
      | FEATURES_REQ -> SwitchFeaturesRequest
      | FEATURES_RESP -> SwitchFeaturesReply (SwitchFeatures.parse buf)
      | PACKET_IN -> PacketInMsg (PacketIn.parse buf)
      | FLOW_REMOVED -> FlowRemovedMsg (FlowRemoved.parse buf)
      | PORT_STATUS -> PortStatusMsg (PortStatus.parse buf)
      | BARRIER_REQ -> BarrierRequest
      | BARRIER_RESP -> BarrierReply
      | STATS_REQ -> StatsRequestMsg (StatsRequest.parse buf)
      | STATS_RESP -> StatsReplyMsg (StatsReply.parse buf)
      | PACKET_OUT -> PacketOutMsg (PacketOut.parse buf)
      | FLOW_MOD -> FlowModMsg (FlowMod.parse buf)
      | GET_CONFIG_REQ -> ConfigRequestMsg
      | GET_CONFIG_RESP -> ConfigReplyMsg (SwitchConfig.parse buf)
      | SET_CONFIG -> SetConfig (SwitchConfig.parse buf)
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
    | VendorMsg _ -> VENDOR
    | SwitchFeaturesRequest -> FEATURES_REQ
    | SwitchFeaturesReply _ -> FEATURES_RESP
    | FlowModMsg _ -> FLOW_MOD
    | PacketOutMsg _ -> PACKET_OUT
    | PortStatusMsg _ -> PORT_STATUS
    | PacketInMsg _ -> PACKET_IN
    | FlowRemovedMsg _ -> FLOW_REMOVED
    | BarrierRequest -> BARRIER_REQ
    | BarrierReply -> BARRIER_RESP
    | StatsRequestMsg _ -> STATS_REQ
    | StatsReplyMsg _ -> STATS_RESP
    | SetConfig _ -> SET_CONFIG
    | ConfigRequestMsg -> GET_CONFIG_REQ
    | ConfigReplyMsg _ -> GET_CONFIG_RESP

  let to_string (msg : t) : string = match msg with
    | Hello       b -> Printf.sprintf "HELLO { body = <bytes:%d> }" (Cstruct.len b)
    | ErrorMsg    e -> Printf.sprintf "ERROR %s" (Error.to_string e)
    | EchoRequest b -> Printf.sprintf "ECHO_REQUEST { body = <bytes:%d> }" (Cstruct.len b)
    | EchoReply   b -> Printf.sprintf "ECHO_REPLY { body = <bytes:%d> }" (Cstruct.len b)
    | VendorMsg   v -> Printf.sprintf "VENDOR %s" (Vendor.to_string v)
    | SwitchFeaturesRequest -> "FEATURES_REQUEST"
    | SwitchFeaturesReply f -> Printf.sprintf "FEATURES_REPLY %s"
        (SwitchFeatures.to_string f)
    | FlowModMsg m -> Printf.sprintf "FLOW_MOD %s" (FlowMod.to_string m)
    | PacketOutMsg m -> Printf.sprintf "PACKET_OUT %s" (PacketOut.to_string m)
    | PortStatusMsg m -> Printf.sprintf "PORT_STATUS %s" (PortStatus.to_string m)
    | PacketInMsg m -> Printf.sprintf "PACKET_IN %s" (PacketIn.to_string m)
    | FlowRemovedMsg m -> Printf.sprintf "FLOW_REMOVED %s" (FlowRemoved.to_string m)
    | BarrierRequest -> Printf.sprintf "BARRIER_REQUEST"
    | BarrierReply -> Printf.sprintf "BARRIER_REPLY"
    | StatsRequestMsg m -> Printf.sprintf "STATS_REQUEST %s" (StatsRequest.to_string m)
    | StatsReplyMsg m -> Printf.sprintf "STATS_REPLY %s" (StatsReply.to_string m)
    | SetConfig m -> Printf.sprintf "SET_CONFIG %s" (SwitchConfig.to_string m)
    | ConfigRequestMsg -> "CONFIG_REQUEST"
    | ConfigReplyMsg m -> Printf.sprintf "CONFIG_REPLY %s" (SwitchConfig.to_string m)

  open Bigarray

  (** Size of the message body, without the header *)
  let sizeof_body (msg : t) : int = match msg with
    | Hello buf -> Cstruct.len buf
    | EchoRequest buf -> Cstruct.len buf
    | EchoReply buf -> Cstruct.len buf
    | VendorMsg buf -> Vendor.size_of msg
    | SwitchFeaturesRequest -> 0
    | SwitchFeaturesReply rep -> SwitchFeatures.size_of rep
    | FlowModMsg msg -> FlowMod.size_of msg
    | PacketOutMsg msg -> PacketOut.size_of msg
    | BarrierRequest -> 0
    | BarrierReply -> 0
    | StatsRequestMsg msg -> StatsRequest.size_of msg
    | PacketInMsg msg -> PacketIn.size_of msg
    | FlowRemovedMsg msg -> FlowRemoved.size_of msg
    | SetConfig msg -> SwitchConfig.size_of msg
    | ConfigRequestMsg -> 0
    | ConfigReplyMsg msg -> SwitchConfig.size_of msg
    | PortStatusMsg msg -> PortStatus.size_of msg
    | ErrorMsg msg -> Error.size_of msg
    | StatsReplyMsg msg -> StatsReply.size_of msg
    (**| code -> raise (Invalid_argument (Printf.sprintf "cannot marshal (controller should not send message this message (%s) to a switch)" (to_string msg)))*)

  let blit_message (msg : t) (out : Cstruct.t) = match msg with
    | Hello buf
    | EchoRequest buf
    | EchoReply buf ->
      Cstruct.blit buf 0 out 0 (Cstruct.len buf)
    | VendorMsg msg ->
      let _ = Vendor.marshal msg out in
      ()
    | SwitchFeaturesRequest ->
      ()
    | ConfigRequestMsg ->
      ()
    | FlowModMsg flow_mod ->
      let _ = FlowMod.marshal flow_mod out in
      ()
    | PacketOutMsg msg ->
      let _ = PacketOut.marshal msg out in
      ()
    | BarrierRequest -> () (* no body, this is okay *)
    | BarrierReply -> () (* no body, this is okay *)
    | StatsRequestMsg msg ->
      let _ = StatsRequest.marshal msg out in
      ()
    | SwitchFeaturesReply rep ->
      let _ = SwitchFeatures.marshal rep out in
      ()
    | PacketInMsg msg ->
      let _ = PacketIn.marshal msg out in
      ()
    | FlowRemovedMsg msg ->
      let _ = FlowRemoved.marshal msg out in
      ()
    | SetConfig msg ->
      let _ = SwitchConfig.marshal msg out in
      ()
    | ConfigReplyMsg msg ->
      let _ = SwitchConfig.marshal msg out in
      ()
    | PortStatusMsg msg ->
      let _ = PortStatus.marshal msg out in
      ()
    | ErrorMsg msg ->
      let _ = Error.marshal msg out in
      ()
    | StatsReplyMsg msg ->
      let _ = StatsReply.marshal msg out in
      ()

  let size_of msg = Header.size + sizeof_body msg

  let header_of xid msg =
    let sizeof_buf = Header.size + sizeof_body msg in
    let open Header in
    { version = 0x01; type_code = msg_code_to_int (msg_code_of_message msg);
      length = sizeof_buf; xid = xid }

  let marshal_body (msg : t) (buf : Cstruct.t) : unit =
    blit_message msg buf

  let marshal (xid : xid) (msg : t) : string =
    let sizeof_buf = Header.size + sizeof_body msg in
    let hdr = header_of xid msg in
    let buf = Cstruct.create sizeof_buf in
    Header.marshal buf hdr;
    marshal_body msg (Cstruct.shift buf Header.size);
    Cstruct.to_string buf
end
