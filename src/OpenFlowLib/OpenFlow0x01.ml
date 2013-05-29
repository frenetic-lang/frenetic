open Frenetic_Bit
open Packet
open Format

exception Unparsable of string

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

  type t = {
    dlSrc : dlAddr option;
    dlDst : dlAddr option;
    dlTyp : dlTyp option;
    dlVlan : dlVlan option;
    dlVlanPcp : dlVlanPcp option;
    nwSrc : nwAddr option;
    nwDst : nwAddr option;
    nwProto : nwProto option;
    nwTos : nwTos option;
    tpSrc : tpPort option;
    tpDst : tpPort option;
    inPort : portId option
  }

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

  let size = sizeof_ofp_match

  let all = {
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
  let is_none x = match x with
    | None -> true
    | Some _ -> false

  let wildcards_of_match (m : t) : Wildcards.t =
    { Wildcards.in_port = is_none m.inPort;
      Wildcards.dl_vlan = is_none m.dlVlan;
      Wildcards.dl_src = is_none m.dlSrc;
      Wildcards.dl_dst = is_none m.dlDst;
      Wildcards.dl_type = is_none m.dlTyp;
      Wildcards.nw_proto = is_none m.nwProto;
      Wildcards.tp_src = is_none m.tpSrc;
      Wildcards.tp_dst = is_none m.tpDst;
      (* TODO(arjun): support IP prefixes *)
      Wildcards.nw_src = if is_none m.nwSrc then 32 else 0x0;
      Wildcards.nw_dst = if is_none m.nwDst then 32 else 0x0;
      Wildcards.dl_vlan_pcp = is_none m.dlVlanPcp;
      Wildcards.nw_tos = is_none m.nwTos;
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
    set_ofp_match_in_port bits (if_some16 m.inPort);
    set_ofp_match_dl_src (bytes_of_mac (if_word48 m.dlSrc)) 0 bits;
    set_ofp_match_dl_dst (bytes_of_mac (if_word48 m.dlDst)) 0 bits;
    let vlan =
      match m.dlVlan with
      | Some (Some v) -> v
      | Some None -> Packet.vlan_none
      | None -> 0 in
    set_ofp_match_dl_vlan bits (vlan);
    set_ofp_match_dl_vlan_pcp bits (if_some8 m.dlVlanPcp);
    set_ofp_match_dl_type bits (if_some16 m.dlTyp);
    set_ofp_match_nw_tos bits (if_some8 m.nwTos);
    set_ofp_match_nw_proto bits (if_some8 m.nwProto);
    set_ofp_match_nw_src bits (if_some32 m.nwSrc);
    set_ofp_match_nw_dst bits (if_some32 m.nwDst);
    set_ofp_match_tp_src bits (if_some16 m.tpSrc);
    set_ofp_match_tp_dst bits (if_some16 m.tpDst);
    sizeof_ofp_match

  let parse bits =
    let w = Wildcards.parse (get_ofp_match_wildcards bits) in
    { dlSrc =
        if w.Wildcards.dl_src then
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_src bits)));
      dlDst =
        if w.Wildcards.dl_dst then
          None
        else
          Some (mac_of_bytes
                  (Cstruct.to_string (get_ofp_match_dl_dst bits)));
      dlVlan =
        if w.Wildcards.dl_vlan then
          None
        else
          begin
            let vlan = get_ofp_match_dl_vlan bits in
            if vlan = Packet.vlan_none then
              Some None
            else
              Some (Some vlan)
          end;
      dlVlanPcp =
        if w.Wildcards.dl_vlan_pcp then
          None
        else
          Some (get_ofp_match_dl_vlan_pcp bits);
      dlTyp =
        if w.Wildcards.dl_type then
          None
        else
          Some (get_ofp_match_dl_type bits);
      nwSrc =
        if w.Wildcards.nw_src >= 32 then (* TODO(arjun): prefixes *)
          None
        else
          Some (get_ofp_match_nw_src bits);
      nwDst =
        if w.Wildcards.nw_dst >= 32 then (* TODO(arjun): prefixes *)
          None
        else
          Some (get_ofp_match_nw_dst bits);
      nwProto =
        if w.Wildcards.nw_proto then
          None
        else
          Some (get_ofp_match_nw_proto bits);
      nwTos =
        if w.Wildcards.nw_tos then
          None
        else
          Some (get_ofp_match_nw_tos bits);
      tpSrc =
        if w.Wildcards.tp_src then
          None
        else
          Some (get_ofp_match_tp_src bits);
      tpDst =
        if w.Wildcards.tp_dst then
          None
        else
          Some (get_ofp_match_tp_dst bits);
      inPort =
        if w.Wildcards.in_port then
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

  let to_string (x : t) : string =
    let all_fields =
      [ fld_str "dlSrc" string_of_mac x.dlSrc;
        fld_str "dlDst" string_of_mac x.dlDst;
        fld_str "dlTyp" string_of_int x.dlTyp;
        (match x.dlVlan with
          | None -> None
          | Some None -> Some "dlVlan = none"
          | Some (Some vlan) -> fld_str "dlVlan" string_of_int (Some vlan));
        fld_str "dlVlanPcp" string_of_int x.dlVlanPcp;
        fld_str "nwSrc" Int32.to_string x.nwSrc;
        fld_str "nwDst" Int32.to_string x.nwDst;
        fld_str "nwProto" string_of_int x.nwProto;
        fld_str "nwTos" string_of_int x.nwTos;
        fld_str "tpSrc" string_of_int x.tpSrc;
        fld_str "tpDst" string_of_int x.tpDst;
        fld_str "inPort" string_of_int x.inPort ] in
    let set_fields =
      List.fold_right
        (fun fo acc -> match fo with None -> acc | Some f -> f :: acc)
        all_fields [] in
    match set_fields with
      | [] -> "{*}"
      | _ ->  "{" ^ (String.concat ", " set_fields) ^ "}"

end

module PseudoPort = struct

  type t =
    | PhysicalPort of portId
    | InPort
    | Flood
    | AllPorts
    | Controller of int

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

  let none = ofp_port_to_int OFPP_NONE

  let to_string (t : t) : string = match t with
    | PhysicalPort p -> string_of_int p
    | InPort -> "InPort"
    | Flood -> "Flood"
    | AllPorts -> "AllPorts"
    | Controller n -> sprintf "Controller<%d bytes>" n

  let make ofp_port_code len =
    match int_to_ofp_port ofp_port_code with
    | Some OFPP_IN_PORT -> InPort
    | Some OFPP_FLOOD -> Flood
    | Some OFPP_ALL -> AllPorts
    | Some OFPP_CONTROLLER -> Controller len
    | _ ->
      if ofp_port_code <= (ofp_port_to_int OFPP_MAX) then
        PhysicalPort ofp_port_code
      else
        raise
          (Unparsable (sprintf "unsupported port number (%d)" ofp_port_code))

end

module Action = struct

  type t =
    | Output of PseudoPort.t
    | SetDlVlan of dlVlan
    | SetDlVlanPcp of dlVlanPcp
    | StripVlan
    | SetDlSrc of dlAddr
    | SetDlDst of dlAddr
    | SetNwSrc of nwAddr
    | SetNwDst of nwAddr
    | SetNwTos of nwTos
    | SetTpSrc of tpPort
    | SetTpDst of tpPort

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

  cstruct ofp_action_vlan_pcp {
    uint8_t vlan_pcp;
    uint8_t pad[3]
  } as big_endian

  cstruct ofp_action_strip_vlan {
    uint8_t pad[2]
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

  let sizeof (a : t) =
    let h = sizeof_ofp_action_header in
    match a with
    | Output _ -> h + sizeof_ofp_action_output
    | SetDlVlan _ -> h + sizeof_ofp_action_vlan_vid
    | SetDlVlanPcp _ -> h + sizeof_ofp_action_vlan_pcp
    | StripVlan -> h + sizeof_ofp_action_strip_vlan
    | SetDlSrc _
    | SetDlDst _ -> h + sizeof_ofp_action_dl_addr
    | SetNwSrc _
    | SetNwDst _ -> h + sizeof_ofp_action_nw_addr
    | SetNwTos _ -> h + sizeof_ofp_action_nw_tos
    | SetTpSrc _
    | SetTpDst _ -> h + sizeof_ofp_action_tp_port

  let marshal a bits =
    set_ofp_action_header_typ bits (ofp_action_type_to_int (type_code a));
    set_ofp_action_header_len bits (sizeof a);
    let bits' = Cstruct.shift bits sizeof_ofp_action_header in
    begin
      match a with
        | Output pp ->
          set_ofp_action_output_port bits' (PseudoPort.marshal pp);
          set_ofp_action_output_max_len bits'
            (match pp with
              | PseudoPort.Controller w -> w
              | _ -> 0)
        | SetNwSrc addr 
        | SetNwDst addr ->
          set_ofp_action_nw_addr_nw_addr bits' addr
        | SetTpSrc pt
        | SetTpDst pt ->
          set_ofp_action_tp_port_tp_port bits' pt
	      | _ ->
	        failwith "NYI: Action.marshal"
    end;
    sizeof a

  let is_to_controller (act : t) : bool = match act with
    | Output (PseudoPort.Controller _) -> true
    | _ -> false

  let move_controller_last (lst : sequence) : sequence =
    let (to_ctrl, not_to_ctrl) = List.partition is_to_controller lst in
    not_to_ctrl @ to_ctrl

  let to_string (t : t) : string = match t with
    | Output p -> "Output " ^ PseudoPort.to_string p
    | SetDlVlan None -> "SetDlVlan None"
    | SetDlVlan (Some n) -> sprintf "SetDlVlan %d" n
    | SetDlVlanPcp n -> sprintf "SetDlVlanPcp n"
    | StripVlan -> "StripDlVlan"
    | SetDlSrc mac -> "SetDlSrc " ^ string_of_mac mac
    | SetDlDst mac -> "SetDlDst " ^ string_of_mac mac
    | SetNwSrc ip -> "SetNwSrc " ^ string_of_ip ip
    | SetNwDst ip -> "SetNwDst " ^ string_of_ip ip
    | SetNwTos d -> sprintf "SetNwTos %x" d
    | SetTpSrc n -> sprintf "SetTpSrc %d" n
    | SetTpDst n -> sprintf "SetTpDst %d" n

  let sequence_to_string (lst : sequence) : string =
    "[" ^ (String.concat "; " (List.map to_string lst)) ^ "]"

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
      if vid = Packet.vlan_none then
        StripVlan
      else
        SetDlVlan (Some vid)
    | Some OFPAT_SET_VLAN_PCP ->
      SetDlVlanPcp (get_ofp_action_vlan_pcp_vlan_pcp bits')
    | Some OFPAT_STRIP_VLAN -> StripVlan
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
    | Some OFPAT_ENQUEUE
    | None ->
      raise (Unparsable
        (sprintf "unrecognized ofpat_action_type (%d)" ofp_action_code))
    in
    (Cstruct.shift bits length, act)

  let parse bits = snd (_parse bits)

  let rec parse_sequence bits : sequence =
    if Cstruct.len bits = 0 then
      []
    else
      let bits', act = _parse bits in
      act::(parse_sequence bits')

end

type portChangeReason =
  | PortAdd
  | PortDelete
  | PortModify

type portConfig =
    { portConfigDown : bool; (* Port is administratively down. *)
      portConfigNoSTP : bool; (* Disable 802.1D spanning tree on port. *)
      portConfigNoRecv : bool; (* Drop all packets except 802.1D spanning
				  tree packets. *)
      portConfigNoRecvSTP : bool; (* Drop received 802.1D STP packets. *)
      portConfigNoFlood : bool; (* Do not include this port when flooding. *)
      portConfigNoFWD : bool; (* Drop packets forwarded to port. *)
      portConfigNoPacketIn : bool (* Do not send packet-in msgs for port. *)
    }

type portState = 
    { portStateDown : bool;  (* No physical link present. *)
      portStateSTPListen : bool;
      portStateSTPForward : bool;
      portStateSTPBlock : bool;
      portStateSTPMask : bool }

type portFeatures =
    { portFeat10MBHD : bool; (* 10 Mb half-duplex rate support. *)
      portFeat10MBFD : bool; (* 10 Mb full-duplex rate support. *)
      portFeat100MBHD : bool; (* 100 Mb half-duplex rate support. *)
      portFeat100MBFD : bool; (* 100 Mb full-duplex rate support. *)
      portFeat1GBHD : bool; (* 1 Gb half-duplex rate support. *)
      portFeat1GBFD : bool; (* 1 Gb full-duplex rate support. *)
      portFeat10GBFD : bool; (* 10 Gb full-duplex rate support. *)
      portFeatCopper : bool; (* Copper medium. *)
      portFeatFiber : bool; (* Fiber medium. *)
      portFeatAutoneg : bool; (* Auto-negotiation. *)
      portFeatPause : bool; (* Pause. *)
      portFeatPauseAsym : bool (* Asymmetric pause. *)
    }

type portDesc =
    { portDescPortNo : portId;
      portDescHwAddr : dlAddr;
      portDescName : string;
      portDescConfig : portConfig;
      portDescState : portState;
      portDescCurr : portFeatures;
      portDescAdvertised : portFeatures;
      portDescSupported : portFeatures;
      portDescPeer : portFeatures }

type portStatus =
    { portStatusReason : portChangeReason;
      portStatusDesc : portDesc }

type capabilities =
  { flow_stats : bool;
    table_stats : bool;
    port_stats : bool;
    stp : bool;
    ip_reasm : bool;
    queue_stats : bool;
    arp_match_ip : bool }

type actions = { output : bool;
                 set_vlan_id : bool;
                 set_vlan_pcp : bool;
                 strip_vlan : bool;
                 set_dl_src : bool;
                 set_dl_dst : bool;
                 set_nw_src : bool;
                 set_nw_dst : bool;
                 set_nw_tos : bool;
                 set_tp_src : bool;
                 set_tp_dst : bool;
                 enqueue : bool;
                 vendor : bool }

type features =
  { switch_id : int64;
    num_buffers : int32;
    num_tables : int8;
    supported_capabilities : capabilities;
    supported_actions : actions;
    ports : portDesc list}

type flowModCommand =
| AddFlow
| ModFlow
| ModStrictFlow
| DeleteFlow
| DeleteStrictFlow

type switchId = int64

let string_of_switchId = Int64.to_string

type priority = int16

type bufferId = int32

type timeout =
| Permanent
| ExpiresAfter of int16

type flowMod =
  { mfModCmd : flowModCommand;
    mfMatch : Match.t;
    mfPriority : priority;
    mfActions : Action.sequence;
    mfCookie : int64;
    mfIdleTimeOut : timeout;
    mfHardTimeOut : timeout;
    mfNotifyWhenRemoved : bool;
    mfApplyToPacket : bufferId option;
    mfOutPort : PseudoPort.t option;
    mfCheckOverlap : bool }

type reason =
| NoMatch
| ExplicitSend

type packetIn =
  { packetInBufferId : bufferId option;
    packetInTotalLen : int16;
    packetInPort : portId;
    packetInReason : reason;
    packetInPacket : bytes }

type xid = int32

type payload =
| Buffer of bufferId
| Packet of bytes

type packetOut =
  { pktOutBufOrBytes : payload;
    pktOutPortId : portId option;
    pktOutActions : Action.sequence }

(* Component types of stats_request messages. *)

type table_id = int8

module IndividualFlowRequest = struct

  type t = { of_match : Match.t;
             table_id : table_id;
             port : PseudoPort.t option }

  cstruct ofp_flow_stats_request {
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

  let sizeof req = sizeof_ofp_flow_stats_request

  let marshal req out =
    let _ = Match.marshal req.of_match out in
    set_ofp_flow_stats_request_table_id out req.table_id;
    begin match req.port with
    | Some port -> 
      set_ofp_flow_stats_request_out_port out (PseudoPort.marshal port);
    | None ->
      let open PseudoPort in
      let port_code = ofp_port_to_int OFPP_NONE in
      set_ofp_flow_stats_request_out_port out port_code
    end;
    sizeof req

end

module AggregateFlowRequest = struct

  type t = { of_match : Match.t;
             table_id : table_id;
             port : PseudoPort.t option }

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

  let sizeof req = sizeof_ofp_aggregate_stats_request

  let marshal req out =
    let _ = Match.marshal req.of_match out in
    set_ofp_aggregate_stats_request_table_id out req.table_id;
    begin match req.port with
    | Some port -> 
      set_ofp_aggregate_stats_request_out_port out (PseudoPort.marshal port);
    | None ->
      let open PseudoPort in
      let port_code = ofp_port_to_int OFPP_NONE in
      set_ofp_aggregate_stats_request_out_port out port_code
    end;
    sizeof req

end

(* component types of ofp_error_msg (datapath -> controller) *)

type helloFailedError =
  | HF_Incompatible
  | HF_Eperm

type badRequestError =
  | BR_BadVersion
  | BR_BadType
  | BR_BadStat
  | BR_BadVendor
  | BR_BadSubType
  | BR_Eperm
  | BR_BadLen
  | BR_BufferEmpty
  | BR_BufferUnknown

type badActionError =
  | BA_BadType
  | BA_BadLen
  | BA_BadVendor
  | BA_BadVendorType
  | BA_BadOutPort
  | BA_BadArgument
  | BA_Eperm
  | BA_TooMany
  | BA_BadQueue

type flowModFailedError =
  | FM_AllTablesFull
  | FM_Overlap
  | FM_Eperm
  | FM_BadEmergTimeout
  | FM_BadCommand
  | FM_Unsupported

type portModFailedError =
  | PM_BadPort
  | PM_BadHwAddr

type queueOpFailedError =
  | QO_BadPort
  | QO_BadQueue
  | QO_Eperm

(* Each error is composed of a couple (error_code, data) *)

type error =
  | HelloFailed of helloFailedError * Cstruct.t
  | BadRequest of badRequestError * Cstruct.t
  | BadAction of badActionError * Cstruct.t
  | FlowModFailed of flowModFailedError * Cstruct.t
  | PortModFailed of portModFailedError * Cstruct.t
  | QueueOpFailed of queueOpFailedError  * Cstruct.t

(* Component types of stats_reply messages. *)

module DescriptionStats = struct
  type t = { manufacturer : string;
             hardware : string;
             software : string;
             serial_number : string;
             datapath : string }
end

module IndividualFlowStats = struct

  type t = { table_id : table_id;
             of_match : Match.t;
             duration_sec : int;
             duration_nsec : int;
             priority : int;
             idle_timeout : int;
             hard_timeout : int;
             cookie : Int64.t;
             packet_count : Int64.t;
             byte_count : Int64.t;
             actions : Action.sequence }

  let to_string stats = Printf.sprintf
    "{ table_id = %d\
     ; of_match = %s\
     ; duration_sec = %d\
     ; duration_nsec = %d\
     ; priority = %d\
     ; idle_timeout = %d\
     ; hard_timeout = %d\
     ; cookie = %s\
     ; packet_count = %s\
     ; byte_count = %s\
     ; actions = %s }"
     stats.table_id
     (Match.to_string stats.of_match)
     stats.duration_sec
     stats.duration_nsec
     stats.priority
     stats.idle_timeout
     stats.hard_timeout
     (Int64.to_string stats.cookie)
     (Int64.to_string stats.packet_count)
     (Int64.to_string stats.byte_count)
     (Action.sequence_to_string stats.actions)

  let sequence_to_string stats_list =
    Frenetic_Misc.string_of_list to_string stats_list

end

module AggregateFlowStats = struct
  type t = { packet_count : int;
             byte_count : int;
             flow_count : int }
end

module TableStats = struct
  type t = { table_id : table_id;
             name : string;
             wildcards : int32;
             max_entries : int;
             active_count : int;
             lookup_count : int;
             matched_count : int }
end

module PortStats = struct
  type t = { port_no : PseudoPort.t;
             rx_packets : int;
             tx_packets : int;
             rx_bytes : int;
             tx_bytes : int;
             rx_dropped : int;
             tx_dropped : int;
             rx_errors : int;
             tx_errors : int;
             rx_frame_err : int;
             rx_over_err : int;
             rx_crc_err : int;
             collisions : int }
end

type statsRequest =
  | DescriptionReq
  | IndividualFlowReq of IndividualFlowRequest.t
  | AggregateFlowReq of AggregateFlowRequest.t
  | TableReq
  | PortReq of PseudoPort.t
  (* TODO(cole): queue and vendor stats requests. *)

type statsReply =
  | DescriptionRep of DescriptionStats.t
  | IndividualFlowRep of IndividualFlowStats.t list
  | AggregateFlowRep of AggregateFlowStats.t
  | TableRep of TableStats.t
  | PortRep of PortStats.t

let string_of_statsReply r = match r with
  | DescriptionRep _ -> "DescriptionRep"
  | IndividualFlowRep _ -> "IndividualFlowRep"
  | AggregateFlowRep _ -> "AggregateFlowRep"
  | TableRep _ -> "TableRep"
  | PortRep _ -> "PortRep"

(* A subset of the OpenFlow 1.0 messages defined in Section 5.1 of the spec. *)

type message =
  | Hello of bytes
  | EchoRequest of bytes
  | EchoReply of bytes
  | FeaturesRequest
  | FeaturesReply of features
  | FlowModMsg of flowMod
  | PacketInMsg of packetIn
  | PortStatusMsg of portStatus
  | PacketOutMsg of packetOut
  | BarrierRequest (* JNF: why not "BarrierRequestMsg"? *)
  | BarrierReply (* JNF: why not "BarrierReplyMsg"? *)
  | StatsRequestMsg of statsRequest
  | StatsReplyMsg of statsReply

let delete_all_flows =
  FlowModMsg {
    mfModCmd = DeleteFlow;
    mfMatch = Match.all;
    mfPriority = 0;
    mfActions = [];
    mfCookie = 0L;
    mfIdleTimeOut = Permanent;
    mfHardTimeOut = Permanent;
    mfNotifyWhenRemoved = false;
    mfApplyToPacket = None;
    mfOutPort = None;
    mfCheckOverlap = false
  }

let add_flow prio match_ actions =
  FlowModMsg {
    mfModCmd = AddFlow;
    mfMatch = match_;
    mfPriority = prio;
    mfActions = actions;
    mfCookie = 0L;
    mfIdleTimeOut = Permanent;
    mfHardTimeOut = Permanent;
    mfNotifyWhenRemoved = false;
    mfApplyToPacket = None;
    mfOutPort = None;
    mfCheckOverlap = false
  }

module type PLATFORM = sig
  exception SwitchDisconnected of switchId
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t
  val recv_from_switch : switchId -> (xid * message) Lwt.t
  val accept_switch : unit -> features Lwt.t
end

