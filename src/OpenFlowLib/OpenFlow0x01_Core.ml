open Frenetic_Bit
open Packet
open Format

exception Unparsable of string
exception Ignored of string

type switchId = int64
type portId = int16
type xid = int32

let string_of_switchId = Int64.to_string
let string_of_portId = string_of_int

let vlan_none = 0xffff

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

  let size_of _ = sizeof_ofp_match

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
      | Some None -> vlan_none
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
            if vlan = vlan_none then
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

  let size_of _ = 2

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

  let size_of (a : t) =
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

  let size_of_sequence acts = List.fold_left (+) 0 (List.map size_of acts)

  let marshal a bits =
    set_ofp_action_header_typ bits (ofp_action_type_to_int (type_code a));
    set_ofp_action_header_len bits (size_of a);
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
        | SetNwDst addr -> set_ofp_action_nw_addr_nw_addr bits' addr
        | SetTpSrc pt
        | SetTpDst pt -> set_ofp_action_tp_port_tp_port bits' pt
	      | SetDlVlan (Some vid) -> set_ofp_action_vlan_vid_vlan_vid bits' vid
	      | SetDlVlan None -> ()
        | _ -> failwith "NYI: Action.marshal"
    end;
    size_of a

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
      if vid = vlan_none then
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

module FlowMod = struct

  module Command = struct

    type t =
      | AddFlow
      | ModFlow
      | ModStrictFlow
      | DeleteFlow
      | DeleteStrictFlow

    cenum ofp_flow_mod_command {
      OFPFC_ADD;
      OFPFC_MODIFY;
      OFPFC_MODIFY_STRICT;
      OFPFC_DELETE;
      OFPFC_DELETE_STRICT
    } as uint16_t

    let size_of _ = 2

    let to_string cmd = match cmd with
      | AddFlow -> "AddFlow"
      | ModFlow -> "ModFlow"
      | ModStrictFlow -> "ModStrictFlow"
      | DeleteFlow -> "DeleteFlow"
      | DeleteStrictFlow -> "DeleteStrictFlow"

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

  module Timeout = struct

    type t =
      | Permanent
      | ExpiresAfter of int16

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

  type t =
    { mod_cmd : Command.t
    ; match_ : Match.t
    ; priority : int16
    ; actions : Action.sequence
    ; cookie : int64
    ; idle_timeout : Timeout.t
    ; hard_timeout : Timeout.t
    ; notify_when_removed : bool
    ; apply_to_packet : int32 option
    ; out_port : PseudoPort.t option
    ; check_overlap : bool }

  let add_flow prio pat actions = 
    { mod_cmd = Command.AddFlow;
      match_ = pat;
      priority = prio;
      actions = actions;
      cookie = 0L;
      idle_timeout = Timeout.Permanent;
      hard_timeout = Timeout.Permanent;
      notify_when_removed = false;
      out_port =  None;
      apply_to_packet = None;
      check_overlap = false
    }


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

  let to_string m = Printf.sprintf
    "{ mod_cmd = %s; match = %s; priority = %d; actions = %s; cookie = %Ld;\
       idle_timeout = %s; hard_timeout = %s; notify_when_removed = %B;\
       apply_to_packet = %s; out_port = %s; check_overlap = %B }"
    (Command.to_string m.mod_cmd)
    (Match.to_string m.match_)
    m.priority
    (Action.sequence_to_string m.actions)
    m.cookie
    (Timeout.to_string m.idle_timeout)
    (Timeout.to_string m.hard_timeout)
    m.notify_when_removed
    (Frenetic_Misc.string_of_option Int32.to_string m.apply_to_packet)
    (Frenetic_Misc.string_of_option PseudoPort.to_string m.out_port)
    m.check_overlap

  let size_of msg =
    (Match.size_of msg.match_)
    + sizeof_ofp_flow_mod
    + (Action.size_of_sequence msg.actions)

  let flags_to_int (check_overlap : bool) (notify_when_removed : bool) =
    (if check_overlap then 1 lsl 1 else 0) lor
      (if notify_when_removed then 1 lsl 0 else 0)

  let marshal m bits =
    let bits = Cstruct.shift bits (Match.marshal m.match_ bits) in
    set_ofp_flow_mod_cookie bits (m.cookie);
    set_ofp_flow_mod_command bits (Command.to_int m.mod_cmd);
    set_ofp_flow_mod_idle_timeout bits (Timeout.to_int m.idle_timeout);
    set_ofp_flow_mod_hard_timeout bits (Timeout.to_int m.hard_timeout);
    set_ofp_flow_mod_priority bits (m.priority);
    set_ofp_flow_mod_buffer_id bits
      (match m.apply_to_packet with
        | None -> -1l
        | Some bufId -> bufId);
    set_ofp_flow_mod_out_port bits (PseudoPort.marshal_optional m.out_port);
    set_ofp_flow_mod_flags bits
      (flags_to_int m.check_overlap m.notify_when_removed);
    let bits = Cstruct.shift bits sizeof_ofp_flow_mod in
    let _ = List.fold_left
      (fun bits act ->
        Cstruct.shift bits (Action.marshal act bits))
      bits
      (Action.move_controller_last m.actions) in
    size_of m

end

module Payload = struct

  type t = 
    | Buffered of int32 * bytes
    | NotBuffered of bytes

  let parse (t : t) = match t with
    | Buffered (_, b)
    | NotBuffered b -> 
      Packet.parse b

  let to_string (t : t) = match t with
    | Buffered (b, pk) ->
      Format.sprintf "%d bytes (buffered at %ld)" (Cstruct.len pk) b
    | NotBuffered pk -> 
      Format.sprintf "%d bytes" (Cstruct.len pk)

  (* sizeof when in a [PacketOut] message *)
  let packetout_sizeof p = match p with
    | Buffered _ -> 0
    | NotBuffered bytes -> Cstruct.len bytes

  let packetout_marshal p out = 
      let _ = match p with
        | Buffered _ -> ()
        | NotBuffered bytes ->
          Cstruct.blit bytes 0 out 0 (Cstruct.len bytes)
        in
      packetout_sizeof p

end

module PacketIn = struct

  module Reason = struct

    type t =
      | NoMatch
      | ExplicitSend

    cenum ofp_reason {
      NO_MATCH = 0;
      ACTION = 1
    } as uint8_t

    let to_string r = match r with
      | NoMatch -> "NoMatch"
      | ExplicitSend -> "ExplicitSend"

    let of_int d = match int_to_ofp_reason d with
      | Some NO_MATCH -> NoMatch
      | Some ACTION -> ExplicitSend
      | None -> raise (Unparsable (sprintf "bad reason in packet_in (%d)" d))

    let to_int r = match r with
      | NoMatch -> ofp_reason_to_int NO_MATCH
      | ExplicitSend -> ofp_reason_to_int ACTION

    let size_of _ = 1

  end

  type t =
    { payload : Payload.t
    ; total_len : int16
    ; port : portId
    ; reason : Reason.t
    }

  cstruct ofp_packet_in {
    uint32_t buffer_id;
    uint16_t total_len;
    uint16_t in_port;
    uint8_t reason;
    uint8_t pad
  } as big_endian

  let to_string pin = Printf.sprintf
    "{ payload = %s; total_len = %d; port = %s; reason = %s; \
       packet = <bytes> }"
    (Payload.to_string pin.payload)
    pin.total_len
    (string_of_portId pin.port)
    (Reason.to_string pin.reason)

  let parse bits =
    let buf_id = match get_ofp_packet_in_buffer_id bits with
      | -1l -> None
      | n -> Some n in
    let total_len = get_ofp_packet_in_total_len bits in
    let in_port = get_ofp_packet_in_in_port bits in
    let reason = Reason.of_int (get_ofp_packet_in_reason bits) in
    let pk = Cstruct.shift bits sizeof_ofp_packet_in in
    let payload = match buf_id with
      | None -> Payload.NotBuffered pk
      | Some n -> Payload.Buffered (n, pk) in
    { payload = payload; total_len = total_len; port = in_port;
      reason = reason }
end

module PacketOut = struct


  type t =
    { payload : Payload.t
    ; port_id : portId option
    ; actions : Action.sequence }

  cstruct ofp_packet_out {
    uint32_t buffer_id;
    uint16_t in_port;
    uint16_t actions_len
  } as big_endian

  let to_string out = Printf.sprintf
    "{ payload = %s; port_id = %s; actions = %s }"
    (Payload.to_string out.payload)
    (Frenetic_Misc.string_of_option string_of_portId out.port_id)
    (Action.sequence_to_string out.actions)

  let size_of (pkt_out : t) : int =
    sizeof_ofp_packet_out +
      (Action.size_of_sequence pkt_out.actions) +
      (Payload.packetout_sizeof pkt_out.payload)

  let marshal (pkt_out : t) (buf : Cstruct.t) : int =
    set_ofp_packet_out_buffer_id buf
      (match pkt_out.payload with
        | Payload.Buffered (n, _) -> n
        | Payload.NotBuffered _  -> -1l);
    set_ofp_packet_out_in_port buf
      (PseudoPort.marshal_optional
        (match pkt_out.port_id with
          | Some id -> Some (PseudoPort.PhysicalPort id)
          | None -> None));
    set_ofp_packet_out_actions_len buf
      (Action.size_of_sequence pkt_out.actions);
    let buf = List.fold_left
      (fun buf act -> Cstruct.shift buf (Action.marshal act buf))
      (Cstruct.shift buf sizeof_ofp_packet_out)
      (Action.move_controller_last pkt_out.actions) in
    let _ = Payload.packetout_marshal pkt_out.payload buf in
    size_of pkt_out

end
