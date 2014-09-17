open Sexplib
open Sexplib.Std

module OF10 = OpenFlow0x01_Core
module OF13 = OpenFlow0x04_Core

open Packet

exception Unsupported of string

type switchId = int64 with sexp
type portId = int32 with sexp
type queueId = int32 with sexp
type bufferId = int32 with sexp

(* general formatters for numeric types *)
let format_int (fmt : Format.formatter) (v:int) =
  Format.fprintf fmt "%u" v

let format_int32 (fmt : Format.formatter) (v:int32) =
  Format.fprintf fmt "%lu" v

let format_hex (fmt : Format.formatter) (v:int) =
  Format.fprintf fmt "0x%x" v

(* formatters for packet fields *)
let format_mac (fmt : Format.formatter) (v:int48) =
  Format.pp_print_string fmt (Packet.string_of_mac v)

let format_vlan (fmt : Format.formatter) (v:int16) =
  match v with
  | 0xffff -> Format.pp_print_string fmt "<none>"
  | _ -> format_int fmt v

let format_ip (fmt : Format.formatter) (v:int32) =
  Format.pp_print_string fmt (Packet.string_of_ip v)

let format_ip_mask (fmt : Format.formatter) ((p,m) : nwAddr * int32) =
  Format.fprintf fmt "%a%s"
    format_ip p
    (if m = 32l then "" else Printf.sprintf "/%ld" m)

(* convert a formatter to a function that produces a string *)
(* TODO(jnf): we have this defined in several places. Consolidate. *)
let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 200;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

module Pattern = struct

  module Ip = struct
    type t = nwAddr * int32

    let match_all = (0l, 0l)

    let unsafe_shift (p, m) =
      match Int32.(to_int (sub 32l m)) with
      | 32 -> 0l
      | i  -> Int32.shift_right_logical p i

    let check_mask m =
      if m < 0l || m > 32l then
        failwith "Pattern.Ip: invalid mask"

    let shift (p, m) =
      check_mask m;
      unsafe_shift (p, m)

    let less_eq (p1, m1) (p2, m2) =
      m1 >= m2 && begin
        check_mask m2;
        unsafe_shift (p1, m2) = unsafe_shift (p2, m2)
      end

    let eq (p1, m1) (p2, m2) =
      m1 = m2 && (m1 = 0l || begin
        check_mask m1;
        unsafe_shift (p1, m1) = unsafe_shift (p2, m2)
      end)

    let join (p1, m1) (p2, m2) =
      let rec loop m =
        if m = 0l then
          (0l, 0l)
        else if unsafe_shift (p1, m) = unsafe_shift (p2, m) then
          (p1, m)
        else
          loop Int32.(sub m 1l)
      in
      let m = min m1 m2 in
      check_mask m;
      loop m

    let intersect ip1 ip2 =
      if less_eq ip1 ip2 then
        Some ip1
      else if less_eq ip2 ip1 then
        Some ip2
      else
        None

    let compatible ip1 ip2 =
      match intersect ip1 ip2 with
        | Some _ -> true
        | None   -> false

    let format = format_ip_mask
    let string_of ip = make_string_of format ip
  end

  type t =
      { dlSrc : dlAddr option
      ; dlDst : dlAddr option
      ; dlTyp : dlTyp option
      ; dlVlan : dlVlan
      ; dlVlanPcp : dlVlanPcp option
      ; nwSrc : Ip.t option
      ; nwDst : Ip.t option
      ; nwProto : nwProto option
      ; tpSrc : tpPort option
      ; tpDst : tpPort option
      ; inPort : portId option }

  let match_all =
      { dlSrc = None
      ; dlDst = None
      ; dlTyp = None
      ; dlVlan = None
      ; dlVlanPcp = None
      ; nwSrc = None
      ; nwDst = None
      ; nwProto = None
      ; tpSrc = None
      ; tpDst = None
      ; inPort = None }

  (* TODO(jnf): rename subseteq ?*)
  let less_eq p1 p2 =
    let check f m1 m2 =
      match m2 with
        | None -> true
        | Some(v2) ->
          begin match m1 with
            | None -> false
            | Some(v1) -> f v1 v2
          end in
    check (=) p1.dlSrc p2.dlSrc
    && check (=) p1.dlDst p2.dlDst
    && check (=) p1.dlTyp p2.dlTyp
    && check (=) p1.dlVlan p2.dlVlan
    && check (=) p1.dlVlanPcp p2.dlVlanPcp
    && check Ip.less_eq p1.nwSrc p2.nwSrc
    && check Ip.less_eq p1.nwDst p2.nwDst
    && check (=) p1.nwProto p2.nwProto
    && check (=) p1.tpSrc p2.tpSrc
    && check (=) p1.tpDst p2.tpDst
    && check (=) p1.inPort p2.inPort

  let eq p1 p2 =
    let check f m1 m2 =
      match m1, m2 with
        | None   , None    -> true
        | Some v1, Some v2 -> f v1 v2
        | _      , _       -> false in
    check (=) p1.dlSrc p2.dlSrc
    && check (=) p1.dlDst p2.dlDst
    && check (=) p1.dlTyp p2.dlTyp
    && check (=) p1.dlVlan p2.dlVlan
    && check (=) p1.dlVlanPcp p2.dlVlanPcp
    && check Ip.eq p1.nwSrc p2.nwSrc
    && check Ip.eq p1.nwDst p2.nwDst
    && check (=) p1.nwProto p2.nwProto
    && check (=) p1.tpSrc p2.tpSrc
    && check (=) p1.tpDst p2.tpDst
    && check (=) p1.inPort p2.inPort

  let eq_join x1 x2 =
    if x1 = x2 then Some x1 else None

  let join p1 p2 =
    let joiner m m1 m2 =
      match m1, m2 with
      | Some v1, Some v2 ->
        m v1 v2
      | _ ->
        None in
    { dlSrc = joiner eq_join p1.dlSrc p2.dlSrc
    ; dlDst = joiner eq_join p1.dlDst p2.dlDst
    ; dlTyp = joiner eq_join p1.dlTyp p2.dlTyp
    ; dlVlan = joiner eq_join p1.dlVlan p2.dlVlan
    ; dlVlanPcp = joiner eq_join p1.dlVlanPcp p2.dlVlanPcp
    ; nwSrc = joiner (fun x y -> Some(Ip.join x y)) p1.nwSrc p2.nwSrc
    ; nwDst = joiner (fun x y -> Some(Ip.join x y)) p1.nwDst p2.nwDst
    ; nwProto = joiner eq_join p1.nwProto p2.nwProto
    ; tpSrc = joiner eq_join p1.tpSrc p2.tpSrc
    ; tpDst = joiner eq_join p1.tpDst p2.tpDst
    ; inPort = joiner eq_join p1.inPort p2.inPort }

  let format (fmt:Format.formatter) (p:t) : unit =
    let first = ref true in
    let format_field name format_val m_val =
      match m_val with
        | None   -> ()
        | Some v ->
          if not (!first) then Format.fprintf fmt ",@,";
          Format.fprintf fmt "%s=%a" name format_val v;
          first := false in
    Format.fprintf fmt "@[{";
    format_field "ethSrc" format_mac p.dlSrc;
    format_field "ethDst" format_mac p.dlDst;
    format_field "ethTyp" format_hex p.dlTyp;
    format_field "vlanId" format_vlan p.dlVlan;
    format_field "vlanPcp" format_int p.dlVlanPcp;
    format_field "nwProto" format_hex p.nwProto;
    format_field "ipSrc" format_ip_mask p.nwSrc;
    format_field "ipDst" format_ip_mask p.nwDst;
    format_field "tcpSrcPort" format_int p.tpSrc;
    format_field "tcpDstPort" format_int p.tpDst;
    format_field "port" format_int32 p.inPort;
    Format.fprintf fmt "}@]"

  let string_of = make_string_of format
end

type modify =
  | SetEthSrc of dlAddr
  | SetEthDst of dlAddr
  | SetVlan of dlVlan
  | SetVlanPcp of dlVlanPcp
  | SetEthTyp of dlTyp
  | SetIPProto of nwProto
  | SetIP4Src of nwAddr
  | SetIP4Dst of nwAddr
  | SetTCPSrcPort of tpPort
  | SetTCPDstPort of tpPort

type pseudoport =
  | Physical of portId
  | InPort
  | Table
  | Normal
  | Flood
  | All
  | Controller of int
  | Local

type action =
  | Output of pseudoport
  | Enqueue of portId * queueId
  | Modify of modify

type seq = action list

type par = seq list

type group = par list

type timeout =
  | Permanent
  | ExpiresAfter of int16

type flow = {
  pattern: Pattern.t;
  action: group;
  cookie: int64;
  idle_timeout: timeout;
  hard_timeout: timeout
}

type flowTable = flow list

type payload =
  | Buffered of bufferId * bytes
  | NotBuffered of bytes
with sexp

let payload_bytes (payload : payload) : bytes =
  match payload with
  | Buffered(_, bytes)
  | NotBuffered(bytes) -> bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

type pktIn = payload * int * portId * packetInReason

type pktOut = payload * (portId option) * (action list)

type switchFeatures = {
  switch_id : switchId;
  switch_ports : portId list
}

type flowStats = {
  flow_table_id : int8; (** ID of table flow came from. *)
  flow_pattern : Pattern.t;
  flow_duration_sec: int32;
  flow_duration_nsec: int32;
  flow_priority: int16;
  flow_idle_timeout: int16;
  flow_hard_timeout: int16;
  flow_action: action;
  flow_packet_count: int64;
  flow_byte_count: int64
}

let format_modify (fmt:Format.formatter) (m:modify) : unit =
  match m with
  | SetEthSrc(dlAddr) ->
    Format.fprintf fmt "SetField(ethSrc, %a)" format_mac dlAddr
  | SetEthDst(dlAddr) ->
    Format.fprintf fmt "SetField(ethDst, %a)" format_mac dlAddr
  | SetVlan(None) ->
    Format.fprintf fmt "SetField(vlan, %a)" format_vlan 0xffff
  | SetVlan(Some(vlan_id)) ->
    Format.fprintf fmt "SetField(vlan, %a)" format_vlan vlan_id
  | SetVlanPcp(pcp) ->
    Format.fprintf fmt "SetField(vlanPcp, %u)" pcp
  | SetEthTyp(dlTyp) ->
    Format.fprintf fmt "SetField(ethTyp, %u)" dlTyp
  | SetIPProto(nwProto) ->
    Format.fprintf fmt "SetField(ipProto, %a)" format_hex nwProto
  | SetIP4Src(nwAddr) ->
    Format.fprintf fmt "SetField(ipSrc, %a)" format_ip nwAddr
  | SetIP4Dst(nwAddr) ->
    Format.fprintf fmt "SetField(ipDst, %a)" format_ip nwAddr
  | SetTCPSrcPort(tpPort) ->
    Format.fprintf fmt "SetField(tcpSrcPort, %a)" format_int tpPort
  | SetTCPDstPort(tpPort) ->
    Format.fprintf fmt "SetField(tcpDstPort, %a)" format_int tpPort

let format_pseudoport (fmt:Format.formatter) (p:pseudoport) : unit =
  match p with
  | Physical(portId) -> Format.fprintf fmt "%lu" portId
  | InPort -> Format.fprintf fmt "InPort"
  | Table -> Format.fprintf fmt "Table"
  | Normal -> Format.fprintf fmt "Normal"
  | Flood -> Format.fprintf fmt "Flood"
  | All -> Format.fprintf fmt "All"
  | Controller(bytes) -> Format.fprintf fmt "Controller(%u)" bytes
  | Local -> Format.fprintf fmt "Local"

let format_action (fmt:Format.formatter) (a:action) : unit =
  match a with
  | Output(p) ->
    Format.fprintf fmt "Output(%a)" format_pseudoport p
  | Enqueue(m,n) ->
    Format.fprintf fmt "Enqueue(%ld,%ld)" m n
  | Modify(m) ->
    format_modify fmt m

let rec format_seq (fmt : Format.formatter) (seq : seq) : unit =
  match seq with
  | [] -> ()
  | [act] -> format_action fmt act
  | (act :: act' :: seq') ->
      Format.fprintf fmt "@[%a;@ %a@]" format_action act format_seq (act' :: seq')

let rec format_par (fmt : Format.formatter) (par : par) : unit =
  match par with
  | [] -> ()
  | [seq] -> format_seq fmt seq
  | (seq :: seq' :: par') ->
    Format.fprintf fmt "@[%a |@ %a@]" format_seq seq format_par (seq' :: par')

let rec format_group (fmt : Format.formatter) (group : group) : unit =
  match group with
  | [] -> ()
  | [par] -> format_par fmt par
  | (par :: par' :: groups) ->
    Format.fprintf fmt "@[%a +@ %a@]" format_par par format_group (par' :: groups)

let format_timeout (fmt:Format.formatter) (t:timeout) : unit =
  match t with
    | Permanent -> Format.fprintf fmt "Permanent"
    | ExpiresAfter(n) -> Format.fprintf fmt "ExpiresAfter(%d)" n

let format_flow (fmt: Format.formatter) (f : flow) : unit =
  Format.fprintf fmt "@[{pattern=%a,@," Pattern.format f.pattern;
  Format.fprintf fmt "action=%a,@," format_group f.action;
  Format.fprintf fmt "cookie=%s,@," (Int64.to_string f.cookie);
  Format.fprintf fmt "idle_timeout=%a,@," format_timeout f.idle_timeout;
  Format.fprintf fmt "hard_timeout=%a}@]" format_timeout f.hard_timeout

let format_flowTable (fmt:Format.formatter) (l:flowTable) : unit =
  Format.fprintf fmt "@[[";
  let _ =
    List.fold_left
      (fun b f ->
        if b then Format.fprintf fmt "@ ";
        format_flow fmt f;
        true) false l in
  Format.fprintf fmt "]@]"

let string_of_action = make_string_of format_action
let string_of_seq = make_string_of format_seq
let string_of_par = make_string_of format_par
let string_of_flow = make_string_of format_flow
let string_of_flowTable = make_string_of format_flowTable
