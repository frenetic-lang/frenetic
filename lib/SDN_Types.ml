module OF10 = OpenFlow0x01_Core
module OF13 = OpenFlow0x04_Core

open Packet

exception Unsupported of string

type switchId = int64
type portId = int32
type queueId = int32

type bufferId = int32

type pattern =
    { dlSrc : dlAddr option
    ; dlDst : dlAddr option
    ; dlTyp : dlTyp option
    ; dlVlan : dlVlan
    ; dlVlanPcp : dlVlanPcp option
    ; nwSrc : nwAddr option
    ; nwDst : nwAddr option
    ; nwProto : nwProto option
    ; tpSrc : tpPort option
    ; tpDst : tpPort option
    ; inPort : portId option }

let all_pattern =
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
  pattern: pattern;
  action: group;
  cookie: int64;
  idle_timeout: timeout;
  hard_timeout: timeout
}

type flowTable = flow list 

type payload =
  | Buffered of bufferId * bytes 
  | NotBuffered of bytes

type packetInReason =
  | NoMatch
  | ExplicitSend

type pktIn = payload * int * portId * packetInReason

type switchFeatures = {
  switch_id : switchId;
  switch_ports : portId list
}

type flowStats = {
  flow_table_id : int8; (** ID of table flow came from. *)
  flow_pattern : pattern;
  flow_duration_sec: int32;
  flow_duration_nsec: int32;
  flow_priority: int16;
  flow_idle_timeout: int16;
  flow_hard_timeout: int16;
  flow_action: action;
  flow_packet_count: int64;
  flow_byte_count: int64
}

let format_mac (fmt : Format.formatter) (v:int48) =
  Format.pp_print_string fmt (Packet.string_of_mac v)

let format_ip (fmt : Format.formatter) (v:int32) =
  Format.pp_print_string fmt (Packet.string_of_ip v)

let format_hex (fmt : Format.formatter) (v:int) =
  Format.fprintf fmt "0x%x" v

let format_int (fmt : Format.formatter) (v:int) =
  Format.fprintf fmt "%u" v

let format_int32 (fmt : Format.formatter) (v:int32) =
  Format.fprintf fmt "%lu" v

let format_pattern (fmt:Format.formatter) (p:pattern) : unit = 
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
  format_field "vlanId" format_int p.dlVlan;
  format_field "vlanPcp" format_int p.dlVlanPcp;
  format_field "nwProto" format_hex p.nwProto;
  format_field "ipSrc" format_ip p.nwSrc;
  format_field "ipDst" format_ip p.nwDst;
  format_field "tcpSrcPort" format_int p.tpSrc;
  format_field "tcpDstPort" format_int p.tpDst;
  format_field "port" format_int32 p.inPort;
  Format.fprintf fmt "}@]"

let format_modify (fmt:Format.formatter) (m:modify) : unit =
  match m with
  | SetEthSrc(dlAddr) ->
    Format.fprintf fmt "SetField(ethSrc, %a)" format_mac dlAddr
  | SetEthDst(dlAddr) ->
    Format.fprintf fmt "SetField(ethDst, %a)" format_mac dlAddr
  | SetVlan(None)
  | SetVlan(Some(0xffff)) ->
    Format.fprintf fmt "SetField(vlan, <none>)"
  | SetVlan(Some(id)) ->
    Format.fprintf fmt "SetField(vlan, %u)" id
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
  Format.fprintf fmt "@[{pattern=%a,@," format_pattern f.pattern;
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

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let string_of_action = make_string_of format_action
let string_of_seq = make_string_of format_seq
let string_of_par = make_string_of format_par
let string_of_pattern = make_string_of format_pattern
let string_of_flow = make_string_of format_flow
let string_of_flowTable = make_string_of format_flowTable
