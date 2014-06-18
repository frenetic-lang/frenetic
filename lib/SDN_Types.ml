module OF10 = OpenFlow0x01_Core
module OF13 = OpenFlow0x04_Core

open Packet

exception Unsupported of string

type switchId = int64
type portId = int32
type queueId = int32

type bufferId = int32

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

let format_ip_mask (fmt : Format.formatter) ((p,m) : nwAddr * int32) = 
  Format.fprintf fmt "%a%s" 
    format_ip p 
    (if m = 32l then "" else Printf.sprintf "/%ld" m)

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

module Pattern = struct

  type t =
      { dlSrc : dlAddr option
      ; dlDst : dlAddr option
      ; dlTyp : dlTyp option
      ; dlVlan : dlVlan
      ; dlVlanPcp : dlVlanPcp option
      ; nwSrc : (nwAddr * int32) option
      ; nwDst : (nwAddr * int32) option
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

  let ip_shift (p,m) =
    try 
      match Int32.to_int (Int32.(sub 32l m)) with 
      | 32 -> 0l
      | i when 0 <= i && i < 32 -> 
	Int32.shift_right_logical p i
      | _ -> 
	failwith "Pattern.ip_shift: invalid mask"
    with _ -> 
      failwith "Pattern.ip_shift: invalid mask" 

  let ip_compatible (p1,m1) (p2,m2) = 
    (* Two masked IPs are compatible if their prefixes agree. *)
    try 
      let common_mask = 
	match Int32.to_int (min m1 m2) with
	| i when 0 <= i && i <= 32 -> 
	  i
	| _ -> 
	  failwith "Pattern.ip_compatible: invalid mask" in 
      let mask =
	Int32.shift_left
          (Int32.(sub) (Int32.shift_left 1l common_mask) 1l)
          (32 - common_mask) in
      mask = 0l || (Int32.logand mask p1) = (Int32.logand mask p2)
    with _ -> 
      failwith "Pattern.ip_compatible: invalid mask"        

  let ip_subseteq (p1,m1) (p2,m2) = 
    ip_compatible (p1,m1) (p2,m2) && m1 >= m2 
      
  let ip_intersect (p1,m1) (p2,m2) = 
    if ip_compatible (p1,m1) (p2,m2) then 
      if m1 >= m2 then Some (p1,m1) else Some (p2,m2)
    else None 

  let ip_meet (p1,m1) (p2,m2) = 
    if ip_compatible (p1,m1) (p2,m2) then 
      if m1 >= m2 then Some (p2,m2) else Some (p1,m1)
    else None 

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
    && check ip_subseteq p1.nwSrc p2.nwSrc
    && check ip_subseteq p1.nwDst p2.nwDst
    && check (=) p1.nwProto p2.nwProto
    && check (=) p1.tpSrc p2.tpSrc
    && check (=) p1.tpDst p2.tpDst
    && check (=) p1.inPort p2.inPort

  let eq p1 p2 =
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
    && check (fun x y -> ip_subseteq x y || ip_subseteq y x) p1.nwSrc p2.nwSrc
    && check (fun x y -> ip_subseteq x y || ip_subseteq y x)p1.nwDst p2.nwDst
    && check (=) p1.nwProto p2.nwProto
    && check (=) p1.tpSrc p2.tpSrc
    && check (=) p1.tpDst p2.tpDst
    && check (=) p1.inPort p2.inPort
    
  let eq_inter x1 x2 = 
    if x1 = x2 then Some x1 else None 

  let eq_meet x1 x2 = 
    if x1 = x2 then Some x1 else None 

  let meet p1 p2 =
    let meeter m m1 m2 =
      match m1, m2 with
      | Some v1, Some v2 -> 
	m v1 v2 
      | _ -> 
        None in 
    { dlSrc = meeter eq_meet p1.dlSrc p2.dlSrc
    ; dlDst = meeter eq_meet p1.dlDst p2.dlDst
    ; dlTyp = meeter eq_meet p1.dlTyp p2.dlTyp
    ; dlVlan = meeter eq_meet p1.dlVlan p2.dlVlan
    ; dlVlanPcp = meeter eq_meet p1.dlVlanPcp p2.dlVlanPcp
    ; nwSrc = meeter ip_meet p1.nwSrc p2.nwSrc
    ; nwDst = meeter ip_meet p1.nwDst p2.nwDst
    ; nwProto = meeter eq_meet p1.nwProto p2.nwProto
    ; tpSrc = meeter eq_meet p1.tpSrc p2.tpSrc
    ; tpDst = meeter eq_meet p1.tpDst p2.tpDst
    ; inPort = meeter eq_meet p1.inPort p2.inPort }

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
    format_field "vlanId" format_int p.dlVlan;
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
