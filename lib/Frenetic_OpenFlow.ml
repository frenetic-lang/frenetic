open Core.Std
open Sexplib
open Sexplib.Std

module OF10 = Frenetic_OpenFlow0x01

open Frenetic_Packet

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
  Format.pp_print_string fmt (Frenetic_Packet.string_of_mac v)

let format_vlan (fmt : Format.formatter) (v:int16) =
  match v with
  | 0xffff -> Format.pp_print_string fmt "<none>"
  | _ -> format_int fmt v

let format_ip (fmt : Format.formatter) (v:int32) =
  Format.pp_print_string fmt (Frenetic_Packet.string_of_ip v)

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
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

module Pattern = struct

  module Ip = struct
    type t = nwAddr * int32 with sexp

    let match_all = (0l, 0l)

    let unsafe_shift (p, m) =
      match Int32.(to_int_exn (32l - m)) with
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
          loop Int32.(m - 1l)
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
    with sexp

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
with sexp

type pseudoport =
  | Physical of portId
  | InPort
  | Table
  | Normal
  | Flood
  | All
  | Controller of int
  | Local
with sexp

type groupId = int32 with sexp

type action =
  | Output of pseudoport
  | Enqueue of portId * queueId
  | Modify of modify
  | FastFail of groupId
with sexp

type seq = action list with sexp

type par = seq list with sexp

type group = par list with sexp

type timeout =
  | Permanent
  | ExpiresAfter of int16
with sexp

type flow = {
  pattern: Pattern.t;
  action: group;
  cookie: int64;
  idle_timeout: timeout;
  hard_timeout: timeout
} with sexp

type flowTable = flow list with sexp

type payload =
  | Buffered of bufferId * Cstruct.t
  | NotBuffered of Cstruct.t
with sexp

let payload_bytes (payload : payload) : Cstruct.t =
  match payload with
  | Buffered(_, b)
  | NotBuffered(b) -> b

type packetInReason =
  | NoMatch
  | ExplicitSend
with sexp

type pktIn = payload * int * portId * packetInReason with sexp

type pktOut = payload * (portId option) * (action list) with sexp

type switchFeatures = {
  switch_id : switchId;
  switch_ports : portId list
} with sexp

type flowStats = {
  flow_table_id : int8; (** ID of table flow came from. *)
  flow_pattern : Pattern.t;
  flow_duration_sec: int32;
  flow_duration_nsec: int32;
  flow_priority: int16;
  flow_idle_timeout: int16;
  flow_hard_timeout: int16;
  flow_actions: action list;
  flow_packet_count: int64;
  flow_byte_count: int64
} with sexp

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

let format_list (ls : 'a list) ~(to_string : 'a -> string) =
  let open Core.Std in
  let str_ls = List.map ~f:to_string ls |> List.intersperse ~sep:"," in
  String.concat (["["] @ str_ls @ ["]"])

let format_action (fmt:Format.formatter) (a:action) : unit =
  match a with
  | Output(p) ->
    Format.fprintf fmt "Output(%a)" format_pseudoport p
  | Enqueue(m,n) ->
    Format.fprintf fmt "Enqueue(%ld,%ld)" m n
  | Modify(m) ->
    format_modify fmt m
    (* TODO(grouptable) *)
  | FastFail gid -> 
    Format.fprintf fmt "FastFail(%ld)" gid

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
    List.fold_left l ~init:false
      ~f:(fun b f ->
        if b then Format.fprintf fmt "@ ";
        format_flow fmt f;
        true) in
  Format.fprintf fmt "]@]"

let string_of_action = make_string_of format_action
let string_of_seq = make_string_of format_seq
let string_of_par = make_string_of format_par
let string_of_flow = make_string_of format_flow

let string_of_vlan (x : int) : string =
  Format.sprintf "Vlan = %d" x

let string_of_vlanpcp (x : dlVlanPcp) : string =
  Format.sprintf "VlanPcp = %d" x

let string_of_ethType (x : dlTyp) : string =
  let extra = if x = 0x800 then " (ip)"
	      else if x = 0x806 then " (arp)"
	      else ""
  in
  Format.sprintf "EthType = 0x%x%s" x extra

let string_of_ipProto (x : nwProto) : string =
  let extra = match x with
    | 0x01 -> " (icmp)"
    | 0x02 -> " (igmp)"
    | 0x06 -> " (tcp)"
    | 0x11 -> " (udp)"
    | _ -> ""
  in
  Format.sprintf "ipProto = 0x%x%s" x extra

let string_of_ethSrc (x : dlAddr) : string =
  Format.sprintf "EthSrc = %s" (Frenetic_Packet.string_of_mac x)
		 
let string_of_ethDst (x : dlAddr) : string =
  Format.sprintf "EthDst = %s" (Frenetic_Packet.string_of_mac x)
		 
let string_of_ip4src (x : Pattern.Ip.t) : string =
  Format.sprintf "IP4Src = %s" (Pattern.Ip.string_of x)

let string_of_ip4dst (x : Pattern.Ip.t) : string =
  Format.sprintf "IP4Dst = %s" (Pattern.Ip.string_of x)

let string_of_tcpSrcPort (x : tpPort) : string =
  Format.sprintf "TCPSrcPort = %d" x

let string_of_tcpDstPort (x : tpPort) : string =
  Format.sprintf "TCPDstPort = %d" x

let string_of_inPort (x : portId) : string =
  Format.sprintf "InPort = %lu" x

let check (string_of : 'a -> string)
	  (x : 'a option)
	  (acc : string list) : string list =
  match x with
  | None -> acc
  | Some x' -> (string_of x') :: acc

(* Builds up a list of strings one for each pattern *)
let pattern_list (p : Pattern.t) : string list =
  check string_of_ethSrc p.dlSrc [] |>
    check string_of_ethDst p.dlDst |>
    check string_of_ethType p.dlTyp |>
    check string_of_vlan p.dlVlan |>
    check string_of_vlanpcp p.dlVlanPcp |>
    check string_of_ip4src p.nwSrc |>
    check string_of_ip4dst p.nwDst |>
    check string_of_ipProto p.nwProto |>
    check string_of_tcpSrcPort p.tpSrc |>
    check string_of_tcpDstPort p.tpDst |>
    check string_of_inPort p.inPort

(* Given a flow, return a pair of list of strings where the first list
 * contains the strings of the pattern and the second list contains
 * the strings of the actions associated with the pattern. *)
let to_entry (f : flow) : (string list) * (string list) =
  let open Core.Std in
  let open List in
  let pattern_list = pattern_list f.pattern in
  let action_list = map (concat (concat f.action)) string_of_action in
  (pattern_list, action_list)

(* Pads a string with spaces so that it is atleast `len` characters. *)
let pad (len : int) (e : string) : string =
  let open Core.Std in
  let padding_size = max 0 (len - (String.length e)) in
  let padding = String.make padding_size ' ' in
  String.concat [e; padding]

(* Helper function *)
let unwrap x =
  match x with
  | None -> 0
  | Some x -> x

(* Given a list of entries to be displayed in the table, calculate a pair
 * containing the max characters in a pattern string and action string *)
let table_size (label : string) (entries : ((string list) * (string list)) list) : int * int =
  let open Core.Std in
  let open List in
  let patterns = map entries fst |> concat in
  let actions = map entries snd |> concat in
  let max_p =  max_elt (map patterns String.length) (-) |> unwrap in
  let max_a = max_elt (map actions String.length) (-) |> unwrap in
  (max max_p ((String.length label) + 3 + (String.length "Pattern")), max max_a (String.length "Action"))

(* Create the top edge of the table *)
let top max_p max_a : string =
  let open Core.Std in
  let open Char in
  let fill = String.make (max_p + max_a + 5) '-' in
  Format.sprintf "+%s+\n" fill

(* Create the bottom edge of the table *)
let bottom max_p max_a : string=
  let open Core.Std in
  let fill = String.make (max_p + max_a + 5) '-' in
  Format.sprintf "+%s+\n" fill

(* Create a divider between entries *)
let div max_p max_a : string =
  let open Core.Std in
  let fill = String.make (max_p + max_a + 5) '-' in
  Format.sprintf "|%s|\n" fill

(* Create the columns of the table *)
let title label max_p max_a : string =
  let open Core.Std in
  let pattern = pad max_p (Format.sprintf "%s | Pattern" label) in
  let action = pad max_a "Action" in
  Format.sprintf "| %s | %s |\n" pattern action

(* Create a row in the table *)
let string_of_entry (max_p : int) (max_a : int) (e : (string list) * (string list)) : string =
  let open Core.Std in
  let open List in
  let padded_patterns = map (fst e) (pad max_p) in
  let padded_actions = map (snd e) (pad max_a) in
  let blank_action = String.make max_a ' ' in
  let blank_pattern = String.make max_p ' ' in
  let rec helper pats acts acc =
    match pats, acts with
    | [], [] -> if (length acc) = 1
		then (Format.sprintf "| %s | %s |\n" blank_pattern blank_action) :: acc
		else acc
    | (p::ps), [] ->
       let acc' = (Format.sprintf "| %s | %s |\n" p blank_action) :: acc in
       helper ps [] acc'
    | [], (a::rest) ->
       let acc' = (Format.sprintf "| %s | %s |\n" blank_pattern a) :: acc in
       helper [] rest acc'
    | (p::ps), (a::rest) ->
       let acc' = (Format.sprintf "| %s | %s |\n" p a) :: acc in
       helper ps rest acc'
  in
  helper padded_patterns padded_actions [(div max_p max_a)]
  |> rev |> String.concat

(* Given a label and a flowTable, returns an ascii flowtable *)
let string_of_flowTable ?(label="") (tbl : flowTable) : string =
  let open Core.Std in
  let entries = List.map tbl to_entry in
  let (max_p, max_a) = table_size label entries in
  let t = (top max_p max_a) in
  let l = (title label max_p max_a) in
  let entry_strings = List.map entries (string_of_entry max_p max_a) in
  let b = bottom max_p max_a in
  String.concat (t :: l :: (List.append entry_strings [b]))

module To0x01 = struct

exception Invalid_port of int32

let from_portId (pport_id : portId) : OF10.portId =
  if pport_id > 0xff00l then (* pport_id <= OFPP_MAX *)
    raise (Invalid_port pport_id)
  else
    Int32.to_int_exn pport_id

let from_output (inPort : OF10.portId option) (pseudoport : pseudoport) : OF10.action =
  match pseudoport with
    | InPort -> Output InPort
    | Table -> Output Table
    | Normal -> Output Normal
    | Flood -> Output Flood
    | All -> Output AllPorts
    | Physical pport_id ->  
      let pport_id = from_portId pport_id in
      if Some pport_id = inPort then
        Output InPort
      else
        Output (PhysicalPort pport_id)
    | Controller n -> 
      Output (Controller n)
    | Local ->
      Output Local
        
let from_action (inPort : OF10.portId option) (act : action) : OF10.action = 
  match act with
    | Output pseudoport ->
      from_output inPort pseudoport
    | Enqueue (pport_id, queue_id) ->
      let pport_id = from_portId pport_id in
      if Some pport_id = inPort then
        Enqueue(InPort, queue_id)
      else 
        Enqueue (PhysicalPort pport_id, queue_id)
    | Modify (SetEthSrc dlAddr) ->
      SetDlSrc dlAddr
    | Modify (SetEthDst dlAddr) ->
      SetDlDst dlAddr
    | Modify (SetVlan vlan) ->
      begin match vlan with
        | None
        | Some(0xffff) ->
          SetDlVlan None
        | Some(n) ->
          SetDlVlan (Some n)
      end
    | Modify (SetVlanPcp pcp) ->
      SetDlVlanPcp pcp
    | Modify (SetEthTyp _) ->
      raise (Invalid_argument "cannot set Ethernet type")
    | Modify (SetIPProto _) ->
      raise (Invalid_argument "cannot set IP protocol")
    | Modify (SetIP4Src nwAddr) ->
      SetNwSrc nwAddr
    | Modify (SetIP4Dst nwAddr) ->
      SetNwDst nwAddr
    | Modify (SetTCPSrcPort tp) ->
      SetTpSrc tp
    | Modify (SetTCPDstPort tp) ->
      SetTpDst tp
      (* TODO(grouptable) *)
    | FastFail _ -> failwith "Openflow 1.0 does not support fast failover."
        
let from_seq (inPort : OF10.portId option) (seq : seq) : OF10.action list = 
  List.map seq ~f:(from_action inPort)

let from_par (inPort : OF10.portId option) (par : par) : OF10.action list =
  List.concat (List.map par ~f:(from_seq inPort))

let from_group (inPort : OF10.portId option) (group : group)
  : OF10.action list =
  match group with
  | [] -> []
  | [par] -> from_par inPort par
  | _ ->
     raise (Unsupported "OpenFlow 1.0 does not support fast-failover")

let from_timeout (timeout : timeout) : OF10.timeout =
  match timeout with
    | Permanent -> Permanent
    | ExpiresAfter n -> ExpiresAfter n
      
      
let from_pattern (pat : Pattern.t) : OF10.pattern =
  { dlSrc = pat.dlSrc
  ; dlDst = pat.dlDst
  ; dlTyp = pat.dlTyp
  ; dlVlan = (match pat.dlVlan with
      | Some(0xffff) -> Some None
      | Some(x) -> Some (Some x)
      | None -> None)
  ; dlVlanPcp = pat.dlVlanPcp
  ; nwSrc = (match pat.nwSrc with
    | None -> None
    | Some (p,m) ->
       let mo =
         if m = 32l then
           None
         else
           Some (Int32.(32l - m)) in
       Some { m_value = p; m_mask = mo })
  ; nwDst = (match pat.nwDst with
    | None -> None
    | Some (p,m) ->
       let mo =
         if m = 32l then
           None
         else
           Some (Int32.(32l - m)) in
       Some { m_value = p; m_mask = mo })
  ; nwProto = pat.nwProto
  ; nwTos = None
  ; tpSrc = pat.tpSrc
  ; tpDst = pat.tpDst
  ; inPort = Core_kernel.Option.map pat.inPort from_portId
  }

let from_flow (priority : int) (flow : flow) : OF10.flowMod =
  match flow with
    | { pattern; action; cookie; idle_timeout; hard_timeout } ->
      let pat = from_pattern pattern in
      { command = AddFlow;
        pattern = pat;
        priority = priority;
        actions = from_group pat.inPort action;
        cookie = cookie;
        idle_timeout = from_timeout idle_timeout;
        hard_timeout = from_timeout hard_timeout;
        notify_when_removed = false;
        apply_to_packet = None;
        out_port = None;
        check_overlap = false }

let from_payload (pay : payload) : OF10.payload =
  match pay with
    | Buffered (buf_id, b) ->
      Buffered (buf_id, b)
    | NotBuffered b -> NotBuffered b
      
let from_packetOut (pktOut : pktOut) : OF10.packetOut =
  let output_payload, port_id, apply_actions = pktOut in
  let output_payload = from_payload output_payload in
  let port_id = Core_kernel.Option.map port_id from_portId in
  let apply_actions = from_par port_id [apply_actions] in
  { output_payload; port_id; apply_actions }
end
