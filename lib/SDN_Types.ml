module OF10 = OpenFlow0x01_Core
module OF13 = OpenFlow0x04_Core

exception Unsupported of string

type int8 = int
type int12 = int
type int16 = int
type int32 = Int32.t
type int64 = Int64.t
type int48 = Int64.t
type bytes = string

type switchId = VInt.t

type bufferId =
  | OF10BufferId of int32
  | OF13BufferId of OF13.bufferId

type port =
  | PhysicalPort of VInt.t
  | AllPorts
  | Controller of int

type field =
  | InPort
  | EthType
  | EthSrc
  | EthDst
  | Vlan
  | VlanPcp
  | IPProto
  | IP4Src
  | IP4Dst
  | TCPSrcPort
  | TCPDstPort

type fieldVal = VInt.t

module FieldMap = Map.Make(struct
  type t = field
  let compare = Pervasives.compare
end)

type pattern = fieldVal FieldMap.t


type action =
  | OutputAllPorts
  | OutputPort of VInt.t
  | SetField of field * fieldVal

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

type pktIn = payload * int * VInt.t * packetInReason

type switchFeatures = {
  switch_id : switchId;
  switch_ports : VInt.t list
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

let format_field (fmt : Format.formatter) (f : field) : unit =
  Format.pp_print_string fmt
    (match f with
      | InPort -> "InPort"
      | EthType -> "EthType"
      | EthSrc -> "EthSrc"
      | EthDst -> "EthDst"
      | Vlan -> "Vlan"
      | VlanPcp -> "VlanPcp"
      | IPProto -> "IPProto"
      | IP4Src -> "IP4Src"
      | IP4Dst -> "IP4Dst"
      | TCPSrcPort -> "TCPSrcPort"
      | TCPDstPort -> "TCPDstPort")


let format_pattern (fmt:Format.formatter) (p:pattern) : unit = 
  Format.fprintf fmt "@[{";
  let _ = 
    FieldMap.fold 
      (fun f v b -> format_field fmt f; 
        Format.fprintf fmt "="; 
        VInt.format fmt v; 
        if b then Format.fprintf fmt ",@,";
        true)
      p false in 
  Format.fprintf fmt "@]"

let rec format_action (fmt:Format.formatter) (a:action) : unit = 
  match a with         
  | OutputAllPorts -> Format.fprintf fmt "OutputAllPorts"
  | OutputPort(n) -> Format.fprintf fmt "OutputPort(%a)" VInt.format n
  | SetField(f,v) -> Format.fprintf fmt "SetField(%a,%a)" format_field f VInt.format v

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
    Format.fprintf fmt "@[%a +@ %a@]" format_seq seq format_par (seq' :: par')

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

module type SWITCH = sig
  type t
  val setup_flow_table : t -> flowTable -> unit Lwt.t
  val flow_stats_request : t -> pattern -> flowStats list Lwt.t
  val packet_in : t -> pktIn Lwt_stream.t
  val packet_out : t -> payload -> par -> unit Lwt.t
  val disconnect : t -> unit Lwt.t
  val features : t -> switchFeatures  
end
