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

type switchId =
  | OF10SwitchId of OF10.switchId
  | OF13SwitchId of OF13.switchId

type portId =
  | OF10PortId of OF10.portId
  | OF13PortId of OF13.portId

type bufferId =
  | OF10BufferId of int32
  | OF13BufferId of OF13.bufferId

type port =
  | PhysicalPort of portId
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

type fieldVal =  
  | Int64 of Int64.t
  | Int48 of Int64.t
  | Int32 of Int32.t
  | Int16 of int
  | Int8 of int
  | Int4 of int

module FieldMap = Map.Make(struct
  type t = field
  let compare = Pervasives.compare
end)

type pattern = fieldVal FieldMap.t

let get_int64 (v : fieldVal) : Int64.t = match v with
  | Int64 n -> n
  | _ -> raise (Invalid_argument "get_int64")

let get_int48 (v : fieldVal) : Int64.t = match v with
  | Int48 n -> n
  | _ -> raise (Invalid_argument "get_int48")

let get_int32 (v : fieldVal) : Int32.t = match v with
  | Int32 n -> n
  | _ -> raise (Invalid_argument "get_int32")

let get_int16 (v : fieldVal) : int = match v with
  | Int16 n -> n
  | _ -> raise (Invalid_argument "get_int16")

let get_int8 (v : fieldVal) : int = match v with
  | Int8 n -> n
  | _ -> raise (Invalid_argument "get_int8")

let get_int4 (v : fieldVal) : int = match v with
  | Int4 n -> n
  | _ -> raise (Invalid_argument "get_int4")

type action =
  | OutputAllPorts
  | OutputPort of portId
  | SetField of field * fieldVal
  | Seq of action * action
  | Par of action * action 
  | Failover of portId * action * action

type timeout =
  | Permanent
  | ExpiresAfter of int16

type flow = {
  pattern: pattern;
  action: action;
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

let format_switchId (fmt : Format.formatter) (switchId : switchId) : unit = 
  match switchId with
  | OF10SwitchId n -> Format.fprintf fmt "%Ld" n
  | OF13SwitchId n -> Format.fprintf fmt "%Ld" n

let format_portId (fmt : Format.formatter) (portId : portId) : unit = 
  match portId with
  | OF10PortId n -> Format.fprintf fmt "%d" n
  | OF13PortId n -> Format.fprintf fmt "%ld" n

module type SWITCH = sig
  type t
  val setup_flow_table : t -> flowTable -> unit Lwt.t
  val flow_stats_request : t -> pattern -> flowStats list Lwt.t
  val packet_in : t -> pktIn Lwt_stream.t
  val packet_out : t -> payload -> action -> unit Lwt.t
  val disconnect : t -> unit Lwt.t
  val features : t -> switchFeatures  
end