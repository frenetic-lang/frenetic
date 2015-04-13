open Sexplib.Conv

(** NetKAT Syntax *)
open Core.Std

(** {2 Basics} *)
open Packet

type switchId = SDN_Types.switchId with sexp
type portId = SDN_Types.portId with sexp
type payload = SDN_Types.payload with sexp

(** {2 Policies} *)

type location =
  | Physical of int32
  | Pipe of string
  | Query of string
  with sexp

type header_val =
  | Switch of switchId
  | Location of location
  | EthSrc of dlAddr
  | EthDst of dlAddr
  | Vlan of int16
  | VlanPcp of dlVlanPcp
  | EthType of dlTyp
  | IPProto of nwProto
  | IP4Src of nwAddr * int32
  | IP4Dst of nwAddr * int32
  | TCPSrcPort of tpPort
  | TCPDstPort of tpPort
  with sexp

type pred =
  | True
  | False
  | Test of header_val
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
  with sexp

type policy =
  | Filter of pred
  | Mod of header_val
  | Union of policy * policy
  | Seq of policy * policy
  | Star of policy
  | Link of switchId * portId * switchId * portId
  with sexp

let id = Filter True
let drop = Filter False

(** {2 Packets}

  If we only defined the semantics and were not building a system, a
  packet would only be a record of headers. However, the runtime needs to
  apply [eval] to packets contained in [PACKET_IN] mesages. For the runtime,
  packets also carry a payload that is unmodified by [eval]. *)

(** {3 Applications} *)

type action = SDN_Types.action

type switch_port = switchId * portId with sexp
type host = Packet.dlAddr * Packet.nwAddr with sexp

type bufferId = Int32.t with sexp (* XXX(seliopou): different than SDN_Types *)
type bytes = Packet.bytes with sexp

type event =
  | PacketIn of string * switchId * portId * payload * int
  | Query of string * int64 * int64
  | SwitchUp of switchId * portId list
  | SwitchDown of switchId
  | PortUp of switch_port
  | PortDown of switch_port
  | LinkUp of switch_port * switch_port
  | LinkDown of switch_port * switch_port
  | HostUp of switch_port * host
  | HostDown of switch_port * host
  with sexp
