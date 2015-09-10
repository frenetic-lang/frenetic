open Sexplib.Conv
open Core.Std

(** NetKAT Syntax *)

(** {1 Basics} *)
open Frenetic_Packet

(* thrown whenever local policy is expected, but global policy
  (i.e. policy containing links) is encountered *)
exception Non_local

type switchId = Frenetic_OpenFlow.switchId with sexp
type portId = Frenetic_OpenFlow.portId with sexp
type payload = Frenetic_OpenFlow.payload with sexp
type vswitchId = int64 with sexp
type vportId = int64 with sexp
type vfabricId = int64 with sexp

(** {2 Policies} *)


let string_of_fastfail = Frenetic_OpenFlow.format_list ~to_string:Int32.to_string

type location =
  | Physical of int32
  | FastFail of int32 list
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
  | VSwitch of vswitchId
  | VPort of vportId
  | VFabric of vfabricId
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
  | VLink of vswitchId * vportId * vswitchId * vportId
  with sexp

let id = Filter True
let drop = Filter False


(** {3 Applications} *)

type action = Frenetic_OpenFlow.action

type switch_port = switchId * portId with sexp
type host = Frenetic_Packet.dlAddr * Frenetic_Packet.nwAddr with sexp

type bufferId = Int32.t with sexp (* XXX(seliopou): different than Frenetic_OpenFlow *)

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
