(** NetKAT Syntax

  The NetKAT language is central to Frenetic, and we factor out the central types here.  The big actors
  on NetKAT structures are Compiler which compiles NetKAT into flow tables,
  Parser which turns NetKAT strings (e.g "TcpSrcPort(8080); port := 2") into NetKAT, and
  Json which turns JSON-formatted NetKAT into NetKAT

*)

open Sexplib.Conv
open Core
open Frenetic_kernel.Packet

(** {2 Basics} *)
(* thrown whenever local policy is expected, but global policy
  (i.e. policy containing links) is encountered *)
exception Non_local

type switchId = Frenetic_kernel.OpenFlow.switchId [@@deriving sexp, compare, eq]
type portId = Frenetic_kernel.OpenFlow.portId [@@deriving sexp, compare, eq]
type payload = Frenetic_kernel.OpenFlow.payload [@@deriving sexp]
type vswitchId = int64 [@@deriving sexp, compare, eq]
type vportId = int64 [@@deriving sexp, compare, eq]
type vfabricId = int64 [@@deriving sexp, compare, eq]
type metaId = string [@@deriving sexp, compare, eq]
type abstract_location = string [@@deriving sexp, compare, eq]

(** {2 Policies} *)

val string_of_fastfail : int32 list -> string

type location =
  | Physical of int32
  | FastFail of int32 list
  | Pipe of string
  | Query of string
  [@@deriving sexp, compare]

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
  | Meta of metaId * int64
  | From of abstract_location
  | AbstractLoc of abstract_location
  [@@deriving sexp]

type pred =
  | True
  | False
  | Test of header_val
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
  [@@deriving sexp]

type meta_init =
  | Alias of header_val
  | Const of int64
  [@@deriving sexp]

type policy =
  | Filter of pred
  | Mod of header_val
  | Union of policy * policy
  | Seq of policy * policy
  | Star of policy
  | Link of switchId * portId * switchId * portId
  | VLink of vswitchId * vportId * vswitchId * vportId
  | Let of { id : metaId; init : meta_init; mut : bool; body : policy }
  | Dup
  [@@deriving sexp]

val id : policy
val drop : policy

(** {3 Applications} *)

type action = Frenetic_kernel.OpenFlow.action

type switch_port = switchId * portId [@@deriving sexp]
type host = Frenetic_kernel.Packet.dlAddr * Frenetic_kernel.Packet.nwAddr [@@deriving sexp]

type bufferId = Int32.t [@@deriving sexp] (* XXX(seliopou): different than OpenFlow *)

