open Sexplib.Conv
open Core.Std

open Frenetic_Packet

exception Non_local

type switchId = Frenetic_OpenFlow.switchId [@@deriving sexp, compare, eq]
type portId = Frenetic_OpenFlow.portId [@@deriving sexp, compare, eq]
type payload = Frenetic_OpenFlow.payload [@@deriving sexp]
type vswitchId = int64 [@@deriving sexp, compare, eq]
type vportId = int64 [@@deriving sexp, compare, eq]
type vfabricId = int64 [@@deriving sexp, compare, eq]

let string_of_fastfail = Frenetic_OpenFlow.format_list ~to_string:Int32.to_string

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
  [@@deriving sexp]

type pred =
  | True
  | False
  | Test of header_val
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
  [@@deriving sexp]

module Coin = struct
  module T = struct
    type coin_label = int [@@deriving sexp]
    type coin_idx = int [@@deriving sexp]
    type prob = float [@@deriving sexp]
    type t = coin_label * coin_idx * prob [@@deriving sexp]
    let  compare (x:t) (y:t) = Pervasives.compare x y
    let hash = Hashtbl.hash
    let to_string x = sexp_of_t x |> Sexp.to_string
    let prob (_,_,prob) = prob
    let idx = ref 0
    let mk_fresh ?(prob=0.5) () =
      let c = (!idx, 0, prob) in
      incr idx; c

  end
  include T
  module Set = Set.Make(T)
 end

type policy =
  | Filter of pred
  | Mod of header_val
  | Union of policy * policy
  | Seq of policy * policy
  | Choice of policy * Coin.t * policy
  | Star of policy
  | Link of switchId * portId * switchId * portId
  | VLink of vswitchId * vportId * vswitchId * vportId
  [@@deriving sexp]

let id = Filter True
let drop = Filter False

type action = Frenetic_OpenFlow.action

type switch_port = switchId * portId [@@deriving sexp]
type host = Frenetic_Packet.dlAddr * Frenetic_Packet.nwAddr [@@deriving sexp]

type bufferId = Int32.t [@@deriving sexp] (* XXX(seliopou): different than Frenetic_OpenFlow *)

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
  [@@deriving sexp]
