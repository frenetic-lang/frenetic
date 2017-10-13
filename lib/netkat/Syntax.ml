open Sexplib.Conv
open Core

open Frenetic_kernel.Packet

exception Non_local

type switchId = Frenetic_kernel.OpenFlow.switchId [@@deriving sexp, compare, eq]
type portId = Frenetic_kernel.OpenFlow.portId [@@deriving sexp, compare, eq]
type payload = Frenetic_kernel.OpenFlow.payload [@@deriving sexp]
type vswitchId = int64 [@@deriving sexp, compare, eq]
type vportId = int64 [@@deriving sexp, compare, eq]
type vfabricId = int64 [@@deriving sexp, compare, eq]
type metaId = string [@@deriving sexp, compare, eq]
type abstract_location = string [@@deriving sexp, compare, eq]

let string_of_fastfail = Frenetic_kernel.OpenFlow.format_list ~to_string:Int32.to_string

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
  | Wavelength of int8
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

let id = Filter True
let drop = Filter False

type action = Frenetic_kernel.OpenFlow.action

type switch_port = switchId * portId [@@deriving sexp]
type host = Frenetic_kernel.Packet.dlAddr * Frenetic_kernel.Packet.nwAddr [@@deriving sexp]

type bufferId = Int32.t [@@deriving sexp] (* XXX(seliopou): different than OpenFlow *)


let rec virtualize_pol pol = match pol with
  | Filter pred -> Filter (virtualize_pred pred)
  | Mod hv -> Mod (virtualize_hv hv)
  | Union (p,q) -> Union (virtualize_pol p, virtualize_pol q)
  | Seq (p,q) -> Seq (virtualize_pol p, virtualize_pol q)
  | Star p -> Star (virtualize_pol p)
  | Link (s,p,s',p') ->
    let s = virtualize_hv (Switch s) in
    let p = virtualize_hv (Location (Physical p)) in
    let s' = virtualize_hv (Switch s') in
    let p' = virtualize_hv (Location (Physical p')) in
    let match_vloc = Filter (And (Test s, Test p)) in
    let set_vloc = Seq (Mod s', Mod p') in
    Seq (match_vloc, set_vloc)
  | Dup | VLink _ | Let _ ->
    let err = Format.(fprintf str_formatter "unexpected primtive %a in virtual policy"
      Sexp.pp_hum (sexp_of_policy pol);
      flush_str_formatter ())
    in
    failwith err

and virtualize_pred pred = match pred with
  | True | False -> pred
  | Test hv -> Test (virtualize_hv hv)
  | And (p,q) -> And (virtualize_pred p, virtualize_pred q)
  | Or (p,q) -> Or (virtualize_pred p, virtualize_pred q)
  | Neg p -> Neg (virtualize_pred p)

and virtualize_hv hv = match hv with
  | Switch s -> VSwitch s
  | Location (Physical p) -> VPort (Int64.of_int32 p)
  | _ -> hv

