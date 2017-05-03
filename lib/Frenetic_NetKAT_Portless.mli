open Core.Std

type header =
  | EthSrc
  | EthDst
  | Vlan
  | VlanPcp
  | EthType
  | IPProto
  | IP4Src
  | IP4Dst
  | TCPSrcPort
  | TCPDstPort
  | From
  | AbstractLoc
[@@deriving sexp, compare]

type pred =
  | True
  | False
  | Test of header * int64 * int
  | And of pred * pred
  | Or of pred * pred
  | Neg of pred
[@@deriving sexp]

type policy =
  | Filter of pred
  | Mod of header * int64 * int
  | Union of policy * policy
  | Seq of policy * policy
  | Star of policy
[@@deriving sexp]

type link =
  | Host of int64
  | Switch of int64 * int64
[@@deriving sexp]

val default_mask: int
val id: policy
val drop: policy

type topo = (link * link) list [@@deriving sexp]

val host_to_abs_loc: int64 -> int64
val switch_to_abs_loc: int64 -> int64

val is_loc_host: int64 -> bool
val is_loc_switch: int64 -> bool

val link_to_abs_loc: link -> int64
