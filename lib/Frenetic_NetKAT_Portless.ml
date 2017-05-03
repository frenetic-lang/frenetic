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

type topo = (link * link) list [@@deriving sexp]

let is_loc_host id = Int64.equal (Int64.bit_and id 1L) 1L
let is_loc_switch id = Int64.equal (Int64.bit_and id 1L) 0L

let host_to_abs_loc id = Int64.(+) (Int64.shift_left id 1) 1L
let switch_to_abs_loc id = Int64.shift_left id 1

let link_to_abs_loc link = match link with
  | Host id -> host_to_abs_loc id
  | Switch (id, pt) -> switch_to_abs_loc id

let default_mask = 64
let id           = Filter True
let drop         = Filter False
