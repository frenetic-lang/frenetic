open Misc
open List
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module Pattern : Pattern.PATTERN
  with type t = NetCoreAction.Action.Pattern.t
  and type port = NetCoreAction.Port.port

type pattern = Pattern.t

type pred =
| PrHdr of pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

type pol =
| PoAction of NetCoreAction.Action.t
| PoFilter of pred
| PoUnion of pol * pol
| PoSeq of pol * pol

type value =
| Pkt of switchId * NetCoreAction.Action.port * packet
   * (bufferId, bytes) sum

val match_pred : pred -> switchId -> Pattern.port -> packet -> bool

val serialize_pkt : packet -> bytes

val eval_action : value -> NetCoreAction.Action.t -> value list

val classify : pol -> value -> value list

val pol_to_string : pol -> string

val pred_to_string : pred -> string

