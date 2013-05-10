open Misc
open List
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module Pattern : Pattern.PATTERN
  with type t = NetCoreAction.NetCoreAction.Pattern.t
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
| PoAction of NetCoreAction.NetCoreAction.t
| PoFilter of pred
| PoUnion of pol * pol
| PoSeq of pol * pol

type value =
| Pkt of switchId * NetCoreAction.NetCoreAction.port * packet
   * (bufferId, bytes) sum

val match_pred : pred -> switchId -> Pattern.port -> packet -> bool

val serialize_pkt : packet -> bytes

val eval_action : value -> NetCoreAction.NetCoreAction.t -> value list

val classify : pol -> value -> value list

