open Datatypes
open List
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module Pattern : sig
  include PatternSignatures.PATTERN
end
  with type port = NetCoreAction.Port.t

type pattern = Pattern.t

type pred =
| PrHdr of pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

val pred_rect :
  (pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 ->
  'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 -> 'a1) -> 'a1
  -> 'a1 -> pred -> 'a1

val pred_rec :
  (pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 ->
  'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 -> 'a1) -> 'a1
  -> 'a1 -> pred -> 'a1

type pol =
| PoAction of NetCoreAction.NetCoreAction.t
| PoFilter of pred
| PoUnion of pol * pol
| PoSeq of pol * pol

val pol_rect :
  (NetCoreAction.NetCoreAction.t -> 'a1) -> (pred -> 'a1) -> (pol -> 'a1 ->
  pol -> 'a1 -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> pol -> 'a1

val pol_rec :
  (NetCoreAction.NetCoreAction.t -> 'a1) -> (pred -> 'a1) -> (pol -> 'a1 ->
  pol -> 'a1 -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> pol -> 'a1

type value =
| Pkt of switchId * NetCoreAction.NetCoreAction.port * packet
   * (bufferId, bytes) sum

val value_rect :
  (switchId -> NetCoreAction.NetCoreAction.port -> packet -> (bufferId,
  bytes) sum -> 'a1) -> value -> 'a1

val value_rec :
  (switchId -> NetCoreAction.NetCoreAction.port -> packet -> (bufferId,
  bytes) sum -> 'a1) -> value -> 'a1

val match_pred : pred -> switchId -> Pattern.port -> packet -> bool

val serialize_pkt : packet -> bytes

val eval_action : value -> NetCoreAction.NetCoreAction.t -> value list

val classify : pol -> value -> value list

