open Misc
open List
open NetworkPacket
open OpenFlow0x01Types
open Word

type pred =
| PrHdr of Pattern.t
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

type pol =
| PoAction of Action.Output.t
| PoFilter of pred
| PoUnion of pol * pol
| PoSeq of pol * pol

type value =
| Pkt of switchId * Pattern.port * packet * (bufferId, bytes) sum

val match_pred : pred -> switchId -> Pattern.port -> packet -> bool

val serialize_pkt : packet -> bytes

val eval_action : value -> Action.Output.t -> value list

val classify : pol -> value -> value list

val pol_to_string : pol -> string

val pred_to_string : pred -> string

