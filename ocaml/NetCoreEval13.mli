open Datatypes
open List0
open WordInterface
open OpenFlow0x04Types
open Packet
open MessagesDef
open NetCoreEval

type id =
  int

type modification = { modifyDlSrc : dlAddr option;
                      modifyDlDst : dlAddr option;
                      modifyDlVlan : dlVlan option option;
                      modifyDlVlanPcp : dlVlanPcp option;
                      modifyNwSrc : nwAddr option;
                      modifyNwDst : nwAddr option;
                      modifyNwTos : nwTos option;
                      modifyTpSrc : tpPort option;
                      modifyTpDst : tpPort option }

val modification_rect :
  (dlAddr option -> dlAddr option -> dlVlan option option -> dlVlanPcp option
  -> nwAddr option -> nwAddr option -> nwTos option -> tpPort option ->
  tpPort option -> 'a1) -> modification -> 'a1

val modification_rec :
  (dlAddr option -> dlAddr option -> dlVlan option option -> dlVlanPcp option
  -> nwAddr option -> nwAddr option -> nwTos option -> tpPort option ->
  tpPort option -> 'a1) -> modification -> 'a1

val modifyDlSrc : modification -> dlAddr option

val modifyDlDst : modification -> dlAddr option

val modifyDlVlan : modification -> dlVlan option option

val modifyDlVlanPcp : modification -> dlVlanPcp option

val modifyNwSrc : modification -> nwAddr option

val modifyNwDst : modification -> nwAddr option

val modifyNwTos : modification -> nwTos option

val modifyTpSrc : modification -> tpPort option

val modifyTpDst : modification -> tpPort option

val unmodified : modification

type act =
| Forward of modification * pseudoPort
| Group of groupId
| ActGetPkt of id

type pred = NetCoreEval.pred
(* | PrHdr of Pattern.pattern *)
(* | PrOnSwitch of switchId *)
(* | PrOr of pred * pred *)
(* | PrNot of pred *)
(* | PrAll *)
(* | PrNone *)

type input =
| InPkt of switchId * portId * packet * bufferId option

type output =
| OutPkt of switchId * pseudoPort * packet * (bufferId, bytes) sum
| OutGetPkt of id * switchId * portId * packet
| OutNothing

type pol =
| PoAtom of pred * act list
| PoUnion of pol * pol

val match_pred : pred -> switchId -> portId -> packet -> bool

val marshal_pkt : packet -> bytes

val maybe_modify :
  'a1 option -> (packet -> 'a1 -> packet) -> packet -> packet

val withVlanNone : dlVlan option option -> dlVlan option

val modify_pkt : modification -> packet -> packet

val eval_action : input -> act -> output

val classify : pol -> input -> output list

val convert_from_eval10 : NetCoreEval.pol -> pol
