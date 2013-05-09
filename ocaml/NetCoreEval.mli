open Datatypes
open List0
open NetworkPacket
open OpenFlow0x01Types
open Types
open WordInterface

type id =
  int
  (* singleton inductive, whose constructor was MkId *)

val id_rect : (int -> 'a1) -> id -> 'a1

val id_rec : (int -> 'a1) -> id -> 'a1

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

type act = { modifications : modification; toPorts : pseudoPort list;
             queries : id list }

val act_rect :
  (modification -> pseudoPort list -> id list -> 'a1) -> act -> 'a1

val act_rec :
  (modification -> pseudoPort list -> id list -> 'a1) -> act -> 'a1

val modifications : act -> modification

val toPorts : act -> pseudoPort list

val queries : act -> id list

val empty_action : act

val is_some : 'a1 option -> bool

val mod_mask : modification -> Pattern.pattern

val action_mask : act -> Pattern.pattern

val par_action : act -> act -> act

val override : 'a1 option -> 'a1 option -> 'a1 option

val seq_mod : modification -> modification -> modification

val seq_action : act -> act -> act

type pred =
| PrHdr of Pattern.pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

val pred_rect :
  (Pattern.pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred ->
  'a1 -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 -> 'a1)
  -> 'a1 -> 'a1 -> pred -> 'a1

val pred_rec :
  (Pattern.pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred ->
  'a1 -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 -> 'a1)
  -> 'a1 -> 'a1 -> pred -> 'a1

type pol =
| PoAtom of pred * act
| PoUnion of pol * pol
| PoSeq of pol * pol

val pol_rect :
  (pred -> act -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> (pol -> 'a1 ->
  pol -> 'a1 -> 'a1) -> pol -> 'a1

val pol_rec :
  (pred -> act -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> (pol -> 'a1 ->
  pol -> 'a1 -> 'a1) -> pol -> 'a1

type input =
| InPkt of switchId * portId * packet * bufferId option

val input_rect :
  (switchId -> portId -> packet -> bufferId option -> 'a1) -> input -> 'a1

val input_rec :
  (switchId -> portId -> packet -> bufferId option -> 'a1) -> input -> 'a1

type output =
| OutPkt of switchId * pseudoPort * packet * (bufferId, bytes) sum
| OutGetPkt of id * switchId * portId * packet
| OutNothing

val output_rect :
  (switchId -> pseudoPort -> packet -> (bufferId, bytes) sum -> 'a1) -> (id
  -> switchId -> portId -> packet -> 'a1) -> 'a1 -> output -> 'a1

val output_rec :
  (switchId -> pseudoPort -> packet -> (bufferId, bytes) sum -> 'a1) -> (id
  -> switchId -> portId -> packet -> 'a1) -> 'a1 -> output -> 'a1

val is_OutPkt : output -> bool

val match_pred : pred -> switchId -> portId -> packet -> bool

val serialize_pkt : packet -> bytes

val maybe_modify :
  'a1 option -> (packet -> 'a1 -> packet) -> packet -> packet

val withVlanNone : dlVlan option option -> dlVlan option

val modify_pkt : modification -> packet -> packet

val outp_to_inp : output -> input option

val eval_action : input -> act -> output list

val classify : pol -> input -> output list

