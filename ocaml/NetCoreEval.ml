open List
open Misc
open NetworkPacket
open OpenFlow0x01Types
open WordInterface

module Pattern = NetCoreAction.NetCoreAction.Pattern

type pattern = Pattern.t

type pred =
| PrHdr of pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

(** val pred_rect :
    (pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 ->
    'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 -> 'a1) ->
    'a1 -> 'a1 -> pred -> 'a1 **)

let rec pred_rect f f0 f1 f2 f3 f4 f5 = function
| PrHdr p0 -> f p0
| PrOnSwitch s -> f0 s
| PrOr (p0, p1) ->
  f1 p0 (pred_rect f f0 f1 f2 f3 f4 f5 p0) p1
    (pred_rect f f0 f1 f2 f3 f4 f5 p1)
| PrAnd (p0, p1) ->
  f2 p0 (pred_rect f f0 f1 f2 f3 f4 f5 p0) p1
    (pred_rect f f0 f1 f2 f3 f4 f5 p1)
| PrNot p0 -> f3 p0 (pred_rect f f0 f1 f2 f3 f4 f5 p0)
| PrAll -> f4
| PrNone -> f5

(** val pred_rec :
    (pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 ->
    'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 -> 'a1) ->
    'a1 -> 'a1 -> pred -> 'a1 **)

let rec pred_rec f f0 f1 f2 f3 f4 f5 = function
| PrHdr p0 -> f p0
| PrOnSwitch s -> f0 s
| PrOr (p0, p1) ->
  f1 p0 (pred_rec f f0 f1 f2 f3 f4 f5 p0) p1
    (pred_rec f f0 f1 f2 f3 f4 f5 p1)
| PrAnd (p0, p1) ->
  f2 p0 (pred_rec f f0 f1 f2 f3 f4 f5 p0) p1
    (pred_rec f f0 f1 f2 f3 f4 f5 p1)
| PrNot p0 -> f3 p0 (pred_rec f f0 f1 f2 f3 f4 f5 p0)
| PrAll -> f4
| PrNone -> f5

type pol =
| PoAction of NetCoreAction.NetCoreAction.t
| PoFilter of pred
| PoUnion of pol * pol
| PoSeq of pol * pol

(** val pol_rect :
    (NetCoreAction.NetCoreAction.t -> 'a1) -> (pred -> 'a1) -> (pol -> 'a1 ->
    pol -> 'a1 -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> pol -> 'a1 **)

let rec pol_rect f f0 f1 f2 = function
| PoAction t0 -> f t0
| PoFilter p0 -> f0 p0
| PoUnion (p0, p1) ->
  f1 p0 (pol_rect f f0 f1 f2 p0) p1 (pol_rect f f0 f1 f2 p1)
| PoSeq (p0, p1) ->
  f2 p0 (pol_rect f f0 f1 f2 p0) p1 (pol_rect f f0 f1 f2 p1)

(** val pol_rec :
    (NetCoreAction.NetCoreAction.t -> 'a1) -> (pred -> 'a1) -> (pol -> 'a1 ->
    pol -> 'a1 -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> pol -> 'a1 **)

let rec pol_rec f f0 f1 f2 = function
| PoAction t0 -> f t0
| PoFilter p0 -> f0 p0
| PoUnion (p0, p1) ->
  f1 p0 (pol_rec f f0 f1 f2 p0) p1 (pol_rec f f0 f1 f2 p1)
| PoSeq (p0, p1) -> f2 p0 (pol_rec f f0 f1 f2 p0) p1 (pol_rec f f0 f1 f2 p1)

type value =
| Pkt of switchId * NetCoreAction.NetCoreAction.port * packet
   * (bufferId, bytes) sum

(** val value_rect :
    (switchId -> NetCoreAction.NetCoreAction.port -> packet -> (bufferId,
    bytes) sum -> 'a1) -> value -> 'a1 **)

let value_rect f = function
| Pkt (x, x0, x1, x2) -> f x x0 x1 x2

(** val value_rec :
    (switchId -> NetCoreAction.NetCoreAction.port -> packet -> (bufferId,
    bytes) sum -> 'a1) -> value -> 'a1 **)

let value_rec f = function
| Pkt (x, x0, x1, x2) -> f x x0 x1 x2

(** val match_pred : pred -> switchId -> Pattern.port -> packet -> bool **)

let rec match_pred pr sw pt pk =
  match pr with
  | PrHdr pat -> Pattern.match_packet pt pk pat
  | PrOnSwitch sw' -> if Word64.eq_dec sw sw' then true else false
  | PrOr (p1, p2) -> (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrAnd (p1, p2) -> (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrNot p' -> not (match_pred p' sw pt pk)
  | PrAll -> true
  | PrNone -> false

(** val serialize_pkt : packet -> bytes **)

let serialize_pkt = Packet_Parser.serialize_packet

(** val eval_action :
    value -> NetCoreAction.NetCoreAction.t -> value list **)

let eval_action inp act =
  let Pkt (sw, pt, pk, buf) = inp in
  map (fun ptpk -> let (pt', pk') = ptpk in Pkt (sw, pt', pk', buf))
    (NetCoreAction.NetCoreAction.apply_action act (pt, pk))

(** val classify : pol -> value -> value list **)

let rec classify p inp =
  match p with
  | PoAction action -> eval_action inp action
  | PoFilter pred0 ->
    let Pkt (sw, pt, pk, buf) = inp in
    if match_pred pred0 sw pt pk then inp :: [] else []
  | PoUnion (p1, p2) -> (classify p1 inp) @ (classify p2 inp)
  | PoSeq (p1, p2) -> concat_map (classify p2) (classify p1 inp)

