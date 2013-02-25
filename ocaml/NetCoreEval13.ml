open Datatypes
open List0
open OpenFlow0x04Types
open MessagesDef
open Packet
open WordInterface
open NetCoreEval

type modification = NetCoreEval.modification
let modify_pkt = NetCoreEval.modify_pkt

let withVlanNone = NetCoreEval.withVlanNone
let maybe_modify = NetCoreEval.maybe_modify
let marshal_pkt = NetCoreEval.marshal_pkt
let match_pred = NetCoreEval.match_pred
type output = NetCoreEval.output
type input = NetCoreEval.input
type pred = NetCoreEval.pred
let unmodified = NetCoreEval.unmodified
let modifyTpDst = NetCoreEval.modifyTpDst
let modifyTpSrc = NetCoreEval.modifyTpSrc
let modifyNwTos = NetCoreEval.modifyNwTos
let modifyNwDst = NetCoreEval.modifyNwDst
let modifyNwSrc = NetCoreEval.modifyNwSrc
let modifyDlVlanPcp = NetCoreEval.modifyDlVlanPcp
let modifyDlVlan = NetCoreEval.modifyDlVlan
let modifyDlDst = NetCoreEval.modifyDlDst
let modifyDlSrc = NetCoreEval.modifyDlSrc
let modification_rec = NetCoreEval.modification_rec
let modification_rect = NetCoreEval.modification_rect
type id = NetCoreEval.id

type act =
| Forward of modification * pseudoPort
| Group of groupId
| ActGetPkt of id

type pol =
| PoAtom of pred * act list
| PoUnion of pol * pol
(** val marshal_pkt : packet -> bytes **)

let eval_action inp = function
| Forward (mods, pp) ->
  let InPkt (sw, p, pk, buf) = inp in
  OutPkt (sw, pp, (modify_pkt mods pk),
  (match buf with
   | Some b -> Coq_inl b
   | None -> Coq_inr (marshal_pkt (modify_pkt mods pk))))
| ActGetPkt x ->
  let InPkt (sw, pt, pk, buf) = inp in OutGetPkt (x, sw, pt, pk)

(** val classify : pol -> input -> output list **)

let rec classify p inp =
  match p with
  | PoAtom (pr, actions) ->
    let InPkt (sw, pt, pk, buf) = inp in
    if match_pred pr sw pt pk then map (eval_action inp) actions else []
  | PoUnion (p1, p2) -> app (classify p1 inp) (classify p2 inp)

let eval_to_eval13 act = match act with
  | NetCoreEval.Forward (a,b) -> Forward (a,b)
  | NetCoreEval.ActGetPkt a -> ActGetPkt a

let convert_from_eval10 = List.map (fun (pat, acts) -> (pat, List.map eval_to_eval13 acts)) 
