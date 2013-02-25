open Datatypes
open List0
open OpenFlow0x04Types
open MessagesDef
open Packet
open WordInterface
open NetCoreEval

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

