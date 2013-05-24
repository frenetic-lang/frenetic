open Packet
open OpenFlow0x01
open NetCore_Types.Internal
open NetCore_Pattern

let rec match_pred pr sw pt pk =
  match pr with
    | PrHdr pat -> 
      NetCore_Pattern.match_packet pt pk pat
    | PrOnSwitch sw' -> 
      if sw = sw' then true else false
    | PrOr (p1, p2) -> 
      (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrAnd (p1, p2) -> 
      (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | PrNot p' -> 
      not (match_pred p' sw pt pk)
    | PrAll -> 
      true
    | PrNone -> 
      false

let eval_action inp act =
  let Pkt (sw, pt, pk, pay) = inp in
  List.map (fun (sw', pt',pk') -> Pkt (sw', pt', pk', pay))
    (NetCore_Action.Output.apply_action act (sw, pt, pk))

let rec classify p inp = match p with 
  | PoAction action -> 
    eval_action inp action
  | PoFilter pred0 ->
    let Pkt (sw, pt, pk, buf) = inp in
    if match_pred pred0 sw pt pk then [inp] else []
  | PoUnion (p1, p2) -> 
    classify p1 inp @ classify p2 inp
  | PoSeq (p1, p2) -> 
    NetCore_Action.concat_map (classify p2) (classify p1 inp)
  | PoITE (pred, then_pol, else_pol) ->
    let Pkt (sw, pt, pk, buf) = inp in
    if match_pred pred sw pt pk then
      classify then_pol inp
    else
      classify else_pol inp
