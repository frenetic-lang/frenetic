open Packet
open OpenFlow0x01
open NetCore_Types.Internal
open NetCore_Pattern
open NetCore_Action.Output

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

let rec eval pol pkt = match pol with
  | PoAction action -> action
  | PoFilter pred0 ->
    let Pkt (sw, pt, pk, buf) = pkt in
    if match_pred pred0 sw pt pk then pass else drop
  | PoUnion (p1, p2) -> 
    par_action (eval p1 pkt) (eval p2 pkt)
  | PoSeq (pol1, pol2) -> 
    let act1 = eval pol1 pkt in
    let pkts' = eval_action pkt act1 in
    let act2 = List.fold_right par_action (List.map (eval pol2) pkts') drop in
    seq_action act1 act2
  | PoITE (pred, then_pol, else_pol) ->
    let Pkt (sw, pt, pk, buf) = pkt in
    if match_pred pred sw pt pk then
      eval then_pol pkt
    else
      eval else_pol pkt

let classify pol pkt = eval_action pkt (eval pol pkt)
