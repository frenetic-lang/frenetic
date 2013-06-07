open Packet
open OpenFlow0x01
open NetCore_Types
open NetCore_Action.Output

let rec match_pred pr sw pt pk =
  match pr with
    | Hdr pat -> 
      NetCore_Pattern.match_packet pt pk pat
    | OnSwitch sw' -> 
      if sw = sw' then true else false
    | Or (p1, p2) -> 
      (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | And (p1, p2) -> 
      (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
    | Not p' -> 
      not (match_pred p' sw pt pk)
    | Everything -> 
      true
    | Nothing -> 
      false

let eval_action inp act =
  let Pkt (sw, pt, pk, pay) = inp in
  List.map (fun (sw', pt',pk') -> Pkt (sw', pt', pk', pay))
    (NetCore_Action.Output.apply_action act (sw, pt, pk))

(* Do side effects, then return switch action. *)
let rec eval_helper_atom atom pkt = 
  let Pkt (sw, pt, pk, _) = pkt in
  match atom with
  | SwitchAction _ -> [atom]
  | ControllerAction f -> 
    eval_helper_action (f sw pt pk) pkt
  | ControllerQuery _ -> []

and eval_helper_action act pkt =
  Frenetic_List.concat_map (fun a -> eval_helper_atom a pkt) act

let rec eval pol pkt = match pol with
  | HandleSwitchEvent _ -> drop
  | Action action -> eval_helper_action action pkt
  | Filter pred0 ->
    let Pkt (sw, pt, pk, buf) = pkt in
    if match_pred pred0 sw pt pk then pass else drop
  | Union (p1, p2) -> 
    par_action (eval p1 pkt) (eval p2 pkt)
  | Seq (pol1, pol2) -> 
    let act1 = eval pol1 pkt in
    let pkts' = eval_action pkt act1 in
    let act2 = List.fold_right par_action (List.map (eval pol2) pkts') drop in
    seq_action act1 act2
  | ITE (pred, then_pol, else_pol) ->
    let Pkt (sw, pt, pk, buf) = pkt in
    if match_pred pred sw pt pk then
      eval then_pol pkt
    else
      eval else_pol pkt

let classify pol pkt = eval_action pkt (eval pol pkt)

(* Interprets a predicate as a predicate on switches. Hdr returns true *)
let rec sw_pred sw = function
  | Hdr pat -> true
  | OnSwitch sw' -> sw = sw'
  | Or (p1, p2) -> sw_pred sw p1 || sw_pred sw p2
  | And (p1, p2) -> sw_pred sw p1 && sw_pred sw p2
  | Not p -> not (sw_pred sw p)
  | Everything -> true
  | Nothing -> false

let event_switch = function
  | SwitchUp (sw, _) -> sw
  | SwitchDown sw -> sw

let rec apply_switch_events evt = function
  | Action _ -> false
  | Filter pred -> sw_pred (event_switch evt) pred
  | Union (pol1, pol2) ->
    apply_switch_events evt pol1 || apply_switch_events evt pol2
  | Seq (pol1, pol2) ->
    apply_switch_events evt pol1 && apply_switch_events evt pol2
  | ITE (pred, pol1, pol2) ->
    if sw_pred (event_switch evt) pred then 
      apply_switch_events evt pol1 
    else
      apply_switch_events evt pol2
  | HandleSwitchEvent handler -> handler evt; true (* return boolean? *)

let handle_switch_events evt pol =
  let _ = apply_switch_events evt pol in
  ()
