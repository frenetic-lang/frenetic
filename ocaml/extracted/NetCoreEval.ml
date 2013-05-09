open Datatypes
open List0
open NetworkPacket
open OpenFlow0x01Types
open Types
open WordInterface

type id =
  int
  (* singleton inductive, whose constructor was MkId *)

(** val id_rect : (int -> 'a1) -> id -> 'a1 **)

let id_rect f i =
  f i

(** val id_rec : (int -> 'a1) -> id -> 'a1 **)

let id_rec f i =
  f i

type modification = { modifyDlSrc : dlAddr option;
                      modifyDlDst : dlAddr option;
                      modifyDlVlan : dlVlan option option;
                      modifyDlVlanPcp : dlVlanPcp option;
                      modifyNwSrc : nwAddr option;
                      modifyNwDst : nwAddr option;
                      modifyNwTos : nwTos option;
                      modifyTpSrc : tpPort option;
                      modifyTpDst : tpPort option }

(** val modification_rect :
    (dlAddr option -> dlAddr option -> dlVlan option option -> dlVlanPcp
    option -> nwAddr option -> nwAddr option -> nwTos option -> tpPort option
    -> tpPort option -> 'a1) -> modification -> 'a1 **)

let modification_rect f m =
  let { modifyDlSrc = x; modifyDlDst = x0; modifyDlVlan = x1;
    modifyDlVlanPcp = x2; modifyNwSrc = x3; modifyNwDst = x4; modifyNwTos =
    x5; modifyTpSrc = x6; modifyTpDst = x7 } = m
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7

(** val modification_rec :
    (dlAddr option -> dlAddr option -> dlVlan option option -> dlVlanPcp
    option -> nwAddr option -> nwAddr option -> nwTos option -> tpPort option
    -> tpPort option -> 'a1) -> modification -> 'a1 **)

let modification_rec f m =
  let { modifyDlSrc = x; modifyDlDst = x0; modifyDlVlan = x1;
    modifyDlVlanPcp = x2; modifyNwSrc = x3; modifyNwDst = x4; modifyNwTos =
    x5; modifyTpSrc = x6; modifyTpDst = x7 } = m
  in
  f x x0 x1 x2 x3 x4 x5 x6 x7

(** val modifyDlSrc : modification -> dlAddr option **)

let modifyDlSrc x = x.modifyDlSrc

(** val modifyDlDst : modification -> dlAddr option **)

let modifyDlDst x = x.modifyDlDst

(** val modifyDlVlan : modification -> dlVlan option option **)

let modifyDlVlan x = x.modifyDlVlan

(** val modifyDlVlanPcp : modification -> dlVlanPcp option **)

let modifyDlVlanPcp x = x.modifyDlVlanPcp

(** val modifyNwSrc : modification -> nwAddr option **)

let modifyNwSrc x = x.modifyNwSrc

(** val modifyNwDst : modification -> nwAddr option **)

let modifyNwDst x = x.modifyNwDst

(** val modifyNwTos : modification -> nwTos option **)

let modifyNwTos x = x.modifyNwTos

(** val modifyTpSrc : modification -> tpPort option **)

let modifyTpSrc x = x.modifyTpSrc

(** val modifyTpDst : modification -> tpPort option **)

let modifyTpDst x = x.modifyTpDst

(** val unmodified : modification **)

let unmodified =
  { modifyDlSrc = None; modifyDlDst = None; modifyDlVlan = None;
    modifyDlVlanPcp = None; modifyNwSrc = None; modifyNwDst = None;
    modifyNwTos = None; modifyTpSrc = None; modifyTpDst = None }

type act = { modifications : modification; toPorts : pseudoPort list;
             queries : id list }

(** val act_rect :
    (modification -> pseudoPort list -> id list -> 'a1) -> act -> 'a1 **)

let act_rect f a =
  let { modifications = x; toPorts = x0; queries = x1 } = a in f x x0 x1

(** val act_rec :
    (modification -> pseudoPort list -> id list -> 'a1) -> act -> 'a1 **)

let act_rec f a =
  let { modifications = x; toPorts = x0; queries = x1 } = a in f x x0 x1

(** val modifications : act -> modification **)

let modifications x = x.modifications

(** val toPorts : act -> pseudoPort list **)

let toPorts x = x.toPorts

(** val queries : act -> id list **)

let queries x = x.queries

(** val empty_action : act **)

let empty_action =
  { modifications = unmodified; toPorts = []; queries = [] }

(** val is_some : 'a1 option -> bool **)

let is_some = function
| Some a -> true
| None -> false

(** val mod_mask : modification -> Pattern.pattern **)

let mod_mask mod0 =
  let { modifyDlSrc = dlSrc0; modifyDlDst = dlDst0; modifyDlVlan = dlVlan0;
    modifyDlVlanPcp = dlVlanPcp0; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos0; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mod0
  in
  fold_right Pattern.Pattern.inter Pattern.Pattern.all
    ((if is_some dlSrc0
      then Pattern.Pattern.dlSrc Word48.zero
      else Pattern.Pattern.all) :: ((if is_some dlDst0
                                     then Pattern.Pattern.dlDst Word48.zero
                                     else Pattern.Pattern.all) :: []))

(** val action_mask : act -> Pattern.pattern **)

let action_mask a =
  mod_mask a.modifications

(** val par_action : act -> act -> act **)

let par_action a1 a2 =
  let { modifications = m1; toPorts = p1; queries = q1 } = a1 in
  let { modifications = m2; toPorts = p2; queries = q2 } = a2 in
  { modifications = m1; toPorts = (app p1 p2); queries = (app q1 q2) }

(** val override : 'a1 option -> 'a1 option -> 'a1 option **)

let override x y = match y with
| Some a -> y
| None -> x

(** val seq_mod : modification -> modification -> modification **)

let seq_mod m1 m2 =
  let { modifyDlSrc = dlSrc1; modifyDlDst = dlDst1; modifyDlVlan = dlVlan1;
    modifyDlVlanPcp = dlVlanPcp1; modifyNwSrc = nwSrc1; modifyNwDst = nwDst1;
    modifyNwTos = nwTos1; modifyTpSrc = tpSrc1; modifyTpDst = tpDst1 } = m1
  in
  let { modifyDlSrc = dlSrc2; modifyDlDst = dlDst2; modifyDlVlan = dlVlan2;
    modifyDlVlanPcp = dlVlanPcp2; modifyNwSrc = nwSrc2; modifyNwDst = nwDst2;
    modifyNwTos = nwTos2; modifyTpSrc = tpSrc2; modifyTpDst = tpDst2 } = m2
  in
  { modifyDlSrc = (override dlSrc1 dlSrc2); modifyDlDst =
  (override dlDst1 dlDst2); modifyDlVlan = (override dlVlan1 dlVlan2);
  modifyDlVlanPcp = (override dlVlanPcp1 dlVlanPcp2); modifyNwSrc =
  (override nwSrc1 nwSrc2); modifyNwDst = (override nwDst1 nwDst2);
  modifyNwTos = (override nwTos1 nwTos2); modifyTpSrc =
  (override tpSrc1 tpSrc2); modifyTpDst = (override tpDst1 tpDst2) }

(** val seq_action : act -> act -> act **)

let seq_action a1 a2 =
  let { modifications = m1; toPorts = p1; queries = q1 } = a1 in
  let { modifications = m2; toPorts = p2; queries = q2 } = a2 in
  { modifications = (seq_mod m1 m2); toPorts = p2; queries = (app q1 q2) }

type pred =
| PrHdr of Pattern.pattern
| PrOnSwitch of switchId
| PrOr of pred * pred
| PrAnd of pred * pred
| PrNot of pred
| PrAll
| PrNone

(** val pred_rect :
    (Pattern.pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred ->
    'a1 -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 ->
    'a1) -> 'a1 -> 'a1 -> pred -> 'a1 **)

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
    (Pattern.pattern -> 'a1) -> (switchId -> 'a1) -> (pred -> 'a1 -> pred ->
    'a1 -> 'a1) -> (pred -> 'a1 -> pred -> 'a1 -> 'a1) -> (pred -> 'a1 ->
    'a1) -> 'a1 -> 'a1 -> pred -> 'a1 **)

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
| PoAtom of pred * act
| PoUnion of pol * pol
| PoSeq of pol * pol

(** val pol_rect :
    (pred -> act -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> (pol -> 'a1
    -> pol -> 'a1 -> 'a1) -> pol -> 'a1 **)

let rec pol_rect f f0 f1 = function
| PoAtom (p0, a) -> f p0 a
| PoUnion (p0, p1) -> f0 p0 (pol_rect f f0 f1 p0) p1 (pol_rect f f0 f1 p1)
| PoSeq (p0, p1) -> f1 p0 (pol_rect f f0 f1 p0) p1 (pol_rect f f0 f1 p1)

(** val pol_rec :
    (pred -> act -> 'a1) -> (pol -> 'a1 -> pol -> 'a1 -> 'a1) -> (pol -> 'a1
    -> pol -> 'a1 -> 'a1) -> pol -> 'a1 **)

let rec pol_rec f f0 f1 = function
| PoAtom (p0, a) -> f p0 a
| PoUnion (p0, p1) -> f0 p0 (pol_rec f f0 f1 p0) p1 (pol_rec f f0 f1 p1)
| PoSeq (p0, p1) -> f1 p0 (pol_rec f f0 f1 p0) p1 (pol_rec f f0 f1 p1)

type input =
| InPkt of switchId * portId * packet * bufferId option

(** val input_rect :
    (switchId -> portId -> packet -> bufferId option -> 'a1) -> input -> 'a1 **)

let input_rect f = function
| InPkt (x, x0, x1, x2) -> f x x0 x1 x2

(** val input_rec :
    (switchId -> portId -> packet -> bufferId option -> 'a1) -> input -> 'a1 **)

let input_rec f = function
| InPkt (x, x0, x1, x2) -> f x x0 x1 x2

type output =
| OutPkt of switchId * pseudoPort * packet * (bufferId, bytes) sum
| OutGetPkt of id * switchId * portId * packet
| OutNothing

(** val output_rect :
    (switchId -> pseudoPort -> packet -> (bufferId, bytes) sum -> 'a1) -> (id
    -> switchId -> portId -> packet -> 'a1) -> 'a1 -> output -> 'a1 **)

let output_rect f f0 f1 = function
| OutPkt (x, x0, x1, x2) -> f x x0 x1 x2
| OutGetPkt (x, x0, x1, x2) -> f0 x x0 x1 x2
| OutNothing -> f1

(** val output_rec :
    (switchId -> pseudoPort -> packet -> (bufferId, bytes) sum -> 'a1) -> (id
    -> switchId -> portId -> packet -> 'a1) -> 'a1 -> output -> 'a1 **)

let output_rec f f0 f1 = function
| OutPkt (x, x0, x1, x2) -> f x x0 x1 x2
| OutGetPkt (x, x0, x1, x2) -> f0 x x0 x1 x2
| OutNothing -> f1

(** val is_OutPkt : output -> bool **)

let is_OutPkt = function
| OutPkt (s, p, p0, s0) -> true
| _ -> false

(** val match_pred : pred -> switchId -> portId -> packet -> bool **)

let rec match_pred pr sw pt pk =
  match pr with
  | PrHdr pat -> Pattern.Pattern.match_packet pt pk pat
  | PrOnSwitch sw' -> if Word64.eq_dec sw sw' then true else false
  | PrOr (p1, p2) -> (||) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrAnd (p1, p2) -> (&&) (match_pred p1 sw pt pk) (match_pred p2 sw pt pk)
  | PrNot p' -> negb (match_pred p' sw pt pk)
  | PrAll -> true
  | PrNone -> false

(** val serialize_pkt : packet -> bytes **)

let serialize_pkt = Packet_Parser.serialize_packet

(** val maybe_modify :
    'a1 option -> (packet -> 'a1 -> packet) -> packet -> packet **)

let maybe_modify newVal modifier pk =
  match newVal with
  | Some v -> modifier pk v
  | None -> pk

(** val withVlanNone : dlVlan option option -> dlVlan option **)

let withVlanNone = function
| Some y ->
  (match y with
   | Some n -> Some n
   | None -> Some coq_VLAN_NONE)
| None -> None

(** val modify_pkt : modification -> packet -> packet **)

let modify_pkt mods pk =
  let { modifyDlSrc = dlSrc0; modifyDlDst = dlDst0; modifyDlVlan = dlVlan0;
    modifyDlVlanPcp = dlVlanPcp0; modifyNwSrc = nwSrc; modifyNwDst = nwDst;
    modifyNwTos = nwTos0; modifyTpSrc = tpSrc; modifyTpDst = tpDst } = mods
  in
  maybe_modify dlSrc0 setDlSrc
    (maybe_modify dlDst0 setDlDst
      (maybe_modify (withVlanNone dlVlan0) setDlVlan
        (maybe_modify dlVlanPcp0 setDlVlanPcp
          (maybe_modify nwSrc setNwSrc
            (maybe_modify nwDst setNwDst
              (maybe_modify nwTos0 setNwTos
                (maybe_modify tpSrc setTpSrc
                  (maybe_modify tpDst setTpDst pk))))))))

(** val outp_to_inp : output -> input option **)

let outp_to_inp = function
| OutPkt (sw, p, pk, s) ->
  (match p with
   | PhysicalPort pt ->
     (match s with
      | Coq_inl bufId -> Some (InPkt (sw, pt, pk, (Some bufId)))
      | Coq_inr b -> None)
   | _ -> None)
| _ -> None

(** val eval_action : input -> act -> output list **)

let eval_action inp act0 =
  let { modifications = mods; toPorts = ports; queries = queries0 } = act0 in
  let InPkt (sw, pt, pk, buf) = inp in
  app
    (map (fun pt0 -> OutPkt (sw, pt0, (modify_pkt mods pk),
      (match buf with
       | Some b -> Coq_inl b
       | None -> Coq_inr (serialize_pkt (modify_pkt mods pk))))) ports)
    (map (fun qid -> OutGetPkt (qid, sw, pt, pk)) queries0)

(** val classify : pol -> input -> output list **)

let rec classify p inp =
  match p with
  | PoAtom (pr, actions) ->
    let InPkt (sw, pt, pk, buf) = inp in
    eval_action inp
      (if match_pred pr sw pt pk then actions else empty_action)
  | PoUnion (p1, p2) -> app (classify p1 inp) (classify p2 inp)
  | PoSeq (p1, p2) ->
    let (outPkts1, queries1) = partition is_OutPkt (classify p1 inp) in
    app queries1 (concat_map (classify p2) (filter_map outp_to_inp outPkts1))

