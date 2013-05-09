open Datatypes
open List0
open NetworkPacket
open Peano

type 'a coq_Classifier = (Pattern.pattern * 'a) list

(** val scan : 'a1 -> 'a1 coq_Classifier -> portId -> packet -> 'a1 **)

let rec scan default classifier pt pk =
  match classifier with
  | [] -> default
  | p :: rest ->
    let (pat0, a) = p in
    if Pattern.Pattern.match_packet pt pk pat0
    then a
    else scan default rest pt pk

(** val inter_entry :
    ('a1 -> 'a1 -> 'a2) -> 'a1 coq_Classifier -> (Pattern.pattern * 'a1) ->
    (Pattern.Pattern.pat * 'a2) list **)

let inter_entry f cl = function
| (pat0, act) ->
  fold_right (fun v' acc ->
    let (pat', act') = v' in
    ((Pattern.Pattern.inter pat0 pat'), (f act act')) :: acc) [] cl

(** val inter :
    ('a1 -> 'a1 -> 'a2) -> 'a1 coq_Classifier -> 'a1 coq_Classifier ->
    (Pattern.Pattern.pat * 'a2) list **)

let inter f cl1 cl2 =
  fold_right (fun v acc -> app (inter_entry f cl2 v) acc) [] cl1

(** val union :
    ('a1 -> 'a1 -> 'a1) -> 'a1 coq_Classifier -> 'a1 coq_Classifier ->
    (Pattern.Pattern.pat * 'a1) list **)

let union f cl1 cl2 =
  app (inter f cl1 cl2) (app cl1 cl2)

(** val elim_shadowed_helper :
    'a1 coq_Classifier -> 'a1 coq_Classifier -> 'a1 coq_Classifier **)

let rec elim_shadowed_helper prefix = function
| [] -> prefix
| p :: cf' ->
  let (pat0, act) = p in
  if existsb (fun entry ->
       let (pat', act0) = entry in Pattern.Pattern.beq pat0 pat') prefix
  then elim_shadowed_helper prefix cf'
  else elim_shadowed_helper (app prefix ((pat0, act) :: [])) cf'

(** val elim_shadowed : 'a1 coq_Classifier -> 'a1 coq_Classifier **)

let elim_shadowed cf =
  elim_shadowed_helper [] cf

(** val prioritize :
    int -> 'a1 coq_Classifier -> ((int * Pattern.pattern) * 'a1) list **)

let rec prioritize prio = function
| [] -> []
| p :: lst' ->
  let (pat0, act) = p in ((prio, pat0), act) :: (prioritize (pred prio) lst')

(** val sequence_atom :
    ('a1 -> Pattern.pattern) -> ('a1 -> 'a1 -> 'a1) -> Pattern.pattern -> 'a1
    -> Pattern.pattern -> 'a1 -> Pattern.Pattern.pat * 'a1 **)

let sequence_atom mask0 seq_action p1 a1 p2 a2 =
  ((Pattern.Pattern.inter p1
     (Pattern.Pattern.mask (Pattern.Pattern.inter p2 (mask0 a2)) (mask0 a2))),
    (seq_action a1 a2))

(** val sequence_helper :
    ('a1 -> Pattern.pattern) -> ('a1 -> 'a1 -> 'a1) -> Pattern.pattern -> 'a1
    -> 'a1 coq_Classifier -> (Pattern.Pattern.pat * 'a1) list **)

let sequence_helper mask0 seq_action p1 a1 tbl2 =
  fold_right (fun x acc ->
    let (p2, a2) = x in (sequence_atom mask0 seq_action p1 a1 p2 a2) :: acc)
    [] tbl2

(** val sequence :
    ('a1 -> Pattern.pattern) -> ('a1 -> 'a1 -> 'a1) -> 'a1 coq_Classifier ->
    'a1 coq_Classifier -> (Pattern.Pattern.pat * 'a1) list **)

let sequence mask0 seq_action tbl1 tbl2 =
  fold_right (fun x acc ->
    let (p1, a1) = x in app (sequence_helper mask0 seq_action p1 a1 tbl2) acc)
    [] tbl1

