open Classifier
open Datatypes
open List0
open NetCoreEval
open OpenFlow0x01Types
open Types
open WordInterface

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val compile_pred :
    (bool coq_Classifier -> bool coq_Classifier) -> pred -> switchId -> bool
    coq_Classifier **)

let rec compile_pred opt pr sw =
  match pr with
  | PrHdr pat -> (pat, true) :: []
  | PrOnSwitch sw' ->
    if Word64.eq_dec sw sw' then (Pattern.Pattern.all, true) :: [] else []
  | PrOr (pr1, pr2) ->
    opt (union (||) (compile_pred opt pr1 sw) (compile_pred opt pr2 sw))
  | PrAnd (pr1, pr2) ->
    opt (inter (&&) (compile_pred opt pr1 sw) (compile_pred opt pr2 sw))
  | PrNot pr' ->
    opt
      (map (second negb)
        (app (compile_pred opt pr' sw) ((Pattern.Pattern.all, false) :: [])))
  | PrAll -> (Pattern.Pattern.all, true) :: []
  | PrNone -> []

(** val apply_act : act -> bool -> act **)

let apply_act a = function
| true -> a
| false -> empty_action

(** val compile_pol :
    (__ -> __ coq_Classifier -> __ coq_Classifier) -> pol -> switchId -> act
    coq_Classifier **)

let rec compile_pol opt p sw =
  match p with
  | PoAtom (pr, act0) ->
    Obj.magic opt __
      (map (second (apply_act act0))
        (app (compile_pred (Obj.magic opt __) pr sw) ((Pattern.Pattern.all,
          false) :: [])))
  | PoUnion (pol1, pol2) ->
    Obj.magic opt __
      (union par_action (compile_pol opt pol1 sw) (compile_pol opt pol2 sw))
  | PoSeq (pol1, pol2) ->
    Obj.magic opt __
      (sequence action_mask seq_action (compile_pol opt pol1 sw)
        (compile_pol opt pol2 sw))

(** val strip_empty_rules : 'a1 coq_Classifier -> 'a1 coq_Classifier **)

let rec strip_empty_rules = function
| [] -> []
| p :: cf0 ->
  let (pat, acts) = p in
  if Pattern.Pattern.is_empty pat
  then strip_empty_rules cf0
  else (pat, acts) :: (strip_empty_rules cf0)

(** val no_opt : 'a1 coq_Classifier -> 'a1 coq_Classifier **)

let no_opt x =
  id x

(** val compile_no_opt : pol -> switchId -> act coq_Classifier **)

let compile_no_opt =
  compile_pol (fun _ -> no_opt)

(** val compile_opt : pol -> switchId -> act coq_Classifier **)

let compile_opt =
  compile_pol (fun _ x -> strip_empty_rules (elim_shadowed x))

