open Misc
open NetworkPacket
open OpenFlow0x01Types
open Pattern

module type ACTION = sig 
  module Pattern : PATTERN
  
  type pattern = Pattern.t
  
  type port = Pattern.port
  
  type t 
  
  type e 
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (port * packet) -> (port * packet) option
  
  val apply_action : t -> (port * packet) -> (port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> pattern -> pattern
  
  val domain : e -> pattern
 end

module type CLASSIFIER = sig 
  module Action : ACTION

  type pattern = Action.pattern
  
  type port = Action.port
  
  type action = Action.t
  
  type t = (pattern * action) list
  
  val scan : t -> port -> packet -> action
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : action list -> Action.t
 end

module type MAKE  = functor (Action : ACTION) -> 
sig 
  include CLASSIFIER
  module Pattern : PATTERN 
end
  with module Action = Action
  and module Pattern = Action.Pattern

module Make : MAKE = 
 functor (Action:ACTION) ->
 struct 
  module Action = Action
  module Pattern = Action.Pattern
  
  type pattern = Action.pattern
  
  type port = Action.port
  
  type action = Action.t
  
  type t = (pattern * action) list
  
  (** val scan' : action -> t -> port -> packet -> action **)
  
  let rec scan' default classifier pt pk =
    match classifier with
    | [] -> default
    | p :: rest ->
      let (pat, a) = p in
      if Pattern.match_packet pt pk pat then a else scan' default rest pt pk
  
  (** val scan : t -> port -> packet -> action **)
  
  let scan =
    scan' Action.drop
  
  (** val elim_shadowed_helper : t -> t -> t **)
  
  let rec elim_shadowed_helper prefix = function
  | [] -> prefix
  | p :: cf' ->
    let (pat, act) = p in
    if List.exists (fun entry ->
         let (pat', act0) = entry in Pattern.beq pat pat') prefix
    then elim_shadowed_helper prefix cf'
    else elim_shadowed_helper (prefix @ ((pat, act) :: [])) cf'
  
  (** val elim_shadowed : t -> t **)
  
  let elim_shadowed cf =
    elim_shadowed_helper [] cf
  
  (** val strip_empty_rules : t -> t **)
  
  let rec strip_empty_rules = function
  | [] -> []
  | p :: cf0 ->
    let (pat, acts) = p in
    if Pattern.is_empty pat
    then strip_empty_rules cf0
    else (pat, acts) :: (strip_empty_rules cf0)
  
  (** val opt : t -> t **)
  
  let opt tbl =
    elim_shadowed (strip_empty_rules tbl)
  
  (** val inter_entry :
      t -> (pattern * action) -> (Pattern.t * Action.t) list **)
  
  let inter_entry cl = function
  | (pat, act) ->
    List.fold_right (fun v' acc ->
      let (pat', act') = v' in
      ((Pattern.inter pat pat'), (Action.par_action act act')) :: acc) [] cl
  
  (** val inter_no_opt : t -> t -> (Pattern.t * Action.t) list **)
  
  let inter_no_opt cl1 cl2 =
    List.fold_right (fun v acc -> (inter_entry cl2 v) @ acc) [] cl1
  
  (** val union_no_opt : t -> t -> (Pattern.t * Action.t) list **)
  
  let union_no_opt cl1 cl2 =
    (inter_no_opt cl1 cl2) @ cl1 @ cl2
  
  (** val par_actions : action list -> Action.t **)
  
  let rec par_actions = function
  | [] -> Action.drop
  | act :: lst' -> Action.par_action act (par_actions lst')
  
  (** val seq : t -> t -> port -> packet -> Action.t **)
  
  let seq tbl1 tbl2 pt pk =
    Action.seq_action (scan tbl1 pt pk)
      (par_actions
        (List.map (fun ptpk -> let (pt0, pk0) = ptpk in scan tbl2 pt0 pk0)
          (Action.apply_action (scan tbl1 pt pk) (pt, pk))))
  
  (** val union : t -> t -> t **)
  
  let union tbl1 tbl2 =
    opt (union_no_opt tbl1 tbl2)
  
  (** val inter : t -> t -> t **)
  
  let inter tbl1 tbl2 =
    opt (inter_no_opt tbl1 tbl2)
  
  (** val unions :
      (pattern * action) list list -> (pattern * action) list **)
  
  let rec unions = function
  | [] -> []
  | tbl :: lst' -> union_no_opt tbl (unions lst')
  
  (** val coq_Pick :
      pattern -> action -> Action.e -> t -> (Pattern.t * Action.t) list **)
  
  let rec coq_Pick p1 a1 atom = function
  | [] -> []
  | p0 :: tbl' ->
    let (p, a) = p0 in
    ((Pattern.inter p1
       (Pattern.inter (Action.domain atom) (Action.restrict_range atom p))),
    (Action.seq_action a1 a)) :: (coq_Pick p1 a1 atom tbl')
  
  (** val sequence_no_opt : t -> t -> (pattern * Action.t) list **)
  
  let rec sequence_no_opt tbl1 tbl2 =
    match tbl1 with
    | [] -> []
    | p0 :: tbl1' ->
      let (p, a) = p0 in
      (match Action.atoms a with
       | [] -> (p, Action.drop) :: (sequence_no_opt tbl1' tbl2)
       | e0 :: l ->
         (unions (List.map (fun atom -> coq_Pick p a atom tbl2) (e0 :: l)))
         @ (sequence_no_opt tbl1' tbl2))
  
  (** val sequence : t -> t -> t **)
  
  let sequence tbl1 tbl2 =
    opt (sequence_no_opt tbl1 tbl2)
 end

