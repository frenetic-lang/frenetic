open Misc
open NetworkPacket
open OpenFlow0x01Types

module type ACTION = sig 
  type t 
  
  type e 
  
  val atoms : t -> e list
  
  val drop : t
  
  val pass : t
  
  val apply_atom : e -> (Pattern.port * packet) -> (Pattern.port * packet) option
  
  val apply_action : t -> (Pattern.port * packet) -> (Pattern.port * packet) list
  
  val par_action : t -> t -> t
  
  val seq_action : t -> t -> t
  
  val restrict_range : e -> Pattern.t -> Pattern.t
  
  val domain : e -> Pattern.t
 end

module type CLASSIFIER = sig 
  module Action : ACTION

  type t = (Pattern.t * Action.t) list
  
  val scan : t -> Pattern.port -> packet -> Action.t
  
  val inter : t -> t -> t
  
  val union : t -> t -> t
  
  val sequence : t -> t -> t
  
  val par_actions : Action.t list -> Action.t
 end

module type MAKE  = functor (Action : ACTION) -> 
  sig include CLASSIFIER end
  with module Action = Action

module Make : MAKE = 
 functor (Action:ACTION) ->
 struct 
  module Action = Action
    
  type action = Action.t
  
  type t = (Pattern.t * action) list
    
  let rec scan' default classifier pt pk = match classifier with
    | [] -> default
    | p :: rest ->
      let (pat, a) = p in
      if Pattern.match_packet pt pk pat then a else scan' default rest pt pk
  
  let scan =
    scan' Action.drop
  
  let rec elim_shadowed_helper prefix = function
  | [] -> prefix
  | p :: cf' ->
    let (pat, act) = p in
    if List.exists (fun entry ->
         let (pat', act0) = entry in pat = pat') prefix
    then elim_shadowed_helper prefix cf'
    else elim_shadowed_helper (prefix @ ((pat, act) :: [])) cf'
  
  let elim_shadowed cf =
    elim_shadowed_helper [] cf
  
  let rec strip_empty_rules = function
  | [] -> []
  | p :: cf0 ->
    let (pat, acts) = p in
    if Pattern.is_empty pat
    then strip_empty_rules cf0
    else (pat, acts) :: (strip_empty_rules cf0)
  
  let opt tbl =
    elim_shadowed (strip_empty_rules tbl)
  
  let inter_entry cl = function
  | (pat, act) ->
    List.fold_right (fun v' acc ->
      let (pat', act') = v' in
      ((Pattern.inter pat pat'), (Action.par_action act act')) :: acc) [] cl
  
  let inter_no_opt cl1 cl2 =
    List.fold_right (fun v acc -> (inter_entry cl2 v) @ acc) [] cl1
  
  let union_no_opt cl1 cl2 =
    (inter_no_opt cl1 cl2) @ cl1 @ cl2
  
  let rec par_actions = function
  | [] -> Action.drop
  | act :: lst' -> Action.par_action act (par_actions lst')
  
  let seq tbl1 tbl2 pt pk =
    Action.seq_action (scan tbl1 pt pk)
      (par_actions
        (List.map (fun ptpk -> let (pt0, pk0) = ptpk in scan tbl2 pt0 pk0)
          (Action.apply_action (scan tbl1 pt pk) (pt, pk))))
  
  let union tbl1 tbl2 =
    opt (union_no_opt tbl1 tbl2)
  
  let inter tbl1 tbl2 =
    opt (inter_no_opt tbl1 tbl2)
    
  let rec unions = function
  | [] -> []
  | tbl :: lst' -> union_no_opt tbl (unions lst')
  
  let rec coq_Pick p1 a1 atom = function
  | [] -> []
  | p0 :: tbl' ->
    let (p, a) = p0 in
    ((Pattern.inter p1
       (Pattern.inter (Action.domain atom) (Action.restrict_range atom p))),
    (Action.seq_action a1 a)) :: (coq_Pick p1 a1 atom tbl')
  
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
  
  let sequence tbl1 tbl2 =
    opt (sequence_no_opt tbl1 tbl2)
 end

