Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Common.Types.
Require Import Pattern.Pattern.
Require Import Network.NetworkPacket.
Require Import Classifier.ClassifierSignatures.

Module Make (Action_ : ACTION) <: CLASSIFIER.

  Module Action := Action_.

  Definition action := Action.t.

  Definition t := list (pattern * action).

  Fixpoint scan' (default : action) (classifier : t)  (pt : portId) (pk : packet) := 
  match classifier with
    | nil => default
    | (pat,a) :: rest => 
      match Pattern.match_packet pt pk pat with
        | true => a
        | false => scan' default rest pt pk
      end
  end. 

  Definition scan := scan' Action.drop.

  Definition inter_entry (cl : t) (v : pattern * action) :=
    let (pat, act) := v in
    fold_right 
      (fun (v' : pattern * action) acc =>
         let (pat', act') := v' in
         (Pattern.inter pat pat', Action.par_action act act') :: acc)
      nil cl.

  Definition inter (cl1 cl2 : t) :=
    fold_right (fun v acc => inter_entry cl2 v ++ acc)
               nil cl1.

  Definition union (cl1 cl2 : t) :=
    inter cl1 cl2 ++ cl1 ++ cl2.

  (** Why so baroque? Filtering the tail of the list is not structurally
      recursive.
   *)
  Fixpoint elim_shadowed_helper (prefix cf : t) :=
    match cf with
      | nil => prefix
      | (pat,act) :: cf' => 
        match existsb 
                (fun (entry : pattern * action) =>
                   let (pat', act) := entry in
                   if Pattern.beq pat pat' then true else false)
                prefix with
          | true => elim_shadowed_helper prefix cf'
          | false => elim_shadowed_helper (prefix ++ [(pat,act)]) cf'
        end
  end.

  Definition elim_shadowed (cf : t) := elim_shadowed_helper nil cf.

  Local Notation "x || y" := (Action.par_action x y).
  Local Notation "x ; y" := (Action.seq_action x y) 
                              (at level 51, right associativity).

  Fixpoint par_actions (lst : list action) :=
    match lst with
      | nil => Action.drop
      | act :: lst' => Action.par_action act (par_actions lst')
    end.


  (** Produces an action that can be applied to the original packet. 
      Therefore, we first sequence. *)
  Definition seq (tbl1 tbl2 : t) pt pk :=
    Action.seq_action
      (scan tbl1 pt pk)
      (par_actions 
         (map (fun (ptpk : portId * packet) => 
                 let (pt,pk) := ptpk in scan tbl2 pt pk) 
              (Action.apply_action (scan tbl1 pt pk) (pt,pk)))).

  Fixpoint unions (lst : list (list (pattern * action))) :=
    match lst with
      | nil => nil
      | tbl :: lst' => union tbl (unions lst')
    end.

  Fixpoint Pick (p1 : pattern) (a1 : action) (atom : Action.e) (tbl : t) :=
    match tbl with
      | nil => nil
      | ((p,a) :: tbl') => 
        (Pattern.inter p1 (Action.mask_pat atom p),
         a1; a) 
          :: (Pick p1 a1 atom tbl')
    end.

  Fixpoint sequence (tbl1 tbl2 : t) :=
      match tbl1 with 
        | nil => nil
        | (p,a) :: tbl1' =>
          match Action.atoms a with
            | nil => (p, Action.drop) :: sequence tbl1' tbl2
            | ats => unions (map (fun atom => Pick p a atom tbl2) ats) ++
                            sequence tbl1' tbl2
          end
      end.
  
End Make.
