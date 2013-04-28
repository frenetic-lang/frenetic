Set Implicit Arguments.

Require Import Common.Types.
Require Import Coq.Lists.List.
Require Import Network.NetworkPacket.
Require Import Word.WordInterface.
Require Import Pattern.Pattern.

Local Open Scope list_scope.

Definition Classifier (A : Type) := list (pattern * A) %type.

Fixpoint scan {A : Type} (default : A) (classifier : Classifier A)  (pt : portId) 
  (pk : packet) := 
  match classifier with
    | nil => default
    | (pat,a) :: rest => 
      match Pattern.match_packet pt pk pat with
        | true => a
        | false => scan default rest pt pk
      end
  end. 

Definition inter_entry {A : Type} {B : Type} (f : A -> A -> B) 
  (cl : Classifier A) (v : pattern * A) :=
  let (pat, act) := v in
    fold_right (fun (v' : pattern * A) acc =>
      let (pat', act') := v' in
        (Pattern.inter pat pat', f act act') :: acc)
    nil cl.

Definition inter {A : Type} {B : Type} (f : A -> A -> B) (cl1 cl2 : Classifier A) :=
  fold_right (fun v acc => inter_entry f cl2 v ++ acc)
  nil cl1.

Definition union {A : Type} (f : A -> A -> A) (cl1 cl2 : Classifier A) :=
  inter f cl1 cl2 ++ cl1 ++ cl2.

(** Why so baroque? Filtering the tail of the list is not structurally
   recursive.
 *)
Fixpoint elim_shadowed_helper {A : Type} (prefix : Classifier A)
  (cf : Classifier A) :=
  match cf with
    | nil => prefix
    | (pat,act) :: cf' => 
      match existsb 
        (fun (entry : pattern * A) =>
          let (pat', act) := entry in
            if Pattern.beq pat pat' then true else false)
        prefix with
        | true => elim_shadowed_helper prefix cf'
        | false => elim_shadowed_helper (prefix ++ [(pat,act)]) cf'
      end
  end.

Definition elim_shadowed {A : Type} (cf : Classifier A) :=
  elim_shadowed_helper nil cf.

  

Fixpoint prioritize
  {A : Type} 
  (prio : nat) 
  (lst : Classifier A) : list (nat * pattern *A) :=
  match lst with
    | nil => nil
    | (pat, act) :: lst' => (prio, pat, act) :: (prioritize (pred prio) lst')
  end.
  
