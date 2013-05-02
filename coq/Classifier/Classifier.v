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

Section Sequencing.


  Variable A : Type.
  Variable Atom : Type.

  Variable zero : A.
  Variable par_action : A -> A -> A.
  Variable seq_action : A -> A -> A.
  Variable apply_atom : Atom -> portId * packet -> portId * packet.
  Variable mask_pat : Atom -> pattern -> pattern.
  Variable atoms : A -> list Atom.

  Definition apply_action (action : A) (ptpk : portId * packet) :=
    map (fun a => apply_atom a ptpk) (atoms action).

  Local Notation "x || y" := (par_action x y).
  Local Notation "x ; y" := (seq_action x y) (at level 51, right associativity).

  Fixpoint par_actions (lst : list A) :=
    match lst with
      | nil => zero
      | act :: lst' => par_action act (par_actions lst')
    end.

  Section Denotation.

    Definition scan' tbl ptpk := 
      match ptpk with
        | (pt, pk) => @scan A zero tbl pt pk
      end.

    Definition seq (tbl1 tbl2 : Classifier A) ptpk :=
      scan' tbl1 ptpk;
      par_actions (map (scan' tbl2) (apply_action (scan' tbl1 ptpk) ptpk)).

  End Denotation.

  Section Compiler.

    Fixpoint unions (lst : list (list (pattern * A))) :=
      match lst with
        | nil => nil
        | tbl :: lst' => union par_action tbl (unions lst')
      end.

    Fixpoint Pick (p1 : pattern) (a1 : A) (atom : Atom) (tbl : Classifier A) :=
      match tbl with
        | nil => nil
        | ((p,a) :: tbl') => 
           (Pattern.inter p1 (mask_pat atom p),
            a1; a) 
             :: (Pick p1 a1 atom tbl')
      end.

    Fixpoint sequence (tbl1 tbl2 : Classifier A) :=
      match tbl1 with 
        | nil => nil
        | (p,a) :: tbl1' =>
          match atoms a with
            | nil => (p, zero) :: sequence tbl1' tbl2
            | ats => unions (map (fun atom => Pick p a atom tbl2) ats) ++
                            sequence tbl1' tbl2
          end
      end.

  End Compiler.

End Sequencing.
