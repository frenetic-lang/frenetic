Set Implicit Arguments.

Require Import Coq.Classes.Equivalence.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.List.
Require Import Common.CpdtTactics.
Require Import Common.Types.
Require Import Classifier.Defs.

Local Open Scope list_scope.
Local Open Scope equiv_scope.

Definition second {A B C: Type} (f : B -> C) (pair : (A * B)) := 
  match pair with
    | (a,b) => (a, (f b))
  end.
