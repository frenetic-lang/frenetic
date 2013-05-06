Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Common.Types.
Require Import Word.WordInterface.
Require Import Pattern2.PatternSignatures.
Require Import Network.NetworkPacket.

Import ListNotations.
Local Open Scope list_scope.
Local Open Scope bool_scope.

Module Make (PatternSpec_ : PATTERN_SPEC).

  Module PatternSpec := PatternSpec_.
  Import PatternSpec.
  Definition pattern := Pattern.t.
  Definition port := Pattern.port.

  Definition t := bool.
  Definition e := bool.

  Definition atoms (b : t) : list e := [b].

  Definition drop : t := false.

  Definition pass : t := true.

  Definition apply_atom (b : e) (ptpk : port * packet) :=
    match b with
      | true => Some ptpk
      | false => None
    end.
  Definition apply_action (action : t) (ptpk : port * packet) :=
    filter_map (fun a => apply_atom a ptpk) (atoms action).

  Definition par_action (b1 b2 : t) : t := b1 || b2.
  Definition seq_action (b1 b2 : t) : t := b1 && b2.

  Definition restrict_range (b : e) (p : pattern) := p.

  Definition domain (b : e) := Pattern.all.

End Make.
