Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import Network.NetworkPacket.
Require Import Word.WordInterface.

Import ListNotations.

Open Scope list_scope.

Record flowTableRule := Rule {
  priority : Word16.t;
  pattern : of_match;
  actions : actionSequence
}.

Definition flowTable := list flowTableRule.

