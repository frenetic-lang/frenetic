Set Implicit Arguments.

Require Import Coq.Lists.List.

Import ListNotations.
Local Open Scope list_scope.

Module Type EXTRACTABLE_ATOMS.

  Parameter packet : Type.
  Parameter switchId : Type.
  Parameter portId : Type.
  Parameter flowTable : Type.
  Parameter flowMod : Type.

  Inductive fromController : Type :=
  | PacketOut : portId -> packet -> fromController
  | BarrierRequest : nat -> fromController
  | FlowMod : flowMod -> fromController.

  Inductive fromSwitch : Type :=
  | PacketIn : portId -> packet -> fromSwitch
  | BarrierReply : nat -> fromSwitch.

  Parameter abst_func : switchId -> portId -> packet -> list (portId * packet).

End EXTRACTABLE_ATOMS.

Module Type EXTRACTABLE_CONTROLLER.

  Declare Module Atoms : EXTRACTABLE_ATOMS.
  Import Atoms.

  Parameter controller : Type.
  Parameter send : controller -> option (controller * switchId * fromController).
  Parameter recv : controller -> switchId -> fromSwitch -> controller.

End EXTRACTABLE_CONTROLLER.
