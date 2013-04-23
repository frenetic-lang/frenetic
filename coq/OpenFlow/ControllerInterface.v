Set Implicit Arguments.

Require Import Common.Types.
Require Import Common.Monad.
Require Import OpenFlow.OpenFlow0x01Types.
Require Import Network.NetworkPacket.

Inductive event : Type :=
  | SwitchConnected : switchId -> event
  | SwitchDisconnected : switchId -> event
  | SwitchMessage : switchId -> xid -> message -> event.

(** Using a thin trusted shim, written in Haskell, we drive verified
    controllers that match this signature. *)
Module Type CONTROLLER_MONAD <: MONAD.

  Include MONAD.
  Parameter state : Type.

  Parameter get : m state.
  Parameter put : state -> m unit.
  Parameter send : switchId -> xid -> message -> m unit.
  Parameter recv : m event.

  (** Must be defined in OCaml *)
  Parameter forever : m unit -> m unit.

End CONTROLLER_MONAD.

