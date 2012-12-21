Set Implicit Arguments.

Require Import OpenFlow.MessagesDef.

(*
Module Type CONTROLLER_MONAD <: MONAD.

  Include MONAD.
  Parameter send : switchId * xid * message -> m unit.
  Parameter recv : m (switchId * xid * message).

End CONTROLLER_MONAD.  
*)
(** Using a thin trusted shim, written in Haskell, we drive verified
    controllers that match this signature. *)
Module Type SINGLE_THREADED_CONTROLLER.

  (** Controller state that is threaded between calls to verified code.
      State threading is single-threaded, so we disallow multi-threaded
      controllers. *)
  Parameter st : Type.

  (** A command transforms the controller state and sends messages to
     switches. *)
  Definition command := st -> list (switchId * xid * message) * st.

  Parameter initial_state : st.

  Parameter new_switch :  switchId -> command.
  
  Parameter recv_message : switchId -> xid -> message -> command.

End SINGLE_THREADED_CONTROLLER.
