open Word
open Unix
open MessagesDef

type switchId = Word64.t

module type PLATFORM = sig

  exception SwitchDisconnected of switchId

  val send_to_switch : switchId -> xid -> message -> unit

  val recv_from_switch : switchId -> xid * message

  val accept_switch : unit -> features

end

module type FD = sig
  val fd : file_descr
end

module ActualPlatform : functor (Fd : FD) -> PLATFORM

module TestPlatform : sig
  include PLATFORM

  (** Blocks until the switch connects. *)
  val connect_switch : switchId -> unit

  (** Returns immediately and throws [SwitchDisconnected] the next time 
      the controller calls [send_to_switch sw_id] or [recv_from_switch sw_id].
  *)
  val disconnect_switch : switchId -> unit

  (** Blocks until the controller consumes the message. *)
  val send_to_controller : switchId -> xid -> message -> unit

  (** Blocks until the controller sends a message to this switch.*)
  val recv_from_controller : switchId -> xid * message

end    
