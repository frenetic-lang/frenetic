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
