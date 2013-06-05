open OpenFlow0x01_Core
open OpenFlow0x01

(** Interface for all platforms. *)
module type PLATFORM = sig

  exception SwitchDisconnected of switchId
  val send_to_switch : switchId -> xid -> Message.t-> unit Lwt.t
  val recv_from_switch : switchId -> (xid * Message.t) Lwt.t
  val accept_switch : unit -> SwitchFeatures.t Lwt.t

end
