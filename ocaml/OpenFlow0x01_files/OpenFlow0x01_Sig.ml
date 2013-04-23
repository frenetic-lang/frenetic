open OpenFlow0x01Types
open Lwt_unix

(** Interface for all platforms. *)
module type PLATFORM = sig

  (** [SwitchDisconnected switch_id] is raised by [send_to_switch] and
      [recv_from_switch]. This exception is only raised once per switch.
      If the functions are applied to [switch_id] again, they raise 
      [Invalid_argument]. *)
  exception SwitchDisconnected of switchId 

  (** [send_to_switch switch_id xid msg] sends [msg] to the switch,
      blocking until the send completes. *)
  val send_to_switch : switchId -> xid -> message -> unit Lwt.t

  (** [recv_from_switch switch_id] blocks until [switch_id] sends a
      message. 

      If the switch sends an [ECHO_REQUEST], [recv_from_switch] will
      itself respond with an [ECHO_REPLY] and block for the next
      message. *)
  val recv_from_switch : switchId -> (xid * message) Lwt.t

  (** [accept_switch] blocks until a switch connects, handles the
      OpenFlow handshake, and returns after the switch sends a
      [FEATURES_REPLY] message. *)
  val accept_switch : unit -> features Lwt.t

end
