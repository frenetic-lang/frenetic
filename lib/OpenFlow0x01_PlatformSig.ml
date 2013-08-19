open OpenFlow0x01

module type PLATFORM = sig
  val send_to_switch : switchId -> xid -> Message.t -> unit Lwt.t
  val recv_from_switch : switchId -> (xid * Message.t) Lwt.t
  val accept_switch : unit -> SwitchFeatures.t Lwt.t
  val wait_disconnect : switchId -> unit Lwt.t
end
