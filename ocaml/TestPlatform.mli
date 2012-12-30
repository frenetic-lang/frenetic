open MessagesDef

include Platform.PLATFORM

val send_to_controller : switchId -> xid -> message -> unit Lwt.t

val recv_from_controller : switchId -> (xid * message) Lwt.t

val connect_switch : switchId -> unit Lwt.t
