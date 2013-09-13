(** Transactional interface to an OpenFlow 1.0 switch.

  The transactional interface takes manages query IDs itself. Unlike the
  lower-level [OpenFlow0x01_Switch] module, it does not allow a program to
  manually set transaction IDs or even receive transaction IDs.

*)

type t

type msg = OpenFlow0x04.Message.t

(** Once [from_switch sw] is applied, it is not a good idea to call 
    [send] and [recv] directly on the lower-level switch. *)
val from_switch : OpenFlow0x04_Switch.t -> t

val transaction : t -> msg -> msg Lwt.t

val send : t -> msg -> unit Lwt.t

val recv_stream : t -> msg Lwt_stream.t
