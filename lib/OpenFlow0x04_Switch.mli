(** Manages a connection to a single OpenFlow 1.3 switch. *)

(** * Low-level switch connection *)

(** [send_to_switch_fd fd xid msg] sends [msg] to the switch at [fd]. It
    returns [true] if the correct number of bytes were written. *)
val send_to_switch_fd : 
  Lwt_unix.file_descr -> 
  OpenFlow0x04_Core.xid ->
  OpenFlow0x04.Message.t -> 
  bool Lwt.t

(** [recv_from_switch_fd fd] blocks until the switch at [fd] sends a
    message. *)
val recv_from_switch_fd : 
  Lwt_unix.file_descr -> 
  (OpenFlow0x04_Core.xid * OpenFlow0x04.Message.t) option Lwt.t

(** * High-level switch connection *)

(* A handle to an OpenFlow switch that manages keep-alive. *)
type t

val id : t -> OpenFlow0x04_Core.switchId

val features : t -> OpenFlow0x04.SwitchFeatures.t

val ports : t -> OpenFlow0x04_Core.portDesc list

(** Performs the OpenFlow 1.3 handshake and returns a handle to the switch.

	  [handshake] creates an LWT thread in the background that handles echo
	  requests and echo replies. *)
val handshake : Lwt_unix.file_descr -> t option Lwt.t

(** [send switch_id xid msg] sends [msg] to [switch_id],
    blocking until the send completes. *)
val send : t 
  -> OpenFlow0x04_Core.xid 
  -> OpenFlow0x04.Message.t -> unit Lwt.t

(** [recv switch_id] blocks until [switch_id] sends a
    message. *)
val recv : t
  -> (OpenFlow0x04_Core.xid * OpenFlow0x04.Message.t) Lwt.t

val disconnect : t -> unit Lwt.t

val wait_disconnect : t -> unit Lwt.t
