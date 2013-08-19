(** Manages a connection to a single OpenFlow 1.0 switch.

	 This module takes care of the OpeFlow handshake and keep-alive. *)

(* A handle to an OpenFlow switch. *)
type t

exception Disconnected of OpenFlow0x01_Core.switchId

val id : t -> OpenFlow0x01_Core.switchId
val features : t -> OpenFlow0x01.SwitchFeatures.t

(** Performs the OpenFlow 1.0 handshake and returns a handle to the switch.

	  [handshake] creates an LWT thread in the background that handles echo
	  requests and echo replies. *)
val handshake : Lwt_unix.file_descr -> t option Lwt.t

(** [send switch_id xid msg] sends [msg] to [switch_id],
    blocking until the send completes. *)
val send : t -> OpenFlow0x01_Core.xid 
  -> OpenFlow0x01.Message.t -> unit Lwt.t

(** [send_to_switch_fd fd xid msg] sends [msg] to the switch at [fd],
    blocking until the send completes. *)
val send_to_switch_fd : 
  Lwt_unix.file_descr -> 
  OpenFlow0x01_Core.xid ->
  OpenFlow0x01.Message.t -> 
  unit option Lwt.t

(** [recv_from_switch_fd fd] blocks until the switch at [fd] sends a
    message. *)
val recv_from_switch_fd : 
  Lwt_unix.file_descr -> 
  (OpenFlow0x01_Core.xid * OpenFlow0x01.Message.t) option Lwt.t

(** [recv switch_id] blocks until [switch_id] sends a
    message. *)
val recv : t
  -> (OpenFlow0x01_Core.xid * OpenFlow0x01.Message.t) Lwt.t

val disconnect : t -> unit Lwt.t
