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

(** [send_to_switch switch_id xid msg] sends [msg] to the switch,
    blocking until the send completes. *)
val send : t -> OpenFlow0x01_Core.xid 
  -> OpenFlow0x01.Message.t -> unit Lwt.t

(** [recv_from_switch switch_id] blocks until [switch_id] sends a
    message. *)
val recv : t
  -> (OpenFlow0x01_Core.xid * OpenFlow0x01.Message.t) Lwt.t

val disconnect : t -> unit Lwt.t