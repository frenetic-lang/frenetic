(** An OpenFlow platform is responsible for accepting and managing
    connections to OpenFlow-enabled switches.  It relies on the
    [Message] module for parsing and marshaling messages.
*)
open Lwt_unix
open OpenFlow0x01

(** [init_with_port p] accepts connections from switches on port [p], which
    is usually [6633]. *)
val init_with_port : int -> unit Lwt.t

(** [init_with_port fd] accepts connections from switches on [fd]. *)
val init_with_fd : file_descr -> unit Lwt.t

(** [shutdown] shuts down the server gracefully *) 
val shutdown : unit -> unit 

(** [send_to_switch switch_id xid msg] sends [msg] to the switch,
    blocking until the send completes. *)
val send_to_switch : switchId -> xid -> Message.t-> unit Lwt.t

(** [recv_from_switch switch_id] blocks until [switch_id] sends a
    message.

    If the switch sends an [ECHO_REQUEST], [recv_from_switch] will
    itself respond with an [ECHO_REPLY] and block for the next
    message. *)
val recv_from_switch : switchId -> (xid * Message.t) Lwt.t

(** [accept_switch] blocks until a switch connects, handles the
    OpenFlow handshake, and returns after the switch sends a
    [FEATURES_REPLY] message. *)
val accept_switch : unit -> SwitchFeatures.t Lwt.t

(** [wait_disconnect switch_id] sleeps until the switch has disconnected. *)
val wait_disconnect : switchId -> unit Lwt.t
