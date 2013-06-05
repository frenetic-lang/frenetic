(** An implementation of [PLATFORM]. The documentation for [PLATFORM] describes its features. *)

include OpenFlow0x01_PlatformSig.PLATFORM

(** [init_with_port p] accepts connections from switches on port [p], which
    is usually [6633]. *)
val init_with_port : int -> unit Lwt.t

(** [init_with_port fd] accepts connections from switches on [fd]. *)
val init_with_fd : Lwt_unix.file_descr -> unit Lwt.t

(** [shutdown] shuts down the server gracefully *) 
val shutdown : unit -> unit 
