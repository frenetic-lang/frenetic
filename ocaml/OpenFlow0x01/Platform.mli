(** An implementation of [Sig.PLATFORM]. The documentation for
    [PLATFORM] describes its features. *)

open Lwt_unix
open OpenFlow0x01Types

include Sig.PLATFORM

(** [init_with_port p] accepts connections from switches on port [p], which
    is usually [6633]. *)
val init_with_port : int -> unit

(** [init_with_port fd] accepts connections from switches on [fd]. *)
val init_with_fd : file_descr -> unit

(** [shutdown] gracefully shuts down the server *) 
val shutdown : unit -> unit 
