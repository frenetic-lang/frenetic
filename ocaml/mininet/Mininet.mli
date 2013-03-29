open MininetTypes

(** Parses the output of Mininet's net command. *)      
val parse_from_chan : in_channel -> string -> (node * portId * node) list

(** A handle to a Mininet sub-process. *)
type mininet

val create_mininet_process : ?custom:string -> string -> mininet Lwt.t

val net : mininet -> (node * portId * node) list Lwt.t

val ping_all : mininet -> bool Lwt.t

val broadcast_ping : mininet -> hostAddr -> unit Lwt.t

val interact : mininet -> string -> string Lwt.t
