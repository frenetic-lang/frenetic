open MininetTypes

(** Parses the output of Mininet's net command. *)      
val parse_from_chan : in_channel -> string -> (node * portId * node) list

(** A handle to a Mininet sub-process. *)
type mininet

val create_mininet_process : ?custom:string -> string -> mininet

val net : mininet -> (node * portId * node) list

val ping_all : mininet -> bool
