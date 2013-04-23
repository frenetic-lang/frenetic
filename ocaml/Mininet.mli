module Types : sig

type switchId = Int64.t
type hostAddr = Int64.t
type portId = int

type node =
  | Host of hostAddr
  | Switch of switchId

type edge_label = portId

val string_of_node : node -> string
val string_of_portId : portId -> string
val string_of_edge_label : edge_label -> string

val node_compare : node -> node -> int

end

open Types

(** Parses the output of Mininet's net command. *)      
val parse_from_chan : in_channel -> string -> (node * portId * node) list

(** A handle to a Mininet sub-process. *)
type mininet

val create_mininet_process : ?custom:string -> string -> mininet Lwt.t

val net : mininet -> (node * portId * node) list Lwt.t

val ping : mininet -> int -> hostAddr -> hostAddr -> bool Lwt.t

val ping_all : mininet -> hostAddr list -> bool Lwt.t

val broadcast_ping : mininet -> int -> hostAddr -> unit Lwt.t

val interact : mininet -> string -> string list Lwt.t

val dump_tables : mininet -> switchId -> unit Lwt.t

val enable_broadcast_ping : mininet -> hostAddr -> unit Lwt.t
