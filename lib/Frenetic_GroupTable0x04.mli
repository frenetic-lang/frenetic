(** Controller's representation of the group table configuration on an
    OpenFlow 1.3 switch. *)
open Frenetic_OpenFlow0x04
type t with sexp

val create : unit -> t

(** Creates a new group, allocating a fresh ID. *)
val add_group :
     t
  -> groupType
  -> bucket list
  -> groupId

(** Forgets all groups *)
val clear_groups : t -> unit

(** Produces a list of messages to realize the previous [add_group] and
   [clear_groups] commands, upto the last [commit] command. *)

val commit : t -> Frenetic_OpenFlow0x04.Message.t list

(** Creates a new fast fail group which forwards out of the first live port in 
 * [ports] and otherwise drops. *)
val add_fastfail_group : t -> portId list -> groupId

