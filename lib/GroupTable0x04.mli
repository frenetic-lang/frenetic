(** Controller's representation of the group table configuration on an
    OpenFlow 1.3 switch. *)

type t

val create : unit -> t

(** Creates a new group, allocating a fresh ID. *)
val add_group :
     t
  -> OpenFlow0x04_Core.groupType 
  -> OpenFlow0x04_Core.bucket list 
  -> OpenFlow0x04_Core.groupId

(** Forgets all groups *)
val clear_groups : t -> unit

(** Produces a list of messages to realize the previous [add_group] and
   [clear_groups] commands, upto the last [commit] command. *)
val commit : t -> OpenFlow0x04.Message.t list