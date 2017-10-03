(** Abstract version of group table, first introduced in OpenFlow 1.1 and
  widely implemented in OpenFlow 1.3.  Generally there is one group table per
  switch.

  A group table combines (1) a list of actions into a bucket - the
  bucket may, for example, modify some packet headers and send it
  out a port. (2) a set of action buckets into a group.  Only one
  action bucket is executed, but the choice between buckets is made by
  "liveness" (as in a fast failover group) or round-robin.

  This version of a group table is mutable - you create a group table, then add groups
  or clear them out with operations.

  TODO: This should probably be merged with OpenFlow since it's an abstraction.  Also
  commit should produce a OpenFlow0x04 structure directly, like to_par, to_seq, etc. in OpenFlow
*)
open OpenFlow0x04

type t

(* SJS *)
(** String representation of group table. *)
val to_string : t -> string

(** Create a new group table *)
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
val commit : t -> OpenFlow0x04.Message.t list

(** Creates a new fast fail group which forwards out of the first live port in
 * [ports] and otherwise drops. *)
val add_fastfail_group : t -> portId list -> groupId

