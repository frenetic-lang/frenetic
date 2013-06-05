(** Launches an Ox application. *)
open OpenFlow0x01_Core
open OpenFlow0x01_Stats

(** Provides default implementations for some advanced event handlers,
    reducing clutter in simple controllers.

    These event handlers simply ignore the messages they receive. *)
module OxDefaults : sig

  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> reply -> unit

end

(** [OXMODULE] is the type of modules that implement Ox controller callback
    functions.  Modules of type [OXMODULE] are passed to the [Make] functor,
    producing an Ox controller. *)
module type OXMODULE = sig

  (** [switch_connected sw] is a callback invoked with [sw] when a switch with
  identifer [sw] connects to the controller. *)
  val switch_connected : switchId -> unit

  (** [switch_disconnected sw] is a callback invoked with [sw] when a switch
  with identifer [sw] disconnects from the controller. *)
  val switch_disconnected : switchId -> unit

  (** [packet_in sw xid pkt] is a callback invoked when a packet [pkt] with
  transaction ID [xid] from switch [sw] arrives at the controller. *)
  val packet_in : switchId -> xid -> packetIn -> unit

  (** [barrier_reply sw xid] is a callback invoked when a barrier reply with
  transaction ID [xid] from switch [sw] arrives at the controller. *)
  val barrier_reply : switchId -> xid -> unit

  (** [stats_reply sw xid rep] is a callback invoked when switch [sw] responds
  with a reply [rep] to a statistics request with transaction ID [xid]. *)
  val stats_reply : switchId -> xid -> reply -> unit

end

(** Given an [OXMODULE] module, produce an Ox controller that, when run,
listens on port 6633 for messages from OpenFlow-enabled switches.  Messages and
network events are passed to the appropriate [OXMODULE] callbacks. *)
module Make : functor (OxModule:OXMODULE) -> sig

end
