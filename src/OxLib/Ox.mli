(** An OpenFlow controller interface for directly sending and receiving
OpenFlow messages. *)

open OpenFlow0x01

(** The [OxPlatform] module contains functions for interacting with the Ox
controller and the underlying OpenFlow network. *)
module OxPlatform : sig

  (** [send_packet_out sw xid pkt] sends a [PacketOutMsg] message with
  transaction ID xid to switch [sw]. *)
  val send_packet_out : switchId -> xid -> PacketOut.t -> unit

  (** [send_flow_mod sw xid mod] sends a [FlowModMsg] message with transaction
  ID xid to switch [sw]. *)
  val send_flow_mod : switchId -> xid -> FlowMod.t -> unit

  (** [send_barrier_request sw xid] sends a barrier request to switch [sw] with
  transaction ID [xid]. *)
  val send_barrier_request : switchId -> xid -> unit

  (** [send_stats_request sw xid req] sends a [StatsRequestMsg] message with
  transaction ID [xid] to switch [sw]. *)
  val send_stats_request : switchId -> xid -> StatsRequest.t -> unit

  (** [timeout n thunk] will (asynchronously) sleep for [n] seconds and then
  invoke [thunk].  Unhandled exceptions raised by [thunk] will generate a
  warning message but are otherwise ignored. *)
  val timeout : float -> (unit -> unit) -> unit

end

(** Provides default implementations for some advanced event handlers,
    reducing clutter in simple controllers.

    These event handlers simply ignore the messages they receive. *)
module OxDefaults : sig
  val barrier_reply : switchId -> xid -> unit

  val stats_reply : switchId -> xid -> StatsReply.t -> unit

  val port_status : switchId -> xid -> PortStatus.t -> unit

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
  val packet_in : switchId -> xid -> PacketIn.t -> unit

  (** [barrier_reply sw xid] is a callback invoked when a barrier reply with
  transaction ID [xid] from switch [sw] arrives at the controller. *)
  val barrier_reply : switchId -> xid -> unit

  (** [stats_reply sw xid rep] is a callback invoked when switch [sw] responds
  with a reply [rep] to a statistics request with transaction ID [xid]. *)
  val stats_reply : switchId -> xid -> StatsReply.t -> unit

  (** [port_status sw xid stat] is a callback invoked when a port status
  message from switch [sw] with transaction ID [xid] arrives at the controller.
  *)
  val port_status : switchId -> xid -> PortStatus.t -> unit

end

(** Given an [OXMODULE] module, produce an Ox controller that, when run,
listens on port 6633 for messages from OpenFlow-enabled switches.  Messages and
network events are passed to the appropriate [OXMODULE] callbacks. *)
module Make : functor (OxModule:OXMODULE) -> sig

end
