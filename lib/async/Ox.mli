(** Launches an Ox application. *)
open Frenetic_kernel.OpenFlow0x01

(** Provides default implementations for some advanced event handlers,
    reducing clutter in simple controllers.

    These event handlers simply ignore the messages they receive. *)
module DefaultHandlers : sig

  val switch_connected : switchId -> SwitchFeatures.t -> unit
  val switch_disconnected : switchId -> unit
  val barrier_reply : switchId -> xid -> unit
  val stats_reply : switchId -> xid -> reply -> unit
  val cleanup : unit -> unit

end

(** [OXMODULE] is the type of modules that implement Ox controller callback
    functions.  Modules of type [OXMODULE] are passed to the [Make] functor,
    producing an Ox controller. *)
module type OXMODULE = sig

  (** [switch_connected sw] is a callback invoked with [sw] when a switch with
  identifer [sw] connects to the controller. *)
  val switch_connected : switchId -> SwitchFeatures.t -> unit

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

  (** [cleanup] is called when an exception stops the running of the main
  controller loop. *)
  val cleanup : unit -> unit

end

(** [Platform] defines functions for sending OpenFlow messages to
switches and wrapping thunks with timeouts. *)
module Platform : sig
  open Frenetic_kernel.OpenFlow0x01
  val send_packet_out : switchId -> xid -> packetOut -> unit
  val send_flow_mod : switchId -> xid -> flowMod -> unit
  val send_stats_request : switchId -> xid -> request -> unit
  val send_barrier_request : switchId -> xid -> unit
  val timeout : float -> (unit -> unit) -> unit
end

(** Given an [OXMODULE] module, build an Ox controller
    that, listens on port 6633 for messages from OpenFlow-enabled
    switches.  Messages and network events are passed to the
    appropriate [OXMODULE] callbacks. *)
module Make : functor (OxModule:OXMODULE) -> sig

   (** [start] is called to start the Ox controller. Note that execution
    will not be transfered back to the calling thread. *)
   val start : unit -> unit

end
