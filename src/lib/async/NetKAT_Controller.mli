open Core
open Async

open Frenetic_kernel.OpenFlow

module type PLUGIN = sig
  val start: int -> unit
  val events : event Pipe.Reader.t
  val switch_features : switchId -> switchFeatures option Deferred.t
  val update : Frenetic_netkat.Local_compiler.t -> unit Deferred.t
  val update_switch : switchId -> Frenetic_netkat.Local_compiler.t -> unit Deferred.t
  val packet_out : switchId -> portId option -> payload -> Frenetic_netkat.Syntax.policy list -> unit Deferred.t
  val flow_stats : switchId -> Pattern.t -> flowStats Deferred.t
  val port_stats : switchId -> portId -> portStats Deferred.t
end

module type CONTROLLER = sig
  (** [start pt] initializes the controller, listening on TCP port [pt]. *)
  val start : int -> unit

  (** [event ()] returns the next event from the network. *)
  val event : unit -> event Deferred.t

  (** [current_switches ()] returns the set of switches currently
      connected to this controller. *)
  val switches : unit -> (switchId * portId list) list Deferred.t

  (** [port_stats sw pt] returns byte and packet counts for switch[sw] port [pt]. *)
  val port_stats : switchId -> portId -> portStats Deferred.t

  (** [update p] sets to local policy [p]. *)
  val update : Frenetic_netkat.Syntax.policy -> unit Deferred.t

  (** [update p] sets to global policy [p]. *)
  val update_global : Frenetic_netkat.Syntax.policy -> unit Deferred.t

  (** [update_fdd fdd] sets the global policy to the one encoded by the FDD. *)
  val update_fdd : Frenetic_netkat.Local_compiler.t -> unit Deferred.t

  (** [send_packet_out sw pd p] injects packets into the network by
      applying [p] to [pd] at [sw]. Optional ingress port helps locate buffer. *)
  val packet_out : switchId -> portId option -> payload -> Frenetic_netkat.Syntax.policy list -> unit Deferred.t

  (** [query x] returns byte and packet counts for query [x]. *)
  val query : string -> (int64 * int64) Deferred.t

  (** [set_current_compiler_options co] sets compiler options for subsequent invocations *)
  val set_current_compiler_options : Frenetic_netkat.Local_compiler.compiler_options -> unit

end

module Make(P:PLUGIN) : CONTROLLER


