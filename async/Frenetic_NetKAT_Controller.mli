open Core.Std
open Async.Std

open Frenetic_NetKAT
(* module Log = Frenetic_Log *)

(* val bytes_to_headers : *)
(* 	Frenetic_OpenFlow.portId -> *)
(* 	Cstruct.t -> *)
(* 	Frenetic_NetKAT_Semantics.HeadersValues.t *)

(* val packet_sync_headers : *)
(*   Frenetic_NetKAT_Semantics.packet -> *)
(* 	Frenetic_NetKAT_Semantics.packet * bool *)

(* val of_to_netkat_event : *)
(*   Frenetic_NetKAT_Compiler.t -> *)
(*   Controller.event -> *)
(*   Frenetic_NetKAT.event list *)

module type CONTROLLER = sig
  open Frenetic_OpenFlow0x01
  val init: int -> unit
  val events : Frenetic_OpenFlow0x01_Controller.event Pipe.Reader.t
  val send : switchId -> xid -> Message.t -> [`Ok | `Eof] Deferred.t
  val send_batch : switchId -> xid -> Message.t list -> [`Ok | `Eof] Deferred.t
  val get_switches : unit -> switchId list Deferred.t
  val get_switch_features : switchId -> SwitchFeatures.t option Deferred.t
  val send_txn : switchId -> Message.t -> [`Ok of (Message.t list) Deferred.t | `Eof] Deferred.t
end

module type UPDATE = sig

end

module type S = sig
  (** [event ()] returns the next event from the network. *)
  val event : unit -> event Deferred.t

  (** [update_policy p] sets the global policy to [p]. *) 
  val update_policy : policy -> unit Deferred.t

  (** [send_packet_out sw pd p] injects packets into the network by
      applying [p] to [pd] at [sw]. *)
  val send_packet_out : switchId -> payload -> policy -> unit Deferred.t

  (** [query x] returns byte and packet counts for query [x]. *)
  val query : string -> (Int64.t * Int64.t) Deferred.t

  (** [is_query x] returns [true] iff [x] is a valid query in the
      installed policy. *)
  val is_query : string -> bool

  (** [port_stats sw pt] retrieves cumulative port statistics for port
      [pt] on switch [sw]. Note that using pseudo-port 0xf..f will
      return the statistics for all ports *)
  val port_stats : switchId -> portId -> Frenetic_OpenFlow0x01.portStats list Deferred.t

  (** [start pt] initializes the controller, listening on TCP port [pt]. *)
  val start : int -> unit
  
  (** [current_switches ()] returns the set of switches currently
      connected to this controller. *)
  val current_switches : unit -> (switchId * portId list) list Deferred.t

  (** This is a hack. *)
  val set_current_compiler_options : Frenetic_NetKAT_Compiler.compiler_options -> unit
end

module Make(C:CONTROLLER) (U:UPDATE) : S

module OF10 : S
