open Core.Std
open Async.Std
open Frenetic_OpenFlow

(* plugin function implementations *)

val start: int -> unit

val events : event Pipe.Reader.t

val switch_features : switchId -> switchFeatures option Deferred.t

val packet_out : switchId -> portId option -> payload -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t

val flow_stats : switchId -> Pattern.t -> flowStats Deferred.t 

val port_stats : switchId -> portId -> portStats Deferred.t

val update : Frenetic_NetKAT_Compiler.t -> unit Deferred.t

val update_switch : switchId -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t

