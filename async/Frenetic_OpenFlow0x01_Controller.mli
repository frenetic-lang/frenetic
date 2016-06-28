open Core.Std
open Async.Std
open Frenetic_OpenFlow

(* plugin function implementations *)
module OF10 = Frenetic_OpenFlow0x01

val start: int -> unit

val events : event Pipe.Reader.t

val switch_features : switchId -> switchFeatures option Deferred.t

val packet_out : switchId -> payload -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t

val flow_stats : switchId -> Frenetic_NetKAT.pred -> flowStats Deferred.t

val port_stats : switchId -> portId -> portStats Deferred.t

val update : Frenetic_NetKAT_Compiler.t -> unit Deferred.t

val update_switch : switchId -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t

(* general functions *)

val get_switches : unit -> switchId list Deferred.t
                                                 
val send : switchId -> OF10.xid -> OF10.Message.t -> [`Ok | `Eof] Deferred.t

val send_batch : switchId -> OF10.xid -> OF10.Message.t list -> [`Ok | `Eof] Deferred.t

val send_txn : switchId -> OF10.Message.t -> [`Ok of (OF10.Message.t list) | `Eof] Deferred.t

