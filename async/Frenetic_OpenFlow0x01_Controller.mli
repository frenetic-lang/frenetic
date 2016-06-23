open Core.Std
open Async.Std
open Frenetic_OpenFlow

(* plugin functions *)
       
val start: int -> unit

val events : event Pipe.Reader.t

val switch_features : switchId -> switchFeatures option Deferred.t

val packet_out : switchId -> payload -> Frenetic_NetKAT_Compiler.t -> unit Deferred.t

val flow_stats : switchId -> Frenetic_NetKAT.pred -> flowStats Deferred.t

val port_stats : switchId -> portId -> portStats Deferred.t

(* general functions *)
                                                 
val send : switchId -> Frenetic_OpenFlow0x01.xid -> Frenetic_OpenFlow0x01.Message.t -> [`Ok | `Eof] Deferred.t

val send_batch : switchId -> Frenetic_OpenFlow0x01.xid -> Frenetic_OpenFlow0x01.Message.t list -> [`Ok | `Eof] Deferred.t

val send_txn : switchId -> Frenetic_OpenFlow0x01.Message.t -> [`Ok of (Frenetic_OpenFlow0x01.Message.t list) | `Eof] Deferred.t

