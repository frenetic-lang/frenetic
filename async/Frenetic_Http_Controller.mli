open Frenetic_NetKAT
open Async.Std

val main : int -> int -> unit -> unit

type t 
val port_stats: t -> switchId -> portId -> Frenetic_OpenFlow0x01.portStats Deferred.t

val current_switches: t -> (switchId * portId list) list Deferred.t 

val query : t -> string -> (Int64.t * Int64.t) Deferred.t option

val event : t -> string -> string Deferred.t 

val pkt_out : t -> switchId -> Frenetic_OpenFlow.pktOut -> unit Deferred.t

val update : t -> string -> Frenetic_NetKAT.policy -> unit Deferred.t 

val start : int -> int -> unit -> unit
