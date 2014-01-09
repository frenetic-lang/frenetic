open Core.Std
open Async.Std

type t

val create : int -> t Deferred.t

val accept_switches : t -> SDN_Types.switchFeatures Pipe.Reader.t

val setup_flow_table : t -> SDN_Types.switchId -> SDN_Types.flowTable -> unit Deferred.t
