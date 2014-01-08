open Core.Std
open Async.Std

val accept_switches : int -> (SDN_Types.switchFeatures Pipe.Reader.t) Deferred.t

val setup_flow_table : SDN_Types.switchId -> SDN_Types.flowTable -> unit Deferred.t
