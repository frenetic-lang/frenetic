open Core.Std
open Async.Std
open Frenetic_NetKAT
open Cohttp_async
module Log = Frenetic_Log
module Net = Frenetic_NetKAT_Net.Net

module Discovery : sig
  type t = {
    nib : Net.Topology.t ref;
    policy : policy;
  }

  val start : event Pipe.Reader.t -> (policy -> unit Deferred.t) ->
    (switchId -> Frenetic_OpenFlow.pktOut -> unit Deferred.t) -> t

  val start_server : int -> (policy -> unit Deferred.t) -> unit

  val get_policy : t -> policy
end
