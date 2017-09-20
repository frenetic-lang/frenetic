open Core
open Async
open Frenetic.OpenFlow

(* plugin function implementations *)

val start: int -> unit

val events : event Pipe.Reader.t

val switch_features : switchId -> switchFeatures option Deferred.t

val packet_out : switchId -> portId option -> payload -> Netkat.Syntax.policy list -> unit Deferred.t

val flow_stats : switchId -> Pattern.t -> flowStats Deferred.t

val port_stats : switchId -> portId -> portStats Deferred.t

val update : Netkat.Compiler.t -> unit Deferred.t

val update_switch : switchId -> Netkat.Compiler.t -> unit Deferred.t

(* Low-level interface for Frenetic.Ox programming *)

type rpc_ack  = RpcOk | RpcEof

module LowLevel : sig
  module OF10 = Frenetic.OpenFlow0x01

  val start: int -> unit

  val send : OF10.switchId -> OF10.xid -> OF10.Message.t -> rpc_ack Deferred.t

  val events : event Pipe.Reader.t
end
