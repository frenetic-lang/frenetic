open Core.Std
open Async.Std
open OpenFlow0x01

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * Message.t
]

val init:
     ?max_pending_connections:int
  -> ?verbose:bool
  -> ?log_disconnects:bool
  -> ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ]
  -> ?monitor_connections:bool
  -> port:int
  -> unit Deferred.t

val close : switchId -> unit

val get_switches : unit -> switchId list

val get_switch_features : switchId -> SwitchFeatures.t option

val events : event Pipe.Reader.t

val send : switchId -> Message.t -> unit

val send_txn : switchId -> Message.t -> Message.t Deferred.t