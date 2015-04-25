open Core.Std
open Async.Std
open OpenFlow0x01

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * OpenFlow_Header.t * Message.t 
]
val init: int -> unit

val get_switches : unit -> switchId list

val get_switch_features : switchId -> SwitchFeatures.t option

val events : event Pipe.Reader.t

val send : switchId -> xid -> Message.t -> [`Ok | `Eof]

val send_txn : switchId -> Message.t -> [`Ok of (Message.t list) Deferred.t | `Eof]
