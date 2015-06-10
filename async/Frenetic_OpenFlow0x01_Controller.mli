open Core.Std
open Async.Std
open Frenetic_OpenFlow0x01

type event = [
  | `Connect of switchId * SwitchFeatures.t
  | `Disconnect of switchId
  | `Message of switchId * Frenetic_OpenFlow_Header.t * Message.t 
]
val init: int -> unit

val get_switches : unit -> switchId list Deferred.t

val get_switch_features : switchId -> SwitchFeatures.t option Deferred.t

val events : event Pipe.Reader.t

val send : switchId -> xid -> Message.t -> [`Ok | `Eof] Deferred.t

val send_batch : switchId -> xid -> Message.t list -> [`Ok | `Eof] Deferred.t

val send_txn : switchId -> Message.t -> [`Ok of (Message.t list) Deferred.t | `Eof] Deferred.t
