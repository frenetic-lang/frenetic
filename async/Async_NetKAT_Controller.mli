open Core.Std
open Async.Std

type t

(** [start pol ()] starts an OpenFlow 1.0 controller that is capable of
    implementing the policy application [pol] on the network.

    By default, topology discovery is disabled when the controller starts. *)
val start
  :  Async_NetKAT.Policy.t
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> ?policy_queue_size:int
  -> unit -> t Deferred.t


(** [enable_discovery t] enables detection of hosts on the network as well as
    links between switches. For host discovery, the controller will intercept a
    copy of every ARP packet. For link disocvery, the controller with synthesize
    packets and periodically send them through all the live ports of each
    switch, which will then be sent to the controller for analysis.

    Both these methods require modifying the policy that the controller will
    install on the network. *)
val enable_discovery  : t -> unit Deferred.t

(** [disable_discovery t] disables host and switch link discovery. *)
val disable_discovery : t -> unit Deferred.t

(** [enable_host_discovery t] enables detection of hosts on the network. The
    controller will intercept a copy of every ARP packet. *)
val enable_host_discovery : t -> unit Deferred.t

(** [disable_host_discovery t] disables host discovery. *)
val disable_host_discovery : t -> unit Deferred.t
