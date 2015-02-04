open Core.Std
open Async.Std

open Async_NetKAT

type t

(** [start pol ()] starts an OpenFlow 1.0 controller that is capable of
    implementing the policy application [pol] on the network.

    By default, topology discovery is disabled when the controller starts. *)
val start
  :  Policy.t
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> ?policy_queue_size:int
  -> unit -> t Deferred.t

(** [query pred t] will query the flows installed on the network that satisfy
    [pred] and return the sum of packet and byte counts across those flows.

    By default, the results will not include packet and byte counts for flows
    that match the predicate but have a _drop_ action. To include flows with
    drop actions in the results, use [query ~ingore_drops:false pred t]. *)
val query : ?ignore_drops:bool -> NetKAT_Types.pred -> t -> (Int64.t * Int64.t) Deferred.t

(** [nib t] return the in-memory representation of the controller's network
    information base. This is an immutable structure that will remain consistent
    with the controller's representation for as long as the application does not
    block. After that, topology events may have been received and integrated
    into the controller's NIB. *)
val nib : t -> Net.Topology.t

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
