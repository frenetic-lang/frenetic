(** An implementation of [PLATFORM] for testing purposes.  The
    [Network] submodule enables basic network simulation. *)

open OpenFlow0x01
  
include OpenFlow0x01.PLATFORM

module Network : sig
    
    (* [connect_switch] simulates the connection of a switch with ID [switchId]
       to the controller, blocking until [accept_switch] accepts the 
       connection.  No messages are sent or received. *)
    val connect_switch : switchId -> unit Lwt.t

    (* [disconnect_switch] simulates a switch with ID [switchId] disconnecting
       from the controller. *)
    val disconnect_switch : switchId -> unit Lwt.t

    (* [send_to_controller] sends a message [message] to the controller with
       transaction ID [xid] from switch [switchId].  The switch should have 
       been connected with [connect_switch] first. *)
    val send_to_controller : switchId -> xid -> Message.t -> unit Lwt.t
  
    (* [recv_from_controller] blocks until switch [switchId] receives a message
       [message] with transaction ID [xid] from the controller.  The switch 
       should have been connected with [connect_switch] first. *)
    val recv_from_controller : switchId -> (xid * Message.t) Lwt.t
  
    (* [tear_down] disconnects any connected switches, discarding any pending
       messages. *)
    val tear_down : unit -> unit

end
