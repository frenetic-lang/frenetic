open ControllerInterface
open OpenFlow0x01Types
open Packet
open Platform
open Printf
open NetCoreSyntax

module type POLICY = sig
  val policy : policy
  (* Necessary due to static compilation in FwOF. *)
  val switches : switchId list
end


module Make (Platform : PLATFORM) (Policy : POLICY) : sig

  type state

  val init_packet_out : unit -> state
  val init_flow_mod : unit -> state
  
  (* Returns after expected switches connect, but still processes messages
     in an asynchronous thread. *)
  val start : state -> unit Lwt.t

end
