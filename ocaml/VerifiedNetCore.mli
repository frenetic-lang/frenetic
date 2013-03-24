open ControllerInterface
open MessagesDef
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
  val start : state -> unit Lwt.t

end
