(** The NetCore policy language *)
open OpenFlow0x01Types
open Packet
open Platform

module Make : functor (Platform : PLATFORM) -> sig
  val start_controller : NetCoreSyntax.policy Lwt_stream.t -> unit Lwt.t
end

