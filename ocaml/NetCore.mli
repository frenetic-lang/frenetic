(** The NetCore policy language *)
open OpenFlow0x01Types
open Packet

module Make : functor (Platform : OpenFlow0x01.Sig.PLATFORM) -> sig
  val start_controller : NetCoreSyntax.policy Lwt_stream.t -> unit Lwt.t
end

