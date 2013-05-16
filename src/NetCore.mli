(** The NetCore policy language *)
open OpenFlow0x01.Types
open Packet.Types

(** [start_controller port policy] *)
val start_controller : int -> Syntax.policy Lwt_stream.t -> unit

(** The NetCore controller. *)
 module Make : functor (Platform : OpenFlow0x01.PLATFORM) -> sig
  val start_controller : Syntax.policy Lwt_stream.t -> unit Lwt.t
 end
