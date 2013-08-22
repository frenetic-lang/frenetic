open OpenFlow0x04_Core
open Packet
open NetCore_Types

module Make : functor (Platform : OpenFlow0x04_Platform.PLATFORM) -> 
sig
  val start_controller : 
    (switchId * portId * bytes) Lwt_stream.t 
    -> pol NetCore_Stream.t 
    -> unit Lwt.t
end
