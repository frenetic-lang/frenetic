open OpenFlow0x01
open Packet
open NetCore_Types

module Make : functor (Platform : PLATFORM) -> 
  sig
    val start_controller : 
      (switchId * portId * bytes) Lwt_stream.t 
      -> pol NetCore_Stream.t 
      -> unit Lwt.t
  end

module MakeConsistent : functor (Platform : PLATFORM) -> 
  sig
    val start_controller : 
      (switchId * portId * bytes) Lwt_stream.t 
      -> pol NetCore_Stream.t 
      -> unit Lwt.t
  end
