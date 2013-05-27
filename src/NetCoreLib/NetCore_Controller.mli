open OpenFlow0x01

module type MAKE  = functor (Platform : PLATFORM) -> 
  sig
    val start_controller : NetCore_Types.Internal.pol NetCore_Stream.t -> unit Lwt.t
  end

module Make : MAKE
