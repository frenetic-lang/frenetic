open OpenFlow0x01

module type MAKE  = functor (Platform : PLATFORM) -> 
  sig
    val start_controller : NetCore_Types.External.policy Lwt_stream.t -> unit Lwt.t
  end

module Make : MAKE
