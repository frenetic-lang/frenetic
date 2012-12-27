open Platform
open ControllerInterface
open Packet
open MessagesDef

module type STATE = sig
  type state
end

module type HANDLERS = sig

  val get_packet_handler : 
    NetCoreSemantics.coq_Id -> switchId -> portId -> packet -> unit

end

module EmptyHandlers : HANDLERS

module Make : 
  functor (Platform : PLATFORM) ->
    functor (State : STATE) ->
      functor (Handlers : HANDLERS) -> sig
        include CONTROLLER_MONAD
        val run : State.state -> 'a m -> 'a
      end

module MakeNetCoreController :
  functor (Platform : PLATFORM) -> 
    functor (Handlers : HANDLERS) -> sig
      val set_policy : NetCoreSemantics.coq_Pol -> unit
      val start_controller : NetCoreSemantics.coq_Pol -> unit
    end

