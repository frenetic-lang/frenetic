open Platform
open ControllerInterface

module type STATE = sig
  type state
end

module Make : 
  functor (Platform : PLATFORM) ->
    functor (State : STATE) -> sig
      include CONTROLLER_MONAD
      val run : State.state -> 'a m -> 'a
    end

module MakeNetCoreController :
  functor (Platform : PLATFORM) -> sig
    val set_policy : NetCore.coq_Pol -> unit
    val start_controller : NetCore.coq_Pol -> unit
  end

