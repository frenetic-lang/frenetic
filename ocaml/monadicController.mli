open Platform
open ControllerInterface
open Packet
open MessagesDef

module type HANDLERS = sig

  val get_packet_handler : 
    NetCoreSemantics.coq_Id -> switchId -> portId -> packet -> unit

end

module EmptyHandlers : HANDLERS

(** * A NetCore controller that does not desugar. *)
module Make :
  functor (Platform : PLATFORM) -> 
    functor (Handlers : HANDLERS) -> sig
      val set_policy : NetCoreSemantics.coq_Pol -> unit
      val start_controller : NetCoreSemantics.coq_Pol -> unit
    end

