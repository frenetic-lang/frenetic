open Packet
open NetCore_Action

module type CLASSIFIER = 
sig 
  type action

  type t = (NetCore_Pattern.t * action) list

  val scan : t -> NetCore_Pattern.port -> packet -> action

  val union : t -> t -> t

  val sequence : t -> t -> t

  val par_actions : action list -> action

  val choice : t -> t -> t

 end

module type MAKE  = functor (Action : ACTION) -> 
sig include CLASSIFIER end
  with type action = Action.t

module Make : MAKE
